# =====================================================================
# Prepares the estimation inputs for the structural Roma-identification model.
# Produces:  sourced by other scripts (defines `data`, `data_temp`/`data_sum`,
#            the model likelihood functions, fixed simulation draws S/S1/S2,
#            and the per-group estimation routine f())
# Inputs:    data_1992_2002_2011_unique_genderless.csv (cleaned linked census,
#            individuals matched across the 1992/2002/2011 rounds)
# Summary:   Builds each person's 3-round Roma-report history (ROMA_iii), computes
#            sex-mismatch (false-match) rates by education x history cell, fits a
#            latent-heterogeneity choice model where d0,d1,d2 are the mean
#            identification "costs" in 1992/2002/2011 and sigma the SD of the
#            individual heterogeneity, and defines f() to bootstrap-estimate the
#            model separately by education group.
# =====================================================================

# ---- Load linked census panel ----
#load data and get summary histories
#load data----
setwd(wd_data_linked)
data_raw<-as.data.frame(fread("data_1992_2002_2011_unique_genderless.csv"))

# ---- Recode rounds, education groups, and 3-round Roma-report histories ----
# Rename to round indices 0/1/2 (=1992/2002/2011), collapse education into groups,
# and encode each person's Roma report across the three rounds as a 3-digit string;
# mismatch_1/mismatch_2 flag sex disagreements indicating a likely false match.
data<-data_raw %>%
  rename(SEX_0=SEX_1992,
         SEX_1=SEX_2002,
         SEX_2=SEX_2011,
         ROMA_0=ROMA_1992,
         ROMA_1=ROMA_2002,
         ROMA_2=ROMA_2011,
         town_2=SIRSUP_2011,
         town_1=SIRSUP_2002,
         town_0=SIRSUP_1992,
         educ_2=years_2011) %>%
  dplyr::select(matches("SEX") | matches("educ") | matches("town") | matches("ROMA")) %>%
  group_by(town_2) %>%
  mutate(
    educ_2=case_when(educ_2 %in% 12:14 ~ 12,
                     TRUE ~ educ_2),
    id=paste0(educ_2)) %>%
  ungroup %>%
  mutate(ROMA_iii=case_when(
    ROMA_0==1 & ROMA_1==1 & ROMA_2==1 ~ '111',
    ROMA_0==1 & ROMA_1==1 & ROMA_2==0 ~ '110',
    ROMA_0==1 & ROMA_1==0 & ROMA_2==1 ~ '101',
    ROMA_0==1 & ROMA_1==0 & ROMA_2==0 ~ '100',
    ROMA_0==0 & ROMA_1==1 & ROMA_2==1 ~ '011',
    ROMA_0==0 & ROMA_1==1 & ROMA_2==0 ~ '010',
    ROMA_0==0 & ROMA_1==0 & ROMA_2==1 ~ '001',
    ROMA_0==0 & ROMA_1==0 & ROMA_2==0 ~ '000'),
    mismatch_1=(SEX_0!=SEX_2),
    mismatch_2=(SEX_1!=SEX_2)) %>%
  arrange(-educ_2)

# ---- Mismatch (false-match) probabilities per education x history cell ----
# m1/m2 are estimated false-match rates for the 1992 and 2002 links (x2 because a
# sex mismatch can only be detected half the time); m_none/m_only1/m_only2/m_both
# are the joint probabilities of the four false-match configurations.
#get mismatch rate for each group
data<-data %>%
  group_by(id,ROMA_iii) %>%
  mutate(m1=sum(mismatch_1==T)*2/length(mismatch_1),
         m2=sum(mismatch_2==T)*2/length(mismatch_2),
         m_none=(1-m1)*(1-m2),
         m_only1=m1*(1-m2),
         m_only2=(1-m1)*(m2),
         m_both=m1*m2
  )

# Collapse to one row per education x report-history cell, with the cell's
# empirical share p (target moments the model is fit to) and false-match weights.
data_sum<-data %>%
  group_by(id,ROMA_iii,ROMA_0,ROMA_1,ROMA_2) %>%
  summarise(m1=sum(mismatch_1==T)*2/length(mismatch_1),
            m2=sum(mismatch_2==T)*2/length(mismatch_2),
            m_none=(1-m1)*(1-m2),
            m_only1=m1*(1-m2),
            m_only2=(1-m1)*(m2),
            m_both=m1*m2,
            n=n()) %>%
  ungroup() %>%
  mutate(p=n/sum(n))

data_temp<-data_sum 



# ---- Load packages ----
#######

library(pacman)
pacman::p_load(tidyverse,data.table,fixest,
               xtable,modelsummary,haven,GA,dtplyr,kableExtra,
               doParallel,
               EnvStats,#for triangular distribution
               foreach)


############################################################
## 1. INVERSE CDFS
############################################################

# Inverse CDF (quantile function) of the triangular distribution on [a, b] with
# mode c; maps uniform draws U in [0,1] to triangular draws.
qtriangle <- function(U, a, c, b) {
  p <- (c - a) / (b - a)
  ifelse(
    U < p,
    a + sqrt(U * (b - a) * (c - a)),
    b - sqrt((1 - U) * (b - a) * (b - c))
  )
}

############################################################
## 2. BUILD LATENT DECISIONS (COMMON)
############################################################

# Turn individual heterogeneity draws (s for the true person; s1/s2 for the
# falsely-matched persons in rounds 1/2) plus the round means d0/d1/d2 into
# per-round identification probabilities, clipped to [0,1]. d0/d1/d2 are the true
# person's probabilities; d0m/d1m are the false-match counterparts.
build_data_sim <- function(s, s1, s2, d0_mean, d1_mean, d2_mean) {
  tibble(
    s  = s,
    s1 = s1,
    s2 = s2,
    d0  = pmin(pmax(s  + d0_mean, 0), 1),
    d1  = pmin(pmax(s  + d1_mean, 0), 1),
    d2  = pmin(pmax(s  + d2_mean, 0), 1),
    d0m = pmin(pmax(s1 + d0_mean, 0), 1),
    d1m = pmin(pmax(s2 + d1_mean, 0), 1)
  )
}

############################################################
## 3. PROBABILITY ENGINE (UNCHANGED LOGIC)
############################################################

# Given per-round identification probabilities and the latent Roma share pr,
# compute the model-predicted probability of each of the 8 report histories
# (p_000 ... p_111). Each history's probability mixes the four false-match cases
# (none / round-1 only / round-2 only / both) using the cell weights in data_temp,
# then averages over the simulated heterogeneity draws.
compute_moments <- function(data_sim, pr, data_temp) {

  data_sim %>%
    ## no mismatch  (correct linkage across all rounds)
    mutate(
      p_111_nm=pr*d0*d1*d2,
      p_101_nm=pr*d0*(1-d1)*d2,
      p_001_nm=pr*(1-d0)*(1-d1)*d2,
      p_011_nm=pr*(1-d0)*d1*d2,
      p_110_nm=pr*d0*d1*(1-d2),
      p_100_nm=pr*d0*(1-d1)*(1-d2),
      p_010_nm=pr*(1-d0)*d1*(1-d2),
      p_000_nm=pr*(1-d0)*(1-d1)*(1-d2)+(1-pr)
    ) %>%
    ## mismatch 2
    mutate(
      p_111_m2=pr*d0*pr*d1m*d2,
      p_101_m2=pr*d0*(pr*(1-d1m)+(1-pr))*d2,
      p_001_m2=pr*(1-d0)*(pr*(1-d1m)+(1-pr))*d2,
      p_011_m2=pr*(1-d0)*pr*d1m*d2,
      p_110_m2=pr*d0*pr*d1m*(1-d2),
      p_100_m2=pr*d0*(pr*(1-d1m)+(1-pr))*(1-d2),
      p_010_m2=pr*(1-d0)*pr*d1m*(1-d2)+(1-pr)*pr*d1m,
      p_000_m2=pr*(1-d0)*(pr*(1-d1m)+(1-pr))*(1-d2)+(1-pr)*(pr*(1-d1m)+(1-pr))
    ) %>%
    ## mismatch 1
    mutate(
      p_111_m1=pr*pr*d0m*d1*d2,
      p_101_m1=pr*pr*d0m*(1-d1)*d2,
      p_001_m1=pr*(pr*(1-d0m)+(1-pr))*(1-d1)*d2,
      p_011_m1=pr*(pr*(1-d0m)+(1-pr))*d1*d2,
      p_110_m1=pr*pr*d0m*d1*(1-d2),
      p_100_m1=pr*pr*d0m*(1-d1)*(1-d2)+(1-pr)*(pr*d0m),
      p_010_m1=pr*(pr*(1-d0m)+(1-pr))*d1*(1-d2),
      p_000_m1=pr*(pr*(1-d0m)+(1-pr))*(1-d1)*(1-d2)+(1-pr)*(pr*(1-d0m)+(1-pr))
    ) %>%
    ## mismatch both
    mutate(
      p_111_m=pr*pr*d0m*pr*d1m*d2,
      p_101_m=pr*pr*d0m*(pr*(1-d1m)+(1-pr))*d2,
      p_001_m=pr*(pr*(1-d0m)+(1-pr))*(pr*(1-d1m)+(1-pr))*d2,
      p_011_m=pr*(pr*(1-d0m)+(1-pr))*pr*d1m*d2,
      p_110_m=pr*pr*d0m*pr*d1m*(1-d2)+(1-pr)*pr*d0m*pr*d1m,
      p_100_m=pr*pr*d0m*(pr*(1-d1m)+(1-pr))*(1-d2)+(1-pr)*pr*d0m*(pr*(1-d1m)+(1-pr)),
      p_010_m=pr*(pr*(1-d0m)+(1-pr))*pr*d1m*(1-d2)+(1-pr)*(pr*(1-d0m)+(1-pr))*pr*d1m,
      p_000_m=pr*(pr*(1-d0m)+(1-pr))*(pr*(1-d1m)+(1-pr))*(1-d2)+(1-pr)*(pr*(1-d0m)+(1-pr))*(pr*(1-d1m)+(1-pr))
    ) %>%
    ## weights: blend the four false-match cases by their cell probabilities
    mutate(
      p_000=data_temp$m_none[data_temp$ROMA_iii=="000"]*p_000_nm+
        data_temp$m_only1[data_temp$ROMA_iii=="000"]*p_000_m1+
        data_temp$m_only2[data_temp$ROMA_iii=="000"]*p_000_m2+
        data_temp$m_both[data_temp$ROMA_iii=="000"]*p_000_m,
      p_001=data_temp$m_none[data_temp$ROMA_iii=="001"]*p_001_nm+
        data_temp$m_only1[data_temp$ROMA_iii=="001"]*p_001_m1+
        data_temp$m_only2[data_temp$ROMA_iii=="001"]*p_001_m2+
        data_temp$m_both[data_temp$ROMA_iii=="001"]*p_001_m,
      p_010=data_temp$m_none[data_temp$ROMA_iii=="010"]*p_010_nm+
        data_temp$m_only1[data_temp$ROMA_iii=="010"]*p_010_m1+
        data_temp$m_only2[data_temp$ROMA_iii=="010"]*p_010_m2+
        data_temp$m_both[data_temp$ROMA_iii=="010"]*p_010_m,
      p_011=data_temp$m_none[data_temp$ROMA_iii=="011"]*p_011_nm+
        data_temp$m_only1[data_temp$ROMA_iii=="011"]*p_011_m1+
        data_temp$m_only2[data_temp$ROMA_iii=="011"]*p_011_m2+
        data_temp$m_both[data_temp$ROMA_iii=="011"]*p_011_m,
      p_100=data_temp$m_none[data_temp$ROMA_iii=="100"]*p_100_nm+
        data_temp$m_only1[data_temp$ROMA_iii=="100"]*p_100_m1+
        data_temp$m_only2[data_temp$ROMA_iii=="100"]*p_100_m2+
        data_temp$m_both[data_temp$ROMA_iii=="100"]*p_100_m,
      p_101=data_temp$m_none[data_temp$ROMA_iii=="101"]*p_101_nm+
        data_temp$m_only1[data_temp$ROMA_iii=="101"]*p_101_m1+
        data_temp$m_only2[data_temp$ROMA_iii=="101"]*p_101_m2+
        data_temp$m_both[data_temp$ROMA_iii=="101"]*p_101_m,
      p_110=data_temp$m_none[data_temp$ROMA_iii=="110"]*p_110_nm+
        data_temp$m_only1[data_temp$ROMA_iii=="110"]*p_110_m1+
        data_temp$m_only2[data_temp$ROMA_iii=="110"]*p_110_m2+
        data_temp$m_both[data_temp$ROMA_iii=="110"]*p_110_m,
      p_111=data_temp$m_none[data_temp$ROMA_iii=="111"]*p_111_nm+
        data_temp$m_only1[data_temp$ROMA_iii=="111"]*p_111_m1+
        data_temp$m_only2[data_temp$ROMA_iii=="111"]*p_111_m2+
        data_temp$m_both[data_temp$ROMA_iii=="111"]*p_111_m
    ) %>%
    summarise(across(starts_with("p_"), mean, na.rm = TRUE))

}

############################################################
## 4. MODELS
############################################################

# NORMAL heterogeneity: individual cost shocks are mean-0 Normal with SD sigma.
# Parameters x = (pr, d0, d1, d2, sigma); returns minus the sum of squared
# distances between predicted and observed history shares (maximised by the optimiser).
model_normal <- function(x, data_temp, env, S, S1, S2) {
  pr=x[1]; d0=x[2]; d1=x[3]; d2=x[4]; sigma=x[5]
  # distribution enters here: Normal(0, sigma) draws via its quantile function
  s <- qnorm(S, 0, sigma)
  s1 <- qnorm(S1, 0, sigma)
  s2 <- qnorm(S2, 0, sigma)
  ds <- build_data_sim(s, s1, s2, d0, d1, d2)
  m <- compute_moments(ds, pr, data_temp)
  env$moments <- unlist(m %>% select(p_000:p_111))
  env$distances <- unlist(m %>% select(p_000:p_111)) - data_temp$p
  -sum(env$distances^2)
}

# UNIFORM heterogeneity: individual cost shocks are Uniform centred at 0 with
# width sigma. Parameters x = (pr, d0, d1, d2, sigma); same SSD objective.
model_uniform <- function(x, data_temp, env, S, S1, S2) {
  pr=x[1]; d0=x[2]; d1=x[3]; d2=x[4]; sigma=x[5]
  # distribution enters here: rescale uniform U to Uniform(-sigma/2, sigma/2)
  s <- sigma * (S - 0.5)
  s1 <- sigma * (S1 - 0.5)
  s2 <- sigma * (S2 - 0.5)
  ds <- build_data_sim(s, s1, s2, d0, d1, d2)
  m <- compute_moments(ds, pr, data_temp)
  env$moments <- unlist(m %>% select(p_000:p_111))
  env$distances <- unlist(m %>% select(p_000:p_111)) - data_temp$p
  -sum(env$distances^2)
}

# TRIANGLE heterogeneity: per-round cost shocks are Triangular on [0, 2*d] with
# mode d, so the spread is tied to the round mean (no separate sigma). Parameters
# x = (pr, d0, d1, d2); same SSD objective.
model_triangle <- function(x, data_temp, env, S, S1, S2) {
  pr=x[1]; d0=x[2]; d1=x[3]; d2=x[4]
  # distribution enters here: Triangular(0, mode = d, 2*d) draws per round
  s <- qtriangle(S, 0, 2*d0, d0)
  s1 <- qtriangle(S1, 0, 2*d1, d1)
  s2 <- qtriangle(S2, 0, 2*d2, d2)
  ds <- build_data_sim(s, s1, s2, d0, d1, d2)
  m <- compute_moments(ds, pr, data_temp)
  env$moments <- unlist(m %>% select(p_000:p_111))
  env$distances <- unlist(m %>% select(p_000:p_111)) - data_temp$p
  -sum(env$distances^2)
}

# LOGNORMAL heterogeneity: identification probabilities are exp(d_mean + s) with
# s a mean-0 Normal(SD sigma) shock, i.e. the cost is lognormal. Parameters
# x = (pr, d0, d1, d2, sigma); same SSD objective. Builds ds inline rather than
# via build_data_sim because of the exponential link.
model_lognormal <- function(x, data_temp, env, S, S1, S2) {
  pr_mean=x[1]; d0_mean=x[2]; d1_mean=x[3]; d2_mean=x[4]; sigma_mean=x[5]

  # distribution enters here: Normal(0, sigma) draws, exponentiated below
  s <- qnorm(S, 0, sigma_mean)
  s1 <- qnorm(S1, 0, sigma_mean)
  s2 <- qnorm(S2, 0, sigma_mean)

  ds <- tibble(
    d0=pmin(pmax(exp(d0_mean+s),0),1),
    d1=pmin(pmax(exp(d1_mean+s),0),1),
    d2=pmin(pmax(exp(d2_mean+s),0),1),
    d0m=pmin(pmax(exp(d0_mean+s1),0),1),
    d1m=pmin(pmax(exp(d1_mean+s2),0),1),
  )

  m <- compute_moments(ds, pr_mean, data_temp)
  env$moments <- unlist(m %>% select(p_000:p_111))
  env$distances <- unlist(m %>% select(p_000:p_111)) - data_temp$p
  -sum(env$distances^2)
}

# LOGNORMAL with serial correlation: like model_lognormal but the three rounds'
# shocks are correlated via rho (a Gaussian copula with correlation rho_mean).
# Parameters x = (pr, d0, d1, d2, sigma, rho). Not used in the main estimation runs.
model_lognormal_correlated <- function(x, data_temp, env, U, U1, U2) {
  pr_mean=x[1]; d0_mean=x[2]; d1_mean=x[3]; d2_mean=x[4]; sigma_mean=x[5]; rho_mean=x[6]
  Z1 <- qnorm(U[,1])
  Z2 <- rho_mean*Z1 + sqrt(1-rho_mean^2)*qnorm(U[,2])
  Z3 <- rho_mean*Z1 + sqrt(1-rho_mean^2)*qnorm(U[,3])
  Z2_m <- qnorm(U1[,1])
  Z3_m <- qnorm(U2[,1])

  ds <- tibble(
    d0=pmin(pmax(exp(d0_mean+sigma*Z1),0),1),
    d1=pmin(pmax(exp(d1_mean+sigma*Z2),0),1),
    d2=pmin(pmax(exp(d2_mean+sigma*Z3),0),1),
    d0m=pmin(pmax(exp(d0_mean+sigma*Z2_m),0),1),
    d1m=pmin(pmax(exp(d1_mean+sigma*Z3_m),0),1)
  )
  m <- compute_moments(ds, pr_mean, data_temp)
  env$moments <- unlist(m %>% select(p_000:p_111))
  env$distances <- unlist(m %>% select(p_000:p_111)) - data_temp$p
  -sum(env$distances^2)
}
############################################################


# ---- Fixed simulation draws (shared across all models and bootstrap iterations) ----
## --------------------------------------------------
## FIXED SIMULATION DRAWS
## --------------------------------------------------
# Seeded once so every model and bootstrap replicate uses the same 10,000 uniform
# draws (S/S1/S2), keeping the simulated likelihood comparable across runs.
set.seed(1)
n_sim <- 10000
S <- runif(n_sim)
S1 <- runif(n_sim)
S2 <- runif(n_sim)

# ---- Per-group bootstrap estimation routine ----
# Estimate the model for education group i on bootstrap replicate j under the
# chosen heterogeneity `type` ("normal"/"uniform"/"lognormal"/"triangle").
# Resamples towns with replacement, recomputes cell moments, picks group-specific
# starting values, optimises the SSD objective under constraints, and returns a
# one-row data frame of estimates, fitted moments, and moment distances.
f <- function(i, j, type, S, S1, S2) {
  setwd(wd_code)
  cat(paste("Starting iteration", i, "and", j, "\n"),
      file = "log.txt", append = TRUE)
  
  print(i)
  educ <- i
  
  ## --------------------------------------------------
  ## 1. bootstrap resampling 
  ## --------------------------------------------------
  
  # Seed by replicate j, then resample the group's 2002 towns (SIRUTA codes) with
  # replacement so inference clusters at the town level.
  set.seed(j)
  siruta_sample <- data.frame(
    town_1 = sample(
      unique(data$town_1[data$id == i]),
      length(unique(data$town_1[data$id == i])),
      replace = TRUE
    )
  )
  
  data_temp <- data %>%
    inner_join(siruta_sample, relationship = "many-to-many") %>%
    group_by(id, ROMA_iii, ROMA_0, ROMA_1, ROMA_2, educ_2) %>%
    summarise(
      m1 = sum(mismatch_1 == TRUE) * 2 / length(mismatch_1),
      m2 = sum(mismatch_2 == TRUE) * 2 / length(mismatch_2),
      m_none  = (1 - m1) * (1 - m2),
      m_only1 = m1 * (1 - m2),
      m_only2 = (1 - m1) * m2,
      m_both  = m1 * m2,
      n = n(),
      .groups = "drop"
    ) %>%
    group_by(id) %>%
    mutate(p = n / sum(n)) %>%
    filter(id == i)
  
 

  ## --------------------------------------------------
  ## 3. initial values
  ## --------------------------------------------------
  # Starting values are tuned by distribution and education group (lower latent
  # Roma share pr at higher education); order matches each model's parameter vector.
  if (type=="lognormal") {
    #initial values for lognormal
    theta_try<-case_when(educ==0 ~ c(0.2,-0.5,-0.5,-0.5,1),
                         educ==4 ~ c(0.09,-0.5,-0.5,-0.5,0.5),
                         educ==8 ~ c(0.05,-0.5,-0.5,-0.5,0.5),
                         educ %in% c(12,13,14) ~ c(0.02,-0.5,-0.5,-0.5,0.5),
                         educ==16 ~ c(0.03,-0.5,-0.5,-0.5,0.5))
  } else if (type=="triangle") {
    #initial values for triangle
    theta_try<-case_when(educ==0 ~ c(0.2,0.8,0.8,0.8),
                         educ==4 ~ c(0.09,0.7,0.7,0.7),
                         educ==8 ~ c(0.05,0.6,0.6,0.6),
                         educ %in% c(12,13,14) ~ c(0.02,0.6,0.45,0.45),
                         educ==16 ~ c(0.03,0.2,0.2,0.2))
  } else {
    theta_try<-case_when(educ==0 ~ c(0.2,0.8,0.8,0.8,0.2),
                         educ==4 ~ c(0.09,0.7,0.7,0.7,0.2),
                         educ==8 ~ c(0.05,0.6,0.6,0.6,0.2),
                         educ %in% c(12,13,14) ~ c(0.02,0.6,0.45,0.45,0.2),
                         educ==16 ~ c(0.03,0.2,0.2,0.2,0.2))
  }
  
  
  
 

  
 
  ## --------------------------------------------------
  ## 4. OPTIMIZATION
  ## --------------------------------------------------
  
  # env captures the fitted moments/distances as a side effect of the objective.
  env <- new.env()

  # Select the objective function matching the requested heterogeneity distribution.
  if (type=="lognormal"){
    model_try<-model_lognormal
  } else if (type=="uniform"){
    model_try<-model_uniform
  } else if (type=="normal"){
    model_try<-model_normal
  } else if (type=="triangle"){
    model_try<-model_triangle
  } 
  
  # Constrained maximisation of the (negative SSD) objective; ui/ci (defined in the
  # caller) enforce the parameter bounds. fnscale = -1 turns minimisation into maximisation.
  opt <- suppressMessages(
    constrOptim(
      theta_try,
      env = env,
      d = data_temp,
      ############################
      model_try,
      ###########################
      NULL,
      ui,
      ci,
      S = S,
      S1=S1,
      S2=S2,
      outer.eps = 1,
      outer.iterations = 10,
      control = list(
        fnscale = -1,
        trace = TRUE,
        reltol = 5e-5
      )
    )
  )
  
  
  ## --------------------------------------------------
  ## 5. STORE RESULTS
  ## --------------------------------------------------
  
  # One row per (education group, bootstrap replicate): parameter estimates,
  # observed sample size, observed 2011 Roma share, and the 8 moment distances and
  # fitted moments for diagnostics.
  results_temp <- data.frame(
    id = i,
    iter = j,
    pr = opt$par[1],
    d0 = opt$par[2],
    d1 = opt$par[3],
    d2 = opt$par[4],
    sigma = opt$par[5],
    n = sum(data_temp$n),
    pr_obs = sum(data_temp$n[data_temp$ROMA_2 == TRUE]) /
      sum(data_temp$n),
    dist_m1 = env$distances[1],
    dist_m2 = env$distances[2],
    dist_m3 = env$distances[3],
    dist_m4 = env$distances[4],
    dist_m5 = env$distances[5],
    dist_m6 = env$distances[6],
    dist_m7 = env$distances[7],
    dist_m8 = env$distances[8],
    m1 = env$moments[1],
    m2 = env$moments[2],
    m3 = env$moments[3],
    m4 = env$moments[4],
    m5 = env$moments[5],
    m6 = env$moments[6],
    m7 = env$moments[7],
    m8 = env$moments[8]
  )
  
  return(results_temp)
}

