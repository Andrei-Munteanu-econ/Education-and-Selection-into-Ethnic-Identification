#load data and get summary histories
#load data----
# Change working directory to the linked-data folder defined in the master script
setwd(wd_data_linked)
# Read the panel of individuals matched across all three censuses (1992, 2002, 2011)
# "genderless" cell_id matching avoids gender-ratio bias from the common-dwelling source
data_raw<-as.data.frame(fread("data_1992_2002_2011_unique_genderless.csv"))

# --- Data Preparation: Rename, Select, and Encode Identification Histories ---

# Rename census-wave-specific variables to a uniform t=0/1/2 convention
# (0=1992, 1=2002, 2=2011) so model code is wave-agnostic
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
  # Retain only the variables needed for the structural estimation
  dplyr::select(matches("SEX") | matches("educ") | matches("town") | matches("ROMA")) %>%
  group_by(town_2) %>%
  mutate(
    # Top-code years of schooling: all post-secondary degrees (12-14 years) are
    # collapsed to 12 to ensure sufficient cell sizes for estimation by education group
    educ_2=case_when(educ_2 %in% 12:14 ~ 12,
                     TRUE ~ educ_2),
    # id: the education group identifier used to split the sample for group-specific estimation
    id=paste0(educ_2)) %>%
  ungroup %>%
  mutate(
    # ROMA_iii: a three-digit binary string encoding the full identification history
    # across the three censuses (1992/2002/2011); e.g., "110" = Roma in 1992 and 2002
    # but non-Roma in 2011. Eight possible histories cover all 2^3 combinations.
    ROMA_iii=case_when(
    ROMA_0==1 & ROMA_1==1 & ROMA_2==1 ~ '111',
    ROMA_0==1 & ROMA_1==1 & ROMA_2==0 ~ '110',
    ROMA_0==1 & ROMA_1==0 & ROMA_2==1 ~ '101',
    ROMA_0==1 & ROMA_1==0 & ROMA_2==0 ~ '100',
    ROMA_0==0 & ROMA_1==1 & ROMA_2==1 ~ '011',
    ROMA_0==0 & ROMA_1==1 & ROMA_2==0 ~ '010',
    ROMA_0==0 & ROMA_1==0 & ROMA_2==1 ~ '001',
    ROMA_0==0 & ROMA_1==0 & ROMA_2==0 ~ '000'),
    # mismatch_1/2: sex inconsistency flags used as a proxy for false census matches.
    # A sex disagreement between wave 0 and wave 2 (or wave 1 and wave 2) signals
    # that two different individuals were linked; these drive the mismatch correction.
    mismatch_1=(SEX_0!=SEX_2),
    mismatch_2=(SEX_1!=SEX_2)) %>%
  # Sort descending by education so higher-education groups appear first
  arrange(-educ_2)

#get mismatch rate for each group
# Compute group-specific mismatch probabilities needed for the measurement-error correction.
# The factor of 2 converts the observed sex-mismatch rate into an estimated false-match rate,
# assuming sex mismatches are symmetric (half of mismatches are undetected because same-sex
# false matches are observationally identical to true matches).
data<-data %>%
  group_by(id,ROMA_iii) %>%
  mutate(m1=sum(mismatch_1==T)*2/length(mismatch_1),
         m2=sum(mismatch_2==T)*2/length(mismatch_2),
         # Joint mismatch probabilities under independence of the two linkage errors
         m_none=(1-m1)*(1-m2),
         m_only1=m1*(1-m2),
         m_only2=(1-m1)*(m2),
         m_both=m1*m2
  )

# Collapse to the sufficient statistics needed by the structural likelihood:
# for each (education group, identification history) cell, store the
# mismatch-correction weights and the empirical cell frequency p.
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
  # p: share of all individuals (across all histories) that fall in this cell;
  # these are the empirical moments the model is fit to match
  mutate(p=n/sum(n))

# data_temp: working copy of the full-sample summary used outside the bootstrap loop
data_temp<-data_sum



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

# Custom quantile function for the triangular distribution on [a, b] with mode c.
# Used to convert uniform random draws S into triangularly-distributed cost draws.
# The piecewise form follows from inverting the triangular CDF analytically.
qtriangle <- function(U, a, c, b) {
  # p: the CDF value at the mode; divides the distribution into two branches
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

# Construct the per-draw identification probability for each census wave.
# s, s1, s2: individual-level heterogeneity draws (cost shocks) for the main linkage
#            and the two mismatch-correction linkages respectively.
# d0_mean, d1_mean, d2_mean: mean identification probabilities for waves 0, 1, 2.
# The clamping to [0,1] ensures the sum of the latent utility exceeds the threshold
# with probability equal to the identification probability.
build_data_sim <- function(s, s1, s2, d0_mean, d1_mean, d2_mean) {
  tibble(
    # s, s1, s2: retained for potential downstream diagnostics
    s  = s,
    s1 = s1,
    s2 = s2,
    # d0/d1/d2: identification probability for correctly-matched records at t=0/1/2
    d0  = pmin(pmax(s  + d0_mean, 0), 1),
    d1  = pmin(pmax(s  + d1_mean, 0), 1),
    d2  = pmin(pmax(s  + d2_mean, 0), 1),
    # d0m/d1m: identification probability for the mismatched record at t=0 and t=1;
    # uses s1/s2 (independent draws) because a falsely-linked individual is unrelated
    # to the true individual's unobserved type
    d0m = pmin(pmax(s1 + d0_mean, 0), 1),
    d1m = pmin(pmax(s2 + d1_mean, 0), 1)
  )
}

############################################################
## 3. PROBABILITY ENGINE (UNCHANGED LOGIC)
############################################################

# Compute model-predicted probabilities for all 8 identification histories,
# integrating over the mismatch-correction structure.
# Arguments:
#   data_sim  : tibble from build_data_sim() with per-draw d0/d1/d2/d0m/d1m
#   pr        : share of population that is Roma (prior probability of Roma type)
#   data_temp : summary data frame with group-specific mismatch weights (m_none etc.)
compute_moments <- function(data_sim, pr, data_temp) {

  data_sim %>%
    ## no mismatch
    # Predicted cell shares assuming both census links (1992 and 2002) are correct.
    # Non-Roma individuals contribute only to p_000_nm via the (1-pr) term.
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
    # Predicted cell shares when the 2002 link is a false match (mismatch_2).
    # The falsely-matched 2002 record has its own independent identification probability d1m.
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
    # Predicted cell shares when the 1992 link is a false match (mismatch_1).
    # The falsely-matched 1992 record contributes d0m independently.
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
    # Predicted cell shares when BOTH the 1992 and the 2002 links are false matches.
    # Both mismatched records contribute independently via d0m and d1m.
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
    ## weights
    # Combine the four mismatch scenarios using the empirical mismatch weights from data_temp.
    # This implements the measurement-error correction: the observed cell frequency is a mixture
    # of true-match and false-match probabilities, weighted by the estimated mismatch rates.
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
    # Average across simulation draws to obtain the simulated predicted moments
    summarise(across(starts_with("p_"), mean, na.rm = TRUE))

}

############################################################
## 4. MODELS
############################################################

# --- Model: Normal cost heterogeneity ---
# Identification cost shocks are drawn from N(0, sigma).
# Parameters: pr (Roma share), d0/d1/d2 (mean identification probabilities per wave),
#             sigma (standard deviation of the cost distribution).
# Objective: minimise sum of squared deviations between predicted and empirical moments
#            (simulated method of moments). Returns negative SSE for use with constrOptim.
model_normal <- function(x, data_temp, env, S, S1, S2) {
  pr=x[1]; d0=x[2]; d1=x[3]; d2=x[4]; sigma=x[5]
  # Convert uniform draws S to Normal(0, sigma) cost shocks via the quantile function
  s <- qnorm(S, 0, sigma)
  s1 <- qnorm(S1, 0, sigma)
  s2 <- qnorm(S2, 0, sigma)
  ds <- build_data_sim(s, s1, s2, d0, d1, d2)
  m <- compute_moments(ds, pr, data_temp)
  env$moments <- unlist(m %>% select(p_000:p_111))
  env$distances <- unlist(m %>% select(p_000:p_111)) - data_temp$p
  -sum(env$distances^2)
}

# --- Model: Uniform cost heterogeneity ---
# Identification cost shocks drawn from Uniform(-sigma/2, sigma/2).
# sigma here controls the width (range) of the uniform distribution.
model_uniform <- function(x, data_temp, env, S, S1, S2) {
  pr=x[1]; d0=x[2]; d1=x[3]; d2=x[4]; sigma=x[5]
  # Shift and scale uniform draws from [0,1] to [-sigma/2, sigma/2]
  s <- sigma * (S - 0.5)
  s1 <- sigma * (S1 - 0.5)
  s2 <- sigma * (S2 - 0.5)
  ds <- build_data_sim(s, s1, s2, d0, d1, d2)
  m <- compute_moments(ds, pr, data_temp)
  env$moments <- unlist(m %>% select(p_000:p_111))
  env$distances <- unlist(m %>% select(p_000:p_111)) - data_temp$p
  -sum(env$distances^2)
}

# --- Model: Triangular cost heterogeneity ---
# Identification cost shocks drawn from a symmetric triangular distribution
# with support [0, 2*d_mean] and mode at d_mean (so the mean equals the mode).
# Note: the triangle model has 4 parameters, not 5, as the distribution shape
# is fully pinned by the mean identification probabilities (no separate sigma).
model_triangle <- function(x, data_temp, env, S, S1, S2) {
  pr=x[1]; d0=x[2]; d1=x[3]; d2=x[4]
  # The triangular distribution is symmetric around each d_mean, so the cost support
  # is [0, 2*d_mean] with mode at d_mean; qtriangle inverts this analytically
  s <- qtriangle(S, 0, 2*d0, d0)
  s1 <- qtriangle(S1, 0, 2*d1, d1)
  s2 <- qtriangle(S2, 0, 2*d2, d2)
  ds <- build_data_sim(s, s1, s2, d0, d1, d2)
  m <- compute_moments(ds, pr, data_temp)
  env$moments <- unlist(m %>% select(p_000:p_111))
  env$distances <- unlist(m %>% select(p_000:p_111)) - data_temp$p
  -sum(env$distances^2)
}

# --- Model: Lognormal cost heterogeneity ---
# Identification probabilities are lognormally distributed: d_t = exp(d_mean + sigma*Z)
# where Z ~ N(0,1). This ensures d_t > 0 and allows right-skewed cost distributions.
# sigma_mean is the standard deviation of the underlying normal (log-scale dispersion).
model_lognormal <- function(x, data_temp, env, S, S1, S2) {
  pr_mean=x[1]; d0_mean=x[2]; d1_mean=x[3]; d2_mean=x[4]; sigma_mean=x[5]

  # Convert uniform draws to N(0, sigma_mean) shocks for the lognormal transform
  s <- qnorm(S, 0, sigma_mean)
  s1 <- qnorm(S1, 0, sigma_mean)
  s2 <- qnorm(S2, 0, sigma_mean)

  # Build the simulated data directly here (rather than via build_data_sim) because
  # the lognormal model applies exp() rather than an additive shift
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

# --- Model: Lognormal with correlated cost shocks (currently unused in main results) ---
# Extends the lognormal model by allowing the three census-wave shocks to be correlated
# via a factor structure: Z2 and Z3 load on Z1 with coefficient rho.
# rho_mean: correlation parameter between cost shocks across census waves.
model_lognormal_correlated <- function(x, data_temp, env, U, U1, U2) {
  pr_mean=x[1]; d0_mean=x[2]; d1_mean=x[3]; d2_mean=x[4]; sigma_mean=x[5]; rho_mean=x[6]
  # Z1: the common factor; Z2/Z3 combine the common factor with idiosyncratic shocks
  Z1 <- qnorm(U[,1])
  Z2 <- rho_mean*Z1 + sqrt(1-rho_mean^2)*qnorm(U[,2])
  Z3 <- rho_mean*Z1 + sqrt(1-rho_mean^2)*qnorm(U[,3])
  # Z2_m/Z3_m: independent shocks for the mismatched records (no cross-wave correlation)
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


## --------------------------------------------------
## FIXED SIMULATION DRAWS
## --------------------------------------------------
# Fix the seed once so that the same quasi-random uniform draws are used in every
# call to the model functions. This guarantees that Monte Carlo noise does not affect
# comparisons across education groups or bootstrap iterations.
set.seed(1)
# n_sim: number of simulation draws for the simulated method of moments
n_sim <- 10000
# S, S1, S2: independent uniform draws for the true-match, mismatch-1, and mismatch-2
# cost draws; held constant throughout to eliminate simulation variance from the objective
S <- runif(n_sim)
S1 <- runif(n_sim)
S2 <- runif(n_sim)

# --- Bootstrap Worker Function ---
# f(i, j, type, S, S1, S2): estimate structural parameters for education group i
# and bootstrap iteration j under the distributional assumption in 'type'.
# Called in parallel via foreach; results are aggregated by the calling script.
f <- function(i, j, type, S, S1, S2) {
  setwd(wd_code)
  cat(paste("Starting iteration", i, "and", j, "\n"),
      file = "log.txt", append = TRUE)

  print(i)
  # educ: the education group (years of schooling) being estimated in this call
  educ <- i

  ## --------------------------------------------------
  ## 1. bootstrap resampling
  ## --------------------------------------------------

  # Reseed per bootstrap iteration so each j produces a different town draw
  # but the draw is replicable (same j always yields the same bootstrap sample)
  set.seed(j)
  # Town-level bootstrap: resample towns with replacement to account for within-town
  # correlation in identification behavior; individual records within sampled towns
  # are all retained (cluster bootstrap).
  siruta_sample <- data.frame(
    town_1 = sample(
      unique(data$town_1[data$id == i]),
      length(unique(data$town_1[data$id == i])),
      replace = TRUE
    )
  )

  # Rebuild the sufficient-statistics summary for this bootstrap sample and education group
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
  # Education-group-specific starting values for the optimizer.
  # Initial values are set manually based on prior knowledge of the gradient:
  # higher-education groups have lower Roma shares (pr) and lower identification
  # probabilities, so the starting point is shifted accordingly to aid convergence.
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
    # Shared starting values for normal and uniform models (both have 5 parameters)
    theta_try<-case_when(educ==0 ~ c(0.2,0.8,0.8,0.8,0.2),
                         educ==4 ~ c(0.09,0.7,0.7,0.7,0.2),
                         educ==8 ~ c(0.05,0.6,0.6,0.6,0.2),
                         educ %in% c(12,13,14) ~ c(0.02,0.6,0.45,0.45,0.2),
                         educ==16 ~ c(0.03,0.2,0.2,0.2,0.2))
  }







  ## --------------------------------------------------
  ## 4. OPTIMIZATION
  ## --------------------------------------------------

  # env: environment used as a mutable container so the model function can
  # write back the final moments and distances after the last evaluation
  env <- new.env()

  # Select the model function corresponding to the requested distributional assumption
  if (type=="lognormal"){
    model_try<-model_lognormal
  } else if (type=="uniform"){
    model_try<-model_uniform
  } else if (type=="normal"){
    model_try<-model_normal
  } else if (type=="triangle"){
    model_try<-model_triangle
  }

  # constrOptim: constrained optimisation using the barrier method.
  # ui/ci define linear inequality constraints (defined in the calling script)
  # to keep parameters in valid ranges (e.g., probabilities in [0,1]).
  # outer.eps and reltol are loosened slightly to balance precision and runtime
  # across 100 bootstrap iterations per education group.
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

  # Collect parameter estimates, fit statistics, and diagnostics into a single row.
  # pr_obs: observed Roma share in the bootstrap sample (used to assess model fit).
  # dist_m1..m8: moment-by-moment residuals (predicted - empirical) for the 8 histories.
  # m1..m8: model-predicted moments at the optimum (ordered as 000, 001, ..., 111).
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

