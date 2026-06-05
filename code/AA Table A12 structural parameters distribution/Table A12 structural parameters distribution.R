# Produces Table A.12: Fraction of Roma-background Individuals with a Null Probability of Roma Self-Reporting
# setwd(wd_data_structural)
# results_all<-readRDS("results_parallel.rds")
# results_all<-results_all %>%
#   mutate(Education=case_when(id==0 ~ "None",
#                              id==4 ~ "Primary",
#                              id %in% 8:10  ~ "Middle School",
#                              id %in% 12:13 ~ "High School",
#                              id %in% 14:16 ~ "Postsec")) %>%
#   mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
#   # mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary")))
#   # mutate(Education=case_when(id==0 ~ "None",
#   #                          id==4 ~ "GS",
#   #                          id %in% 8:10  ~ "MS",
#   #                          id %in% 12:13 ~ "HS",
#   #                          id %in% 14:16 ~ "Postsec")) %>%
#   # mutate(Education=factor(Education,levels=c("None","GS","MS","HS","Postsec"))) %>%
#   mutate(educ=as.numeric(as.character(id)))
#
# setwd(wd_data_structural)
# results_lognormal<-readRDS("results_parallel_lognormal.rds")
# results_lognormal<-results_lognormal %>%
#   mutate(Education=case_when(id==0 ~ "None",
#                              id==4 ~ "Primary",
#                              id %in% 8:10  ~ "Middle School",
#                              id %in% 12:13 ~ "High School",
#                              id %in% 14:16 ~ "Postsec")) %>%
#   mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
#   # mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary")))
#   # mutate(Education=case_when(id==0 ~ "None",
#   #                          id==4 ~ "GS",
#   #                          id %in% 8:10  ~ "MS",
#   #                          id %in% 12:13 ~ "HS",
#   #                          id %in% 14:16 ~ "Postsec")) %>%
#   # mutate(Education=factor(Education,levels=c("None","GS","MS","HS","Postsec"))) %>%
#   mutate(educ=as.numeric(as.character(id)))
#
# setwd(wd_data_structural)
# results_u<-readRDS("results_uniform.rds")
# results_u<-results_u %>%
#   mutate(Education=case_when(id==0 ~ "None",
#                              id==4 ~ "Primary",
#                              id %in% 8:10  ~ "Middle School",
#                              id %in% 12:13 ~ "High School",
#                              id %in% 14:16 ~ "Postsec")) %>%
#   mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
#   # mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary")))
#   # mutate(Education=case_when(id==0 ~ "None",
#   #                          id==4 ~ "GS",
#   #                          id %in% 8:10  ~ "MS",
#   #                          id %in% 12:13 ~ "HS",
#   #                          id %in% 14:16 ~ "Postsec")) %>%
#   # mutate(Education=factor(Education,levels=c("None","GS","MS","HS","Postsec"))) %>%
#   mutate(educ=as.numeric(as.character(id)))
#
#
# implied_share_0<-results_all %>%
#   mutate(prob_zero_92=pnorm(0, mean = d0, sd = sigma),
#          prob_zero_02=pnorm(0, mean = d1, sd = sigma),
#          prob_zero_11=pnorm(0, mean = d2, sd = sigma)) %>%
#   group_by(Education) %>%
#   summarise(sigma=mean(sigma),
#             prob_zero_92=mean(prob_zero_92),
#             prob_zero_02=mean(prob_zero_02),
#             prob_zero_11=mean(prob_zero_11)
#             )
#
# implied_share_0_log<-results_lognormal %>%
#   mutate(prob_zero_92=0,
#          prob_zero_02=0,
#          prob_zero_11=0) %>%
#   group_by(Education) %>%
#   summarise(sigma=mean(sigma),
#             prob_zero_92=mean(prob_zero_92),
#             prob_zero_02=mean(prob_zero_02),
#             prob_zero_11=mean(prob_zero_11)
#   )
#
# implied_share_0_u<-results_u %>%
#   mutate(prob_zero_92=-min(0,(d0-sigma/2))/sigma,
#          prob_zero_02=-min(0,(d1-sigma/2))/sigma,
#          prob_zero_11=-min(0,(d2-sigma/2))/sigma) %>%
#   group_by(Education) %>%
#   summarise(sigma=mean(sigma),
#             prob_zero_92=mean(prob_zero_92),
#             prob_zero_02=mean(prob_zero_02),
#             prob_zero_11=mean(prob_zero_11)
#   )
#
# print(xtable(implied_share_0), include.rownames = FALSE)
# print(xtable(implied_share_0_u), include.rownames = FALSE)
# print(xtable(implied_share_0_log), include.rownames = FALSE)
#
# setwd(wd_output)
# writeLines(print(xtable(implied_share_0), include.rownames = FALSE),"05_null_probability_normal.txt")
# writeLines(print(xtable(implied_share_0_u), include.rownames = FALSE),"05_null_probability_uniform.txt")
# writeLines(print(xtable(implied_share_0_log), include.rownames = FALSE),"05_null_probability_lognormal.txt")


# --- Load Structural Estimation Results ---

# results_all: bootstrap draws from the Normal cost-heterogeneity model.
# Each row is one bootstrap iteration x education group.
# id: years-of-schooling bin used to stratify the structural estimation.
# d0, d1, d2: estimated mean utility cost of Roma identification in 1992, 2002, 2011 respectively.
# sigma: estimated standard deviation of the cost distribution for that education group.
setwd(wd_data_structural)
results_all<-readRDS("results_parallel.rds")
# Map numeric schooling bins (id) to human-readable education labels consistent
# with the five groups used throughout the paper's structural analysis.
results_all<-results_all %>%
  mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School",
                             id %in% 14:16 ~ "Postsec")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
  # mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary")))
  # mutate(Education=case_when(id==0 ~ "None",
  #                          id==4 ~ "GS",
  #                          id %in% 8:10  ~ "MS",
  #                          id %in% 12:13 ~ "HS",
  #                          id %in% 14:16 ~ "Postsec")) %>%
  # mutate(Education=factor(Education,levels=c("None","GS","MS","HS","Postsec"))) %>%
  mutate(educ=as.numeric(as.character(id)))

# results_lognormal: bootstrap draws from the Lognormal cost-heterogeneity model.
# Under lognormal costs the support is strictly positive (cost > 0 always),
# so the share with null passing probability is identically zero for all groups.
setwd(wd_data_structural)
results_lognormal<-readRDS("results_parallel_lognormal.rds")
results_lognormal<-results_lognormal %>%
  mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School",
                             id %in% 14:16 ~ "Postsec")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
  # mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary")))
  # mutate(Education=case_when(id==0 ~ "None",
  #                          id==4 ~ "GS",
  #                          id %in% 8:10  ~ "MS",
  #                          id %in% 12:13 ~ "HS",
  #                          id %in% 14:16 ~ "Postsec")) %>%
  # mutate(Education=factor(Education,levels=c("None","GS","MS","HS","Postsec"))) %>%
  mutate(educ=as.numeric(as.character(id)))

# results_u: bootstrap draws from the Uniform cost-heterogeneity model.
# The uniform distribution is parameterised by mean d and half-width sigma/2,
# so support is [d - sigma/2, d + sigma/2].
setwd(wd_data_structural)
results_u<-readRDS("results_uniform.rds")
results_u<-results_u %>%
  mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School",
                             id %in% 14:16 ~ "Postsec")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
  # mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary")))
  # mutate(Education=case_when(id==0 ~ "None",
  #                          id==4 ~ "GS",
  #                          id %in% 8:10  ~ "MS",
  #                          id %in% 12:13 ~ "HS",
  #                          id %in% 14:16 ~ "Postsec")) %>%
  # mutate(Education=factor(Education,levels=c("None","GS","MS","HS","Postsec"))) %>%
  mutate(educ=as.numeric(as.character(id)))


# --- Compute Implied Share with Zero Passing Probability, by Model ---

# Under the Normal model, an individual has zero probability of Roma self-report
# (i.e. always passes) when their idiosyncratic cost draw c <= 0.
# Pr(c <= 0) = Phi(0; d_t, sigma) where d_t is the mean cost in census year t.
# Averaging over bootstrap draws gives the bootstrap-mean estimate per education group.
# implied_share_0: Normal model -- share for whom p(Roma self-report) = 0, by education and census year.
implied_share_0<-results_all %>%
  mutate(prob_zero_92=pnorm(0, mean = d0, sd = sigma),
         prob_zero_02=pnorm(0, mean = d1, sd = sigma),
         prob_zero_11=pnorm(0, mean = d2, sd = sigma)) %>%
  group_by(Education) %>%
  summarise(sigma=mean(sigma),
            prob_zero_92=mean(prob_zero_92),
            prob_zero_02=mean(prob_zero_02),
            prob_zero_11=mean(prob_zero_11)
  )

# implied_share_0_log: Lognormal model -- share with p=0 is always 0 because lognormal support
# is (0, infinity); no individual has a cost draw at or below zero under this distribution.
implied_share_0_log<-results_lognormal %>%
  mutate(prob_zero_92=0,
         prob_zero_02=0,
         prob_zero_11=0) %>%
  group_by(Education) %>%
  summarise(sigma=mean(sigma),
            prob_zero_92=mean(prob_zero_92),
            prob_zero_02=mean(prob_zero_02),
            prob_zero_11=mean(prob_zero_11)
  )

# implied_share_0_u: Uniform model -- share with p=0 is the mass of the distribution
# that lies at or below zero.  For Uniform(d - sigma/2, d + sigma/2), that mass is
# max(0, -( d - sigma/2 )) / sigma, i.e. the leftward overhang below zero divided by
# the total width.  The expression -min(0, d_t - sigma/2)/sigma implements this directly.
implied_share_0_u<-results_u %>%
  mutate(prob_zero_92=-min(0,(d0-sigma/2))/sigma,
         prob_zero_02=-min(0,(d1-sigma/2))/sigma,
         prob_zero_11=-min(0,(d2-sigma/2))/sigma) %>%
  group_by(Education) %>%
  summarise(sigma=mean(sigma),
            prob_zero_92=mean(prob_zero_92),
            prob_zero_02=mean(prob_zero_02),
            prob_zero_11=mean(prob_zero_11)
  )

# Diagnostic console prints to verify the three summary tables before writing output.
print(xtable(implied_share_0), include.rownames = FALSE)
print(xtable(implied_share_0_u), include.rownames = FALSE)
print(xtable(implied_share_0_log), include.rownames = FALSE)

setwd(wd_output)
# writeLines(print(xtable(implied_share_0), include.rownames = FALSE),"05_null_probability_normal.txt")
# writeLines(print(xtable(implied_share_0_u), include.rownames = FALSE),"05_null_probability_uniform.txt")
# writeLines(print(xtable(implied_share_0_log), include.rownames = FALSE),"05_null_probability_lognormal.txt")


#3-panel table: fraction with null probability (Table tab.zero)----
# Build the combined 3-panel LaTeX tabular exactly as it appears in the paper.
# Each panel reports, by education: sigma and the p=0 self-report fraction for
# 1992 / 2002 / 2011, under the Normal / Uniform / Lognormal model.
# edu_lab: maps internal factor level names to the display labels used in the paper.
edu_lab <- c("None"="None","Primary"="Primary","Middle School"="Middle School",
             "High School"="High School","Postsec"="Postsecondary")

# panel_rows(): formats one panel's data rows as LaTeX table lines.
# Returns a character vector, one element per education group, with & separators
# and \\ row-endings.  sigma and probability columns are formatted to 2 decimal places.
panel_rows <- function(df) {
  df <- df %>% arrange(Education)
  sprintf("%s & %.2f & %.2f & %.2f & %.2f \\\\",
          edu_lab[as.character(df$Education)],
          df$sigma, df$prob_zero_92, df$prob_zero_02, df$prob_zero_11)
}

# panel_block(): wraps one model's rows in the column headers and panel title
# that appear above each of the three panels in Table A.12.
panel_block <- function(title, df) c(
  "  \\hline",
  paste0(" &  \\multicolumn{4}{c}{", title, "}\\\\"),
  " & &  \\multicolumn{3}{c}{\\multirow{2}{*}{\\shortstack{$p=0$ Roma \\\\   self-report}}}\\\\ \\\\",
  "Education & $\\sigma$ & 1992 & 2002 & 2011\\\\ ",
  "   \\cline{3-5}",
  "  \\hline",
  panel_rows(df))

# Assemble the full three-panel tabular environment by stacking the three model blocks.
tab_zero <- c(
  "\\begin{tabular}{lcccc}",
  panel_block("Panel 1: Normal",    implied_share_0),
  panel_block("Panel 2: Uniform",   implied_share_0_u),
  panel_block("Panel 3: Lognormal", implied_share_0_log),
  "  \\hline",
  "\\end{tabular}")

# Output: Table A12.tex -- LaTeX tabular for the paper's appendix table on
# the fraction of Roma-background individuals with zero Roma self-report probability,
# reported separately for the Normal, Uniform, and Lognormal distributional assumptions.
setwd(wd_output)
writeLines(tab_zero, "Table A12.tex")

