# Produces Table A.5: Changes in Ethnic Identification by Education: Instrumental Variables Approach (no fixed effects)

# --- Load and Prepare Linked 1992-2011 Roma Panel ---

#load data
setwd(wd_data_linked)
# data_1992_2011_r: the main analysis dataset linking 1992 and 2011 census records
data_1992_2011_r<-fread('data_1992_2011_roma_unique.csv')
# Construct language-based Roma flags as an alternative ethnic definition:
# ROMA_1992_LIM: individual spoke Romani (LIM_1992==12) in the 1992 census
# ROMA_2011_LIM: individual spoke Romani (LIM_2011==1201) in the 2011 census
data_1992_2011_r<-data_1992_2011_r %>%
  mutate(ROMA_1992_LIM=LIM_1992==12,
         ROMA_2011_LIM=LIM_2011==1201)

# --- Load 2011 Census Data (Vital Statistics / Births Sample) ---

setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
# read_sample() loads a representative sample for initial column selection
data_2011<-read_sample(filename)
# Keep only the variables needed for intergenerational analysis (child-to-mother linkage)
data_2011<-data_2011 %>%
  select(id11,id11_MOM,id11_MOM_BC,nat,LIM,AA,years,SIRUTA,SIRSUP,ROMA,ROMA_MOM,HHID,scoala_m,years_MOM,years_POP,SEX,category,source)
# read_data() reads the full file, restricting columns to the selection above
data_2011<-read_data(filename,data_2011) %>%
  # ROMA_bc: ethnicity based on nativity/background code (nat==12) — birth-certificate Roma definition
  mutate(ROMA_bc=nat==12) %>%
  # ROMA_lim: language-based Roma flag using the 2011 language code
  mutate(ROMA_lim=LIM==1201)
# Restrict to children born 2002-2011 to identify mothers present in the 2011 census
data_2011_kids<-data_2011 %>%
  filter(AA %in% 2002:2011)

# --- Build Intergenerational Dataset: Match Children to Their Linked Mothers ---

# Join children (born 2002-2011) in the 2011 census to their mothers' records
# in the 1992-2011 linked panel, using id11_MOM_BC (mother's birth-certificate ID)
# This allows IV estimation where the mother's 1992 schooling instruments for her 2011 schooling,
# and the outcome is the child's ethnic identification at birth (ROMA_bc) or in the 2011 census (ROMA)
data_1992_2011_mom<-data_2011_kids %>%
  inner_join(data_1992_2011_r %>%
               select(id11,years_1992,years_2011,
                      ROMA_1992,ROMA_2011,
                      ROMA_1992_LIM,ROMA_2011_LIM,
                      AA_2011,SEX_2011,
                      years_MOM_2011,years_POP_2011,HHID_1992,HHID_2011,years_MOM_1992,years_POP_1992,
                      SIRSUP_2011,SIRSUP_1992,AA_1992,AA_2011,
                      ET_SPOUSE_2011,
                      category_1992,source_1992,category_2011,source_2011),
             by=c("id11_MOM_BC"="id11"),
             suffix=c("_CHILD","_MOM"))



# --- OLS and IV Regressions: Main Linked Sample (1992-2011 Panel) ---

#baseline mom-11 mom
# OLS benchmark: effect of 2011 schooling on 2011 Roma self-identification
# Restricted to records with non-missing years_1992 (comparable sample to the IV below)
# Clustered at the 1992 locality (SIRSUP_1992) to account for within-community correlation
model_ols_baseline_11_92<-feols(data=data_1992_2011_r %>% filter(!is.na(years_1992)),
                                ROMA_2011~years_2011,
                                cluster=~SIRSUP_1992)
summary(model_ols_baseline_11_92)
#baseline mom-11 mom
# IV baseline: years_1992 instruments for years_2011; outcome is 2011 Roma self-identification
# Instrumenting with 1992 schooling purges endogenous changes in education between censuses
model_iv_baseline_11_92<-feols(data=data_1992_2011_r,
                               ROMA_2011~1|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92)
#baseline mom-bc
# IV: effect of mother's 2011 schooling (instrumented by 1992 schooling) on child's birth-certificate Roma status
# Sample: mothers identified as Roma in 1992 (ROMA_1992==T), intergenerational spillover test
model_iv_baseline_bc_92<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                               ROMA_bc~1|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_bc_92)



# --- OLS Benchmarks: Vital Statistics / 2011 Census Children ---

#bc-11 mom
# OLS: among children with Roma birth-certificate status, effect of mother's 2011 schooling
# on mother's own 2011 census Roma identification (cross-sectional, endogenous)
model_ols_bc_11<-feols(data=data_1992_2011_mom %>% filter(ROMA_bc==T),
                               ROMA_2011~years_2011,
                               cluster=~SIRSUP_2011)
summary(model_ols_bc_11)

#bc-11 mom
# OLS: among birth-certificate Roma in the full 2011 sample, effect of mother's schooling
# on whether the mother herself identifies as Roma in the 2011 census
# Uses full 2011 census data (not the linked panel); SIRSUP is the 2011 locality code
model_ols_bc_11_MOM<-feols(data=data_2011 %>% filter(ROMA_bc==T),
                      ROMA_MOM~years_MOM,
                      cluster=~SIRSUP)
summary(model_ols_bc_11_MOM)

# --- IV Regressions: Intergenerational Outcomes (Child Roma Status) ---

#baseline mom-bc
# IV: effect of mother's 2011 schooling (instrumented by 1992) on child's birth-certificate Roma status
# Sample: mothers Roma in 1992 — tests whether mother's education affects Roma identification at birth
model_iv_baseline_11_92_child<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA_bc~1|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child)

#baseline mom-2011 child
# IV: same instrument and sample as above, but outcome is child's 2011 census Roma identification
# Compares birth-certificate (administrative) vs. self-reported (census) child ethnicity
model_iv_baseline_11_92_child2<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA~1|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child2)


# --- Robustness: IV for Mothers' Own 2011 Passing (Linked Panel, Roma-in-1992 Sample) ---

#robustness check
#moms from VSN: 92-11 passing
#ols
# IV: mother's 1992 schooling instruments for 2011 schooling, outcome = mother's 2011 Roma identification
# Sample: mothers Roma in 1992 drawn from the intergenerational dataset
# Robustness check: replicates the main IV in the VSN-linked subsample
model_iv_baseline_11_92_momz_iv<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA_2011~1|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_momz_iv)

# #iv
# model_iv_baseline_11_92_momz<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
#                                     ROMA_2011~years_2011|SIRSUP_1992^AA_1992,
#                                     cluster=~SIRSUP_1992)
# summary(model_iv_baseline_11_92_momz)


# --- Table Formatting Setup ---

######
# Coefficient label map: standardises "Schooling Yrs" display across all model variants
variables<-c('years_2011'='Schooling Yrs',
             'years_MOM'='Schooling Yrs',
             'years_1992'='Schooling Yrs',
             'years_2002'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_MOM_1992'="Schooling Yrs Mom (Baseline)",
             'years_POP_1992'="Schooling Yrs Dad (Baseline)",
             "fit_years_MOM_CHILD"='Schooling Yrs (Endline)')
# Formatter for large numbers with comma thousands separator
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
options(modelsummary_format_numeric_latex = "plain")
# Custom glance method: appends "Mean of DV" as an additional GOF row in modelsummary output
# Reconstructs the DV as fitted values + residuals to recover the in-sample mean
glance_custom.fixest <- function(x, ...) {
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# Extract first-stage F-statistics for IV models to include as a table row
# OLS columns receive an empty string; IV columns receive the Cragg-Donald/cluster-robust F
f<-data.frame(n="F-stat",

              ols="",
              # ols2="",
              b=fitstat(model_iv_baseline_11_92, "ivf")$ivf1$stat,
              ols="",
              # c=fitstat(model_iv_baseline_bc_92, "ivf")$ivf1$stat,
              d=fitstat(model_iv_baseline_11_92_child, "ivf")$ivf1$stat,
              e=fitstat(model_iv_baseline_11_92_child2, "ivf")$ivf1$stat
)
f<-f_big(f)


# --- Produce and Post-Process LaTeX Table ---

#IV table
setwd(wd_output)
# Columns: OLS and IV for the main linked panel, OLS for the VSN mother sample,
# and two IV specifications for intergenerational child outcomes
tab <- modelsummary(list(
  "OLS '92-'11"   = model_ols_baseline_11_92,
  "IV '92-'11"    = model_iv_baseline_11_92,
  "OLS 'VSN-'11"  = model_ols_bc_11_MOM,
  "IV '92-'11" = model_iv_baseline_11_92_child,
  "IV '92-'11" = model_iv_baseline_11_92_child2
),
estimate="{estimate}{stars}",
statistic = "std.error",
stars=c('$^{*}$'=0.1,'$^{**}$'=0.05,'$^{***}$'=0.01),
gof_map=list(list("raw" = "nobs", "clean" = "N", "fmt" = f_big),
             list("raw" = "r.squared", "clean" = "R$^2$",fmt="%.2f"),
             list("raw"="Mean of DV","clean"="DV Mean",fmt="%.2f")),
metrics="R2",
add_rows = f,
#gof_omit = '^(?!.*R2)',
#coef_omit='polynn',
# output="Table 01.tex",
output="latex_tabular",
coef_rename=variables,
# theme = "tabular",
align = "lccccc",
escape=F)

# Split the raw LaTeX string into lines for surgical insertion of sample/outcome header rows
lines <- strsplit(tab, "\n")[[1]]

# The Sample/Outcome block: each cell stacks 2-3 lines via \makecell
sample_outcome <- c(
  "\\midrule",
  "Sample & '92-'11 & '92-'11 & VSN-'11 & '92-VSN-'11 & '92-VSN-'11 \\\\",
  "Outcome & \\makecell{Individual\\\\Census '11} & \\makecell{Individual\\\\Census '11} & \\makecell{Mother\\\\Census '11} & \\makecell{Child\\\\VSN} & \\makecell{Child\\\\Census '11} \\\\"
)

# insert right before the \midrule that precedes N (the GOF block)
# find the midrule that comes just before the "N &" row
n_row   <- grep("^N &", lines)[1]
mid_idx <- max(grep("midrule", lines[seq_len(n_row)]))   # last midrule before N

lines <- append(lines, sample_outcome, after = mid_idx - 1)
# Output: Table A05.tex — LaTeX tabular fragment for Table A.5 (no fixed effects IV table)
writeLines(lines, "Table A05.tex")

# add_column_numbers() inserts (1)–(5) column numbering row at the top of the tabular
add_column_numbers("Table A05.tex")

