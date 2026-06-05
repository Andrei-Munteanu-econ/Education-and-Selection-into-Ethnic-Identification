# Produces Table 1: Changes in Ethnic Identification by Education: Instrumental Variables Approach

# --- Load and prepare 1992-2011 linked Roma panel ---

#load data
setwd(wd_data_linked)
# data_1992_2011_r: individual-level panel linking 1992 and 2011 census records for Roma-identified persons
data_1992_2011_r<-fread('data_1992_2011_roma_unique.csv')
# Construct language-based Roma flags as alternatives to self-identification (ZZ==12):
# ROMA_1992_LIM: Romani mother-tongue in 1992; ROMA_2011_LIM: Romani mother-tongue in 2011
data_1992_2011_r<-data_1992_2011_r %>%
  mutate(ROMA_1992_LIM=LIM_1992==12,
         ROMA_2011_LIM=LIM_2011==1201)

# --- Load and prepare 2011 census records (children sample) ---

setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
# Read a representative sample first to determine column structure
data_2011<-read_sample(filename)
# Keep only the variables needed for the mother-child matching and IV analysis
data_2011<-data_2011 %>%
  select(id11,id11_MOM,id11_MOM_BC,nat,LIM,AA,years,SIRUTA,SIRSUP,ROMA,ROMA_MOM,HHID,scoala_m,years_MOM,years_POP,SEX,category,source)
# Re-read the full dataset (using selected columns) and add Roma flags:
# ROMA_bc: Roma by birth-certificate ethnicity (nat==12); ROMA_lim: Roma by mother tongue (LIM==1201)
data_2011<-read_data(filename,data_2011) %>%
  mutate(ROMA_bc=nat==12) %>%
  mutate(ROMA_lim=LIM==1201)
# Restrict to children born 2002-2011 to identify mothers whose 2011 identification
# can be observed via their child's birth certificate (a non-self-reported check)
data_2011_kids<-data_2011 %>%
  filter(AA %in% 2002:2011)

# --- Build mother-child linked dataset ---

# Join children (born 2002-2011) to the 1992-2011 matched panel via the mother's 2011 census id.
# id11_MOM_BC in the child record equals id11 in the mother's linked record.
# This yields a dataset where each row is a child whose mother is observed in both 1992 and 2011,
# enabling IV estimation of the effect of the mother's education on her child's ethnic identification.
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



# --- Baseline OLS model: 1992-2011 panel, individual self-identification ---

#baseline mom-11 mom
# OLS of 2011 Roma self-identification on 2011 years of schooling.
# Fixed effects: locality (SIRSUP_1992) x birth-year (AA_1992) cell, to control for
# cohort-by-locality confounders (e.g., local education expansion, assimilation trends).
# Standard errors clustered at the locality level to account for within-town correlation.
model_ols_baseline_11_92<-feols(data=data_1992_2011_r %>% filter(!is.na(years_1992)),
                                ROMA_2011~years_2011|SIRSUP_1992^AA_1992,
                                cluster=~SIRSUP_1992)
summary(model_ols_baseline_11_92)

# --- Baseline IV model: 1992-2011 panel ---

#baseline mom-11 mom
# IV: instruments years_2011 (endogenous) with years_1992 (1992 schooling).
# Identification: 1992 education predicts 2011 education but affects 2011 ethnic
# identification only through the education channel (exclusion restriction).
# Same locality x birth-year FE and clustered SEs as the OLS above.
model_iv_baseline_11_92<-feols(data=data_1992_2011_r,
                               ROMA_2011~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92)

# --- IV model using birth-certificate Roma status as outcome ---

#baseline mom-bc
# Outcome is ROMA_bc (Roma recorded on birth certificate of a child born 2002-2011),
# a non-self-reported measure of ethnic identification -- avoids potential self-report bias.
# Sample restricted to mothers who self-identified as Roma in 1992 (ROMA_1992==T).
model_iv_baseline_bc_92<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                               ROMA_bc~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_bc_92)



# --- OLS cross-section: birth-certificate Roma mothers, 2011 self-identification ---

#bc-11 mom
# Cross-sectional OLS using only mothers identified as Roma by birth certificate (ROMA_bc==T).
# Checks whether the IV result holds in this selected sub-sample.
# FE: 2011 locality x child birth-year cell (SIRSUP_2011 x AA_2011).
model_ols_bc_11<-feols(data=data_1992_2011_mom %>% filter(ROMA_bc==T),
                               ROMA_2011~years_2011|SIRSUP_2011^AA_2011,
                               cluster=~SIRSUP_2011)
summary(model_ols_bc_11)

# --- OLS cross-section: VSN birth-record sample, mother's identification via child ---

#bc-11 mom
# Uses the 2011 census children file directly (not the linked panel).
# Outcome: ROMA_MOM -- whether the mother is Roma according to the child's household record.
# Exploits the fact that mothers of young children can be identified ethnically via VSN
# (vital statistics / birth records) independently of the mother's own census response.
model_ols_bc_11_MOM<-feols(data=data_2011 %>% filter(ROMA_bc==T),
                      ROMA_MOM~years_MOM|SIRSUP^AA,
                      cluster=~SIRSUP)
summary(model_ols_bc_11_MOM)

# --- IV model: mother's 2011 education -> child's birth-certificate Roma status ---

#baseline mom-bc
# IV specification where the outcome is ROMA_bc (child's birth-certificate ethnicity).
# This is the intergenerational transmission channel: more-educated mothers are less
# likely to register their child as Roma on the birth certificate.
model_iv_baseline_11_92_child<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA_bc~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child)

# --- IV model: mother's 2011 education -> child's 2011 census self-identification ---

#baseline mom-2011 child
# Outcome is the child's own Roma self-identification in the 2011 census (ROMA),
# as opposed to the birth-certificate record. Compares administrative vs. self-reported
# ethnicity for children of Roma mothers.
model_iv_baseline_11_92_child2<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child2)




# --- Robustness: VSN-linked mothers, 1992-2011 passing ---

#robustness check
#moms from VSN: 92-11 passing
#ols
# IV specification on the VSN-matched mother sub-sample (mothers identifiable via birth records),
# using the main 1992-2011 passing outcome. Checks that results hold within the VSN sub-sample.
model_iv_baseline_11_92_momz_iv<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA_2011~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_momz_iv)

#iv
# OLS counterpart on the same VSN sub-sample (no instrumentation) for comparison.
model_iv_baseline_11_92_momz<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                    ROMA_2011~years_2011|SIRSUP_1992^AA_1992,
                                    cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_momz)



# --- Table formatting setup ---

######
# Coefficient rename map: standardise the display label across OLS / IV / cross-section models.
# Different variable names (years_2011, years_MOM, fit_years_2011, etc.) all refer to years of
# schooling in different model specifications and are shown under one unified label.
variables<-c('years_2011'='Schooling Yrs',
             'years_MOM'='Schooling Yrs',
             'years_1992'='Schooling Yrs',
             'years_2002'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_MOM_1992'="Schooling Yrs Mom (Baseline)",
             'years_POP_1992'="Schooling Yrs Dad (Baseline)",
             "fit_years_MOM_CHILD"='Schooling Yrs (Endline)')
# f_big: number formatter for large integers (adds thousands separator, no scientific notation)
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
# Ensure modelsummary outputs raw numeric values rather than formatted strings in LaTeX
options(modelsummary_format_numeric_latex = "plain")
# glance_custom.fixest: adds a "Mean of DV" goodness-of-fit row to modelsummary output.
# Computes the unconditional mean of the outcome variable from fitted values + residuals.
glance_custom.fixest <- function(x, ...) {
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# Build the first-stage F-statistic row (instrument relevance diagnostic).
# Blank cells for OLS columns (no first stage); numeric F-stats for IV columns.
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



# --- Produce LaTeX table ---

#IV table
# Columns: OLS baseline | IV baseline | OLS (VSN) | IV (child BC) | IV (child census).
# Stars follow the estimate (no separate significance column).
# GOF rows: N, R^2, DV Mean (from glance_custom.fixest above).
tab <- modelsummary(list(
  "OLS '92-'11"=model_ols_baseline_11_92,
  # "OLS Birth-'11 Mom"=model_ols_bc_11_MOM,
  "IV '92-'11"=model_iv_baseline_11_92,
  "OLS 'VSN-'11"=model_ols_bc_11_MOM,
  # "IV '92-Birth"=model_iv_baseline_bc_92,
  "IV '92-'11"=model_iv_baseline_11_92_child,
  "IV '92-'11"=model_iv_baseline_11_92_child2
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

# Split the raw LaTeX string into individual lines for surgical post-processing
lines <- strsplit(tab, "\n")[[1]]

# The Sample/Outcome block: each cell stacks 2-3 lines via \makecell
# Rows describe (i) which panel / cohort the sample comes from and
# (ii) which ethnic-identification measure is used as the outcome in each column.
sample_outcome <- c(
  "\\midrule",
  "Sample & '92-'11 & '92-'11 & VSN-'11 & '92-VSN-'11 & '92-VSN-'11 \\\\",
  "Outcome & \\makecell{Individual\\\\Census '11} & \\makecell{Individual\\\\Census '11} & \\makecell{Mother\\\\Census '11} & \\makecell{Child\\\\VSN} & \\makecell{Child\\\\Census '11} \\\\"
)

# insert right before the \midrule that precedes N (the GOF block)
# find the midrule that comes just before the "N &" row
n_row   <- grep("^N &", lines)[1]
mid_idx <- max(grep("midrule", lines[seq_len(n_row)]))   # last midrule before N

# Splice the sample/outcome rows immediately before the GOF midrule
lines <- append(lines, sample_outcome, after = mid_idx - 1)
# Output: LaTeX tabular saved to Table 01.tex in the working directory
writeLines(lines, "Table 01.tex")

# Post-process the .tex file to add (1)-(5) column-number header row
add_column_numbers("Table 01.tex")

