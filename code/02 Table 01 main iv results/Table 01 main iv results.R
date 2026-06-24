# =====================================================================
# Table 01 — Main IV results: effect of schooling on Roma self-identification
# Produces:  output/Table 01.tex
# Inputs:    data_1992_2011_roma_unique.csv (1992-2011 linked Roma panel),
#            data_2011_clean_births.csv (2011 census linked to birth records)
# Summary:   Estimates the causal effect of years of schooling on the probability
#            of reporting Roma ethnicity, instrumenting 2011 schooling (years_2011)
#            with baseline-census schooling (years_1992). Fixed effects are commune x
#            birth-year (SIRSUP_1992^AA_1992); SEs clustered at the commune (SIRSUP).
#            Columns: an OLS benchmark, the main IV on 1992-Roma individuals, an OLS
#            for VSN-linked mothers, and two IV columns differing only in the outcome
#            (child's birth-certificate ethnicity vs. child's 2011 self-report).
# =====================================================================

# ---- Load data ----
setwd(wd_data_linked)
data_1992_2011_r<-fread('data_1992_2011_roma_unique.csv')
# Add language-based Roma indicators (Romani as native language: code 12 in 1992,
# 1201 in 2011); not used in the main spec but kept for consistency with other scripts.
data_1992_2011_r<-data_1992_2011_r %>%
  mutate(ROMA_1992_LIM=LIM_1992==12,
         ROMA_2011_LIM=LIM_2011==1201)

# Load the 2011 census already linked to birth records (children + their mothers).
setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
data_2011<-read_sample(filename)
data_2011<-data_2011 %>%
  select(id11,id11_MOM,id11_MOM_BC,nat,LIM,AA,years,SIRUTA,SIRSUP,ROMA,ROMA_MOM,HHID,scoala_m,years_MOM,years_POP,SEX,category,source)
# ROMA_bc: child is recorded as Roma on the birth certificate (nat==12).
# ROMA_lim: Romani native language in 2011 (LIM==1201).
data_2011<-read_data(filename,data_2011) %>%
  mutate(ROMA_bc=nat==12) %>%
  mutate(ROMA_lim=LIM==1201)
# Restrict to children born 2002-2011 (the cohort with usable birth records).
data_2011_kids<-data_2011 %>%
  filter(AA %in% 2002:2011)

# ---- Build VSN mother-child linked sample ----
# Link each child to its mother (via the mother's birth-certificate id) and attach
# the mother's full 1992-2011 census history. Suffixes _CHILD / _MOM disambiguate
# shared column names between the child record and the mother's record.
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



# ---- Specifications ----

# Column 1 (OLS): naive OLS of 2011 Roma self-report on 2011 schooling, on the
# 1992-2011 linked Roma panel (restricted to individuals with non-missing 1992
# schooling so the sample matches the IV column). No instrument.
model_ols_baseline_11_92<-feols(data=data_1992_2011_r %>% filter(!is.na(years_1992)),
                                ROMA_2011~years_2011|SIRSUP_1992^AA_1992,
                                cluster=~SIRSUP_1992)
summary(model_ols_baseline_11_92)
# Column 2 (main IV): instrument 2011 schooling with baseline (1992) schooling on
# the same 1992-2011 linked Roma panel. This is the headline estimate.
model_iv_baseline_11_92<-feols(data=data_1992_2011_r,
                               ROMA_2011~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92)
# IV with the child's birth-certificate ethnicity as outcome, on mothers who were
# Roma in 1992 (not shown in the final table — commented out in the model list below).
model_iv_baseline_bc_92<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                               ROMA_bc~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_bc_92)



# OLS of mother's 2011 Roma self-report on her schooling, among children recorded
# Roma on the birth certificate (not shown in the final table).
model_ols_bc_11<-feols(data=data_1992_2011_mom %>% filter(ROMA_bc==T),
                               ROMA_2011~years_2011|SIRSUP_2011^AA_2011,
                               cluster=~SIRSUP_2011)
summary(model_ols_bc_11)

# Column 3 (OLS, VSN mothers): among children recorded Roma on the birth certificate,
# OLS of the mother's 2011 Roma self-report on the mother's schooling. Cross-sectional
# (uses 2011 commune/birth-year FE), no instrument.
model_ols_bc_11_MOM<-feols(data=data_2011 %>% filter(ROMA_bc==T),
                      ROMA_MOM~years_MOM|SIRSUP^AA,
                      cluster=~SIRSUP)
summary(model_ols_bc_11_MOM)

# Column 4 (IV): on mothers who were Roma in 1992, instrument 2011 schooling with
# 1992 schooling; outcome is the child's birth-certificate Roma status (ROMA_bc).
model_iv_baseline_11_92_child<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA_bc~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child)

# Column 5 (IV): same sample and instrument as column 4, but the outcome is the
# child's own 2011 census Roma self-report (ROMA) instead of birth-certificate status.
model_iv_baseline_11_92_child2<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child2)





# ---- Robustness checks (not shown in the final table) ----
# Restrict to mothers in the VSN-linked sample who were Roma in 1992, and re-estimate
# the mother's own 2011 passing (ROMA_2011) on her schooling, by IV and by OLS, to
# confirm the headline result survives on this narrower mother subsample.
# IV version (instrument 2011 schooling with 1992 schooling).
model_iv_baseline_11_92_momz_iv<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA_2011~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_momz_iv)

# OLS version of the same robustness check.
model_iv_baseline_11_92_momz<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                    ROMA_2011~years_2011|SIRSUP_1992^AA_1992,
                                    cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_momz)





# ---- Table formatting setup ----
# variables: maps each model's coefficient name to a common display label so the
# OLS, IV, and fitted-IV schooling terms all appear on a single "Schooling Yrs" row.
variables<-c('years_2011'='Schooling Yrs',
             'years_MOM'='Schooling Yrs',
             'years_1992'='Schooling Yrs',
             'years_2002'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_MOM_1992'="Schooling Yrs Mom (Baseline)",
             'years_POP_1992'="Schooling Yrs Dad (Baseline)",
             "fit_years_MOM_CHILD"='Schooling Yrs (Endline)')
# f_big: format integers (e.g. N) with comma thousands separators, no scientific notation.
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
# Stop modelsummary from wrapping numbers in \num{} LaTeX macros.
options(modelsummary_format_numeric_latex = "plain")
# glance_custom.fixest: add a "Mean of DV" GOF row to every fixest model, recovered
# as mean(fitted + residual) = mean of the raw outcome.
glance_custom.fixest <- function(x, ...) {
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# Build the first-stage F-statistic row: empty cells for the OLS columns, and the
# instrument F-stat (ivf1$stat) for each IV column, in the table's column order.
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



# ---- Assemble Table 01 ----
# Five columns: OLS '92-'11, IV '92-'11 (headline), OLS VSN mothers, and the two
# child-outcome IV columns. Estimates carry significance stars; SEs (clustered at
# commune) below; GOF rows are N, R^2, and DV mean, with the F-stat row appended.
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

# ---- Inject Sample/Outcome header rows and write the file ----
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
writeLines(lines, "Table 01.tex")

# Append a (1)...(5) column-number row to the saved table.
add_column_numbers("Table 01.tex")

