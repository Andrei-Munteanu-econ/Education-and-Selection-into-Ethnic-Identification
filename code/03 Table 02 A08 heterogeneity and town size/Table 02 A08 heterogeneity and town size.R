###############################################################################
# Script:  Table 2.R
# Purpose: Build the heterogeneity table (Table 2, "Passing by Education --
#          Heterogeneity", label tab.passing.het) as a 3-panel LaTeX tabular,
#          and a separate appendix table of passing by town size (6 columns,
#          1992-2011 and 2002-2011 samples).
# Output:  - Table 2.tex            (3 panels: gender/parental educ; other
#                                     individual & family chars; age & hh head)
#          - Table town size.tex    (6 cols: <5k / 5k-50k / 50k+, by sample)
# Note:    All models use the main IV strategy (instrument 2011 education with
#          baseline education, FE = commune x birth-year + census) estimated on
#          subgroups. Panels use the 1992-2011 linked Roma sample (data_92);
#          the town-size appendix adds the 2002-2011 sample (data_02).
###############################################################################

# --- Load Linked Roma Samples ---

#load linked Roma data----
setwd(wd_data_linked)
filename<-'data_1992_2011_roma_unique.csv'
# data_92: 1992-2011 linked Roma sample; each row is one individual matched
# across the two censuses and identified as Roma in 1992.
data_92<-read_sample(filename)
data_92<-read_data(filename,data_92) %>%
  # pop_1992: baseline commune population categorised into three size bins
  # (<5,000; 5,000-50,000; 50,000+) for the town-size heterogeneity analysis.
  mutate(pop_1992=cut(pop_SIRSUP_1992,
                      breaks = c(0,5000,50000,Inf),
                      labels = c("<5000", "5k-50k","50k+")))

setwd(wd_data_linked)
filename<-'data_2002_2011_roma_unique.csv'
# data_02: 2002-2011 linked Roma sample; parallel structure to data_92 but
# uses 2002 as the baseline census (used only for the town-size appendix).
data_02<-read_sample(filename)
data_02<-read_data(filename,data_02) %>%
  # pop_2002: same three-bin categorisation applied to 2002 commune populations.
  mutate(pop_2002=cut(pop_SIRSUP_2002,
                      breaks = c(0,5000,50000,Inf),
                      labels = c("<5000", "5k-50k","50k+")))

# --- Load Full Census Records (for Household-Member Roma Flags) ---

#identify Roma family connections across censuses (for Roma-spouse definition)----
setwd(wd_data_92)
filename<-'data_1992_clean.csv'
# Load 1992 full census; retain only the columns needed to identify Roma family
# ties: own Roma flag, parents' ethnicity, spouse ethnicity, dwelling source,
# and the individual line-number ID used for cross-file matching.
data_1992<-read_sample(filename) %>%
  select(ROMA,ET_MOM,ET_POP,ET_SPOUSE,source,id92)
data_1992<-read_data(filename,data_1992)
# data_1992_roma_loose: anyone with a Roma parent or who is themselves Roma —
# used as the 1992 universe for determining whether a spouse is Roma-connected.
data_1992_roma_loose<-data_1992 %>%
  filter(ET_MOM==12 | ET_POP==12 | ROMA==T)

setwd(wd_data_02)
filename<-'data_2002_clean.csv'
# Same procedure for the 2002 census; id02 links individuals to data_02.
data_2002<-read_sample(filename) %>%
  select(ROMA,ET_MOM,ET_POP,ET_SPOUSE,source,id02)
data_2002<-read_data(filename,data_2002)
data_2002_roma_loose<-data_2002 %>%
  filter(ET_MOM==12 | ET_POP==12 | ROMA==T)

setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
# Same procedure for 2011; note ET_MOM/ET_POP codes are 1200-1299 in 2011
# (extended code scheme), so we match with %in% 1200:1299 rather than ==12.
data_2011<-read_sample(filename) %>%
  select(ROMA,ET_MOM,ET_POP,ET_SPOUSE,source,id11)
data_2011<-read_data(filename,data_2011)
data_2011_roma_loose<-data_2011 %>%
  filter(ET_MOM %in% 1200:1299 | ET_POP %in% 1200:1299 | ROMA==T)

# --- Construct Heterogeneity Variables and Standardise Column Names ---

#construct heterogeneity variables and rename baseline columns----
# ROMA_SPOUSE_FAMILY: spouse ever recorded Roma (or had Roma parents) in any census.
# migrant: changed commune between baseline and 2011.
# Rename _1992/_2002 -> _baseline so the same model code applies to both cohorts.
data_92<-data_92 %>%
  # ROMA_SPOUSE_FAMILY: broad Roma-spouse indicator — TRUE if the individual's
  # 1992 spouse appears in the loose-Roma list for either 1992 or 2011, or if
  # the 2011 ethnicity code for the spouse falls in the Roma range (1200-1299).
  # This captures both stable Roma-identified spouses and those who later passed.
  mutate(ROMA_SPOUSE_FAMILY=(id92_SPOUSE %in% data_1992_roma_loose$id92 |
                               id11_SPOUSE %in% data_2011_roma_loose$id11 |
                               ET_SPOUSE_2011 %in% 1200:1299),
         # ROMA_SPOUSE_1992: narrow indicator using only 1992 ethnicity code (==12).
         ROMA_SPOUSE_1992=ET_SPOUSE_1992 %in% 12,
         # ROMA_SPOUSE: indicator based on the 2011 ethnicity code alone.
         ROMA_SPOUSE=ET_SPOUSE_2011 %in% 1200:1299) %>%
  # migrant: TRUE if the individual resided in a different commune in 2011 than
  # in 1992; used to test whether geographic mobility mediates passing.
  mutate(migrant= SIRSUP_2011!=SIRSUP_1992) %>%
  # Standardise all year-specific suffixes to "_baseline" so Panel 1-3 model
  # code can be re-used without modification for the 2002-2011 cohort.
  rename_with(.fn = ~gsub("_1992","_baseline",.))

# Identical Roma-spouse and migrant construction for the 2002-2011 cohort.
data_02<-data_02 %>%
  mutate(ROMA_SPOUSE_FAMILY=(id02_SPOUSE %in% data_2002_roma_loose$id02 |
                               id11_SPOUSE %in% data_2011_roma_loose$id11 |
                               ET_SPOUSE_2011 %in% 1200:1299),
         ROMA_SPOUSE_2002=ET_SPOUSE_2002 %in% 12,
         ROMA_SPOUSE=ET_SPOUSE_2011 %in% 1200:1299) %>%
  mutate(migrant= SIRSUP_2011!=SIRSUP_2002) %>%
  rename_with(.fn = ~gsub("_2002","_baseline",.))

# AGE_baseline: age at the baseline census, computed as (census year) - birth year - 1.
# The -1 accounts for the fact that not everyone had their birthday before census day.
data_92<-data_92 %>% mutate(AGE_baseline=1992-AA_2011-1)
data_02<-data_02 %>% mutate(AGE_baseline=2002-AA_2011-1)

# Add a "census" label so both cohorts can be stacked if needed, and for
# use as a FE in the within-cohort model (two rounds per cohort possible).
data_reg_92<-data_92 %>% mutate(census="92")
data_reg_02<-data_02 %>% mutate(census="02")


# --- IV Regressions by Subgroup ---

#regressions----
## Panel 1: gender and parental education ----
# All Panel 1 models share the same IV specification:
#   Outcome:    ROMA_2011 (1 = still Roma-identified in 2011)
#   Endogenous: years_2011 (years of schooling in 2011)
#   Instrument: years_baseline (years of schooling in baseline census)
#   Fixed effects: commune x birth-year (SIRSUP_baseline^AA_baseline) + census wave
#   SE cluster: commune (SIRSUP_baseline), accounting for within-locality correlation.
# Running separately by subgroup tests whether the education-passing gradient
# differs across gender and parental education backgrounds.

# Columns 1-2: male vs. female subsamples, no parental controls.
model_sex_92_iv_m<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                         data=data_reg_92 %>% filter(SEX_2011==1),
                         cluster=~SIRSUP_baseline)
model_sex_92_iv_f<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                         data=data_reg_92 %>% filter(SEX_2011==2),
                         cluster=~SIRSUP_baseline)
# Columns 3-4: male vs. female, adding mother's and father's years of schooling
# as controls to partial out intergenerational transmission of education.
model_sex_92_iv_m_parent<-feols(ROMA_2011~years_MOM_baseline+years_POP_baseline|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                data=data_reg_92 %>% filter(SEX_2011==1),
                                cluster=~SIRSUP_baseline)
model_sex_92_iv_f_parent<-feols(ROMA_2011~years_MOM_baseline+years_POP_baseline|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                data=data_reg_92 %>% filter(SEX_2011==2),
                                cluster=~SIRSUP_baseline)
# Columns 5-6: split by mother's education (>=8 years = completed gymnasium or more).
model_mom_educ_92_iv_h<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                              data=data_reg_92 %>% filter(years_MOM_baseline>=8),
                              cluster=~SIRSUP_baseline)
model_mom_educ_92_iv_l<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                              data=data_reg_92 %>% filter(years_MOM_baseline<8),
                              cluster=~SIRSUP_baseline)
# Columns 7-8: split by father's education (same 8-year threshold).
model_pop_educ_92_iv_h<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                              data=data_reg_92 %>% filter(years_POP_baseline>=8),
                              cluster=~SIRSUP_baseline)
model_pop_educ_92_iv_l<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                              data=data_reg_92 %>% filter(years_POP_baseline<8),
                              cluster=~SIRSUP_baseline)

## Panel 2: other individual and family characteristics ----
# Columns 1-2: inter-commune migrants vs. non-migrants between 1992 and 2011.
# Tests whether the passing effect operates through geographic assimilation.
model_migration_92_iv_m<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                               data=data_reg_92 %>% filter(migrant==T),
                               cluster=~SIRSUP_baseline)
model_migration_92_iv_nm<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                data=data_reg_92 %>% filter(migrant==F),
                                cluster=~SIRSUP_baseline)
# Columns 3-4: Romani vs. non-Romani native language (LIM_baseline == 12).
# Tests whether language-based Roma identity moderates the education-passing link.
model_lang_92_iv_romani<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                               data=data_reg_92 %>% filter(LIM_baseline %in% c(12)),
                               cluster=~SIRSUP_baseline)
model_lang_92_iv_romanian<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                 data=data_reg_92 %>% filter(!LIM_baseline %in% c(12)),
                                 cluster=~SIRSUP_baseline)
# Columns 5-6: Orthodox Christian (REL_baseline == 10) vs. other religion.
# Tests whether traditional religious community ties affect passing propensity.
model_rel_92_iv_trad<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                            data=data_reg_92 %>% filter(REL_baseline %in% c(10)),
                            cluster=~SIRSUP_baseline)
model_rel_92_iv_neo<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                           data=data_reg_92 %>% filter(!REL_baseline %in% c(10)),
                           cluster=~SIRSUP_baseline)
# Columns 7-8: Roma-connected spouse (ROMA_SPOUSE_FAMILY) vs. no Roma spouse.
# The ROMA_baseline==T restriction ensures the denominator is always Roma-identified
# individuals; the split tests whether social network embeddedness moderates passing.
model_spouse_92_iv_roma<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                               data=data_reg_92 %>% filter(ROMA_SPOUSE_FAMILY==T & ROMA_baseline==T),
                               cluster=~SIRSUP_baseline)
model_spouse_92_iv_nroma<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                data=data_reg_92 %>% filter(ROMA_SPOUSE_FAMILY==F & ROMA_baseline==T),
                                cluster=~SIRSUP_baseline)

## Panel 3: baseline age and household head status ----
# GRUD_baseline: household head indicator (1 = head of household at baseline).
# Household heads may differ in social visibility and incentives to pass.
# Columns 1-2: head vs. non-head at baseline.
model_head_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                           data=data_reg_92 %>% filter(GRUD_baseline==1),
                           cluster=~SIRSUP_baseline)
model_nhead_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                            data=data_reg_92 %>% filter(GRUD_baseline!=1),
                            cluster=~SIRSUP_baseline)
# Column 3: head in both 1992 and 2011 — persistently high-status individuals.
model_head_always<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                         data=data_reg_92 %>% filter(GRUD_baseline==1 & GRUD_2011==1),
                         cluster=~SIRSUP_baseline)
# Columns 4-7: age cohorts (10-20, 21-30, 31-40, 41+) at baseline.
# Younger individuals may be more responsive to education because their ethnic
# identity is less entrenched, and their schooling investment is more recent.
model_young_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                            data=data_reg_92 %>% filter(AGE_baseline %in% 10:20),
                            cluster=~SIRSUP_baseline)
model_20_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                         data=data_reg_92 %>% filter(AGE_baseline %in% 21:30),
                         cluster=~SIRSUP_baseline)
model_30_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                         data=data_reg_92 %>% filter(AGE_baseline %in% 31:40),
                         cluster=~SIRSUP_baseline)
model_old_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                          data=data_reg_92 %>% filter(AGE_baseline>=41),
                          cluster=~SIRSUP_baseline)

## Town size: 1992-2011 and 2002-2011 samples ----
# Six models: three town-size bins x two cohorts.
# Tests whether the education-passing effect differs by urban vs. rural context;
# larger towns may offer more anonymity and more diverse ethnic networks.
model_5k_92<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                   data=data_reg_92 %>% filter(pop_baseline %in% "<5000"),
                   cluster=~SIRSUP_baseline)
model_5_50k_92<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                      data=data_reg_92 %>% filter(pop_baseline %in% "5k-50k"),
                      cluster=~SIRSUP_baseline)
model_50k_92<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                    data=data_reg_92 %>% filter(pop_baseline %in% "50k+"),
                    cluster=~SIRSUP_baseline)
# 2002-2011 cohort: same three bins using 2002 commune populations (renamed to
# pop_baseline by rename_with above).
model_5k_02<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                   data=data_reg_02 %>% filter(pop_baseline %in% "<5000"),
                   cluster=~SIRSUP_baseline)
model_5_50k_02<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                      data=data_reg_02 %>% filter(pop_baseline %in% "5k-50k"),
                      cluster=~SIRSUP_baseline)
model_50k_02<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                    data=data_reg_02 %>% filter(pop_baseline %in% "50k+"),
                    cluster=~SIRSUP_baseline)


# --- Table Formatting Helpers ---

#table formatting setup----
# f_big: formats large integers with comma separators and no scientific notation,
# used for observation counts in the N row.
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
# Suppress modelsummary's default behaviour of wrapping numbers in \num{} macros.
options(modelsummary_format_numeric_latex = "plain")
# glance_custom.fixest: adds a "Mean of DV" row to every fixest model's GOF block.
# The DV mean is recovered as the average of fitted values plus residuals (= raw outcome).
glance_custom.fixest <- function(x, ...) {
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# F-stat row helper: one column per model, in the same order as the model list.
# Extracts the first-stage F-statistic for the instrument (ivf1$stat) from each
# fixest model and formats the result as a data frame for add_rows in modelsummary.
fstat_row <- function(...) {
  vals <- lapply(list(...), function(m) fitstat(m, "ivf")$ivf1$stat)
  df <- data.frame(n = "F-stat", stringsAsFactors = FALSE)
  for (i in seq_along(vals)) df[[paste0("c", i)]] <- vals[[i]]
  f_big(df)
}

# Run modelsummary -> latex_tabular and return only the body: the coefficient
# rows, the internal \midrule, and the GOF rows (N / R2 / DV Mean / F-stat).
# Stripping the tabular environment header/footer allows the three panels to be
# assembled manually into a single multi-panel tabular below.
ms_body <- function(models, fstat, coef_rename, align) {
  tab <- modelsummary(models,
                      estimate="{estimate}{stars}",
                      statistic = "std.error",
                      stars=c('$^{*}$'=0.1,'$^{**}$'=0.05,'$^{***}$'=0.01),
                      gof_map=list(list("raw" = "nobs", "clean" = "N", "fmt" = f_big),
                                   list("raw" = "r.squared", "clean" = "R$^2$",fmt="%.2f"),
                                   list("raw"="Mean of DV","clean"="DV Mean",fmt="%.2f")),
                      metrics="R2",
                      add_rows = fstat,
                      output="latex_tabular",
                      coef_rename=coef_rename,
                      align=align,
                      escape=F)
  if (!is.character(tab)) tab <- paste(as.character(tab), collapse="\n")
  lines <- strsplit(tab, "\n")[[1]]
  # Locate the first \midrule (separating coefficients from GOF) and \bottomrule.
  first_mid <- grep("midrule", lines)[1]
  bot       <- grep("bottomrule", lines)[1]
  # Return only the interior rows: coefficients + GOF, excluding the outer rules.
  lines[(first_mid+1):(bot-1)]
}

# variables: coefficient label mapping for Panels 1-2 (two endogenous regressors
# plus parental education controls).
variables    <- c('fit_years_2011'='Schooling Yrs (Endline)',
                  'years_2011'='Schooling Yrs (Endline)',
                  'years_MOM_baseline'="Schooling Years (Mother)",
                  'years_POP_baseline'="Schooling Years (Father)")
# variables_p3: shorter label for Panel 3 where no parental controls are included.
variables_p3 <- c('fit_years_2011'='Schooling Yrs',
                  'years_2011'='Schooling Yrs')


# --- Assemble Table 2: Three-Panel Heterogeneity Table ---

#Table 2: 3-panel heterogeneity table----
# Extract first-stage F-statistics for all 8 Panel 1 models.
f1 <- fstat_row(model_sex_92_iv_m, model_sex_92_iv_f,
                model_sex_92_iv_m_parent, model_sex_92_iv_f_parent,
                model_mom_educ_92_iv_h, model_mom_educ_92_iv_l,
                model_pop_educ_92_iv_h, model_pop_educ_92_iv_l)
# body1: coefficient estimates, SE, and GOF for the 8 Panel 1 subgroup models.
body1 <- ms_body(list("m1"=model_sex_92_iv_m,
                      "m2"=model_sex_92_iv_f,
                      "m3"=model_sex_92_iv_m_parent,
                      "m4"=model_sex_92_iv_f_parent,
                      "m5"=model_mom_educ_92_iv_h,
                      "m6"=model_mom_educ_92_iv_l,
                      "m7"=model_pop_educ_92_iv_h,
                      "m8"=model_pop_educ_92_iv_l),
                 f1, variables, "lcccccccc")

# Panel 2 F-stats and body.
f2 <- fstat_row(model_migration_92_iv_m, model_migration_92_iv_nm,
                model_lang_92_iv_romani, model_lang_92_iv_romanian,
                model_rel_92_iv_trad, model_rel_92_iv_neo,
                model_spouse_92_iv_roma, model_spouse_92_iv_nroma)
body2 <- ms_body(list("m1"=model_migration_92_iv_m,
                      "m2"=model_migration_92_iv_nm,
                      "m3"=model_lang_92_iv_romani,
                      "m4"=model_lang_92_iv_romanian,
                      "m5"=model_rel_92_iv_trad,
                      "m6"=model_rel_92_iv_neo,
                      "m7"=model_spouse_92_iv_roma,
                      "m8"=model_spouse_92_iv_nroma),
                 f2, variables, "lcccccccc")

# Panel 3 F-stats and body (7 models).
f3 <- fstat_row(model_head_baseline, model_nhead_baseline, model_head_always,
                model_young_baseline, model_20_baseline, model_30_baseline,
                model_old_baseline)
body3 <- ms_body(list("m1"=model_head_baseline,
                      "m2"=model_nhead_baseline,
                      "m3"=model_head_always,
                      "m4"=model_young_baseline,
                      "m5"=model_20_baseline,
                      "m6"=model_30_baseline,
                      "m7"=model_old_baseline),
                 f3, variables_p3, "lccccccc")
# Panel 3 has 7 models shown in data columns 2-8: pad each row with a leading
# empty data cell so it aligns with the 8-column master tabular.
body3 <- sub("&", "& &", body3, fixed = TRUE)

# LaTeX column headers for each panel, including multicolumn span labels and
# sub-group labels. These are manually constructed strings so that each panel
# can carry its own descriptive header inside the shared tabular environment.
p1_header <- c(
  "& \\multicolumn{8}{c}{Panel 1: Passing, Gender and Parental Education}\\\\",
  "& \\multicolumn{8}{c}{\\textit{Dependent Variable: Reported Roma Ethnicity (2011)}}\\\\",
  "& \\multicolumn{2}{c}{Sex} & \\multicolumn{2}{c}{Sex \\& Parental Educ.} & \\multicolumn{2}{c}{Mom Education} & \\multicolumn{2}{c}{Dad Education}\\\\",
  "& Male & Female & Male & Female & $\\geq 8$ Yrs & $<8$ Yrs & $\\geq 8$ Yrs & $<8$ Yrs \\\\",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\")

p2_header <- c(
  "& \\multicolumn{8}{c}{Panel 2: Passing and Other Individual and Family Characteristics}\\\\",
  "& \\multicolumn{8}{c}{\\textit{Dependent Variable: Reported Roma Ethnicity (2011)}}\\\\",
  "& \\multicolumn{2}{c}{Migrant} & \\multicolumn{2}{c}{Native Language} & \\multicolumn{2}{c}{Orthodox Religion} & \\multicolumn{2}{c}{Roma Spouse}\\\\",
  "& Yes & No & Romani & Non-Romani & Yes & No & Yes & No/N.A.\\\\",
  "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\")

p3_header <- c(
  "& & \\multicolumn{7}{c}{Panel 3: Baseline Age and Household Head Status}\\\\",
  "& & \\multicolumn{7}{c}{\\textit{Dependent Variable: Reported Roma Ethnicity (2011)}}\\\\",
  "& & Head & Not Head & Head & 10-20 y.o. & 20s & 30s & 40+ y.o.\\\\",
  "& & Baseline & Baseline & Always & Baseline & Baseline & Baseline & Baseline\\\\",
  "& & (1) & (2) & (3) & (4) & (5) & (6) & (7) \\\\")

# Stitch the three panels together into a single LaTeX tabular: outer rules at
# top and bottom, \midrule between each panel header and its body, and between
# consecutive panels. The result is a self-contained tabular (no \begin{table}).
table2 <- c(
  "\\begin{tabular}[t]{lcccccccc}",
  "\\toprule",
  p1_header,
  "\\midrule",
  body1,
  "\\midrule",
  p2_header,
  "\\midrule",
  body2,
  "\\midrule",
  p3_header,
  "\\midrule",
  body3,
  "\\bottomrule",
  "\\end{tabular}")

# Output: Table 02.tex — the main heterogeneity table.
setwd(wd_output)
writeLines(table2, "Table 02.tex")


# --- Appendix Table A08: Passing by Town Size ---

#Appendix: passing by town size (6 columns)----
# F-stats for the six town-size x cohort models.
ftown <- fstat_row(model_5k_92, model_5_50k_92, model_50k_92,
                   model_5k_02, model_5_50k_02, model_50k_02)
town_body <- ms_body(list("m1"=model_5k_92,
                          "m2"=model_5_50k_92,
                          "m3"=model_50k_92,
                          "m4"=model_5k_02,
                          "m5"=model_5_50k_02,
                          "m6"=model_50k_02),
                     ftown,
                     c('fit_years_2011'='Schooling Yrs (Endline)',
                       'years_2011'='Schooling Yrs (Endline)'),
                     "lcccccc")
# Split the body into coefficient rows and GOF rows so the Sample / Town
# Population labels can be inserted between them.
town_mid  <- grep("midrule", town_body)[1]
# town_coef: rows above the internal \midrule (coefficient estimates and SEs).
town_coef <- town_body[1:(town_mid-1)]
# town_gof: rows below the internal \midrule (N, R^2, DV Mean, F-stat).
town_gof  <- town_body[(town_mid+1):length(town_body)]

# Assemble the appendix table; Sample and Town Population descriptor rows are
# inserted manually between the coefficient block and the GOF block.
town_table <- c(
  "\\begin{tabular}[t]{lcccccc}",
  "\\toprule",
  "& \\multicolumn{6}{c}{\\textit{Dependent Variable: Reported Roma Ethnicity (2011)}}\\\\",
  "& (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\midrule",
  town_coef,
  "\\midrule",
  "Sample & '92-'11 & '92-'11 & '92-'11 & '02-'11 & '02-'11 & '02-'11 \\\\",
  "Town Population & $<$5k & 5k-50k & 50k+ & $<$5k & 5k-50k & 50k+ \\\\",
  "\\midrule",
  town_gof,
  "\\bottomrule",
  "\\end{tabular}")

# Output: Table A08.tex — appendix table of passing by town size.
setwd(wd_output)
writeLines(town_table, "Table A08.tex")
