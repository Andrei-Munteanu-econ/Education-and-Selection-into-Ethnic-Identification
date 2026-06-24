###############################################################################
# Script:  Table 2.R
# Purpose: Build the heterogeneity table (Table 2) as a 3-panel LaTeX tabular,
#          and a separate appendix table of ethnic identification by town size
#          (Table A08, 6 columns, 1992-2011 and 2002-2011 samples).
# Output:  - Table 02.tex   (3 panels: gender/parental educ; other individual &
#                             family chars; age & hh head) -- 1992-2011 sample
#          - Table A08.tex  (6 cols: <5k / 5k-50k / 50k+, by sample)
# Note:    All models use the main IV strategy (instrument 2011 education with
#          baseline education, FE = commune x birth-year + census) estimated on
#          subgroups. The 3-panel table is built by build_het_table() on the
#          1992-2011 (data_reg_92) linked Roma sample. The 2002-2011 counterpart
#          (Table A15) is produced by the self-contained script in
#          'AA Table A15 heterogeneity 2002-2011/'. The Roma-spouse split is
#          deliberately omitted here; it is covered by the marriage tables
#          (Table 3 and Table A11).
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
# uses 2002 as the baseline census.
data_02<-read_sample(filename)
data_02<-read_data(filename,data_02) %>%
  # pop_2002: same three-bin categorisation applied to 2002 commune populations.
  mutate(pop_2002=cut(pop_SIRSUP_2002,
                      breaks = c(0,5000,50000,Inf),
                      labels = c("<5000", "5k-50k","50k+")))

# --- Construct Heterogeneity Variables and Standardise Column Names ---

#construct heterogeneity variables and rename baseline columns----
# migrant: changed commune between baseline and 2011.
# Rename _1992/_2002 -> _baseline so the same model code applies to both cohorts.
data_92<-data_92 %>%
  # migrant: TRUE if the individual resided in a different commune in 2011 than
  # at baseline; used to test whether geographic mobility mediates ethnic
  # identification.
  mutate(migrant= SIRSUP_2011!=SIRSUP_1992) %>%
  # Standardise all year-specific suffixes to "_baseline" so the panel model
  # code can be re-used without modification for the 2002-2011 cohort.
  rename_with(.fn = ~gsub("_1992","_baseline",.))

# Identical migrant construction for the 2002-2011 cohort.
data_02<-data_02 %>%
  mutate(migrant= SIRSUP_2011!=SIRSUP_2002) %>%
  rename_with(.fn = ~gsub("_2002","_baseline",.))

# AGE_baseline: age at the baseline census, computed as (census year) - birth year - 1.
# The -1 accounts for the fact that not everyone had their birthday before census day.
data_92<-data_92 %>% mutate(AGE_baseline=1992-AA_2011-1)
data_02<-data_02 %>% mutate(AGE_baseline=2002-AA_2011-1)

# Add a "census" label used as a FE in the within-cohort model.
data_reg_92<-data_92 %>% mutate(census="92")
data_reg_02<-data_02 %>% mutate(census="02")


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
# rows, an internal \midrule, and the GOF rows (N / R2 / DV Mean / F-stat).
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
  # modelsummary 2.x 'latex_tabular' returns a bare tabular with no booktabs
  # rules: \begin{tabular}{..}, a model-name header row, the estimate/GOF rows,
  # then \end{tabular}. Keep only the LaTeX data rows (those ending in '\\'),
  # which drops the tabular wrapper, then drop the first one (the header row).
  lines <- sub("\\s+$", "", lines)
  lines <- lines[endsWith(lines, "\\\\")]
  lines <- lines[-1]
  # Re-insert a \midrule between the coefficient rows and the GOF rows. The GOF
  # block begins at the first row whose label is one of the known GOF labels;
  # the multi-panel assembly and the town-size split both rely on this rule.
  gof_labels <- c("N", "R$^2$", "DV Mean", "F-stat")
  labs       <- trimws(sub("&.*$", "", lines))
  first_gof  <- which(labs %in% gof_labels)[1]
  c(lines[seq_len(first_gof - 1)], "\\midrule", lines[first_gof:length(lines)])
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


# --- 3-Panel Heterogeneity Table Builder ---

#build_het_table----
# Estimate the three heterogeneity panels on a linked Roma sample (data_reg) and
# return the assembled 3-panel LaTeX tabular (a character vector). Here it is run
# on the 1992-2011 sample (-> Table 02); the 2002-2011 counterpart (Table A15) is
# produced by an identical copy of this builder in its own script. All models
# share the IV spec:
#   Outcome:       ROMA_2011 (1 = still Roma-identified in 2011)
#   Endogenous:    years_2011 (years of schooling in 2011)
#   Instrument:    years_baseline (years of schooling in the baseline census)
#   Fixed effects: commune x birth-year (SIRSUP_baseline^AA_baseline) + census
#   SE cluster:    commune (SIRSUP_baseline)
build_het_table <- function(data_reg) {
  
  ## Panel 1: gender and parental education ----
  # Columns 1-2: male vs. female subsamples, no parental controls.
  model_sex_92_iv_m<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                           data=data_reg %>% filter(SEX_2011==1),
                           cluster=~SIRSUP_baseline)
  model_sex_92_iv_f<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                           data=data_reg %>% filter(SEX_2011==2),
                           cluster=~SIRSUP_baseline)
  # Columns 3-4: male vs. female, adding mother's and father's years of schooling
  # as controls to partial out intergenerational transmission of education.
  model_sex_92_iv_m_parent<-feols(ROMA_2011~years_MOM_baseline+years_POP_baseline|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                  data=data_reg %>% filter(SEX_2011==1),
                                  cluster=~SIRSUP_baseline)
  model_sex_92_iv_f_parent<-feols(ROMA_2011~years_MOM_baseline+years_POP_baseline|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                  data=data_reg %>% filter(SEX_2011==2),
                                  cluster=~SIRSUP_baseline)
  # Columns 5-6: split by mother's education (>=8 years = completed gymnasium or more).
  model_mom_educ_92_iv_h<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                data=data_reg %>% filter(years_MOM_baseline>=8),
                                cluster=~SIRSUP_baseline)
  model_mom_educ_92_iv_l<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                data=data_reg %>% filter(years_MOM_baseline<8),
                                cluster=~SIRSUP_baseline)
  # Columns 7-8: split by father's education (same 8-year threshold).
  model_pop_educ_92_iv_h<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                data=data_reg %>% filter(years_POP_baseline>=8),
                                cluster=~SIRSUP_baseline)
  model_pop_educ_92_iv_l<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                data=data_reg %>% filter(years_POP_baseline<8),
                                cluster=~SIRSUP_baseline)
  
  ## Panel 2: other individual and family characteristics ----
  # Columns 1-2: inter-commune migrants vs. non-migrants between baseline and 2011.
  # Tests whether the effect operates through geographic assimilation.
  model_migration_92_iv_m<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                 data=data_reg %>% filter(migrant==T),
                                 cluster=~SIRSUP_baseline)
  model_migration_92_iv_nm<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                  data=data_reg %>% filter(migrant==F),
                                  cluster=~SIRSUP_baseline)
  # Columns 3-4: Romani vs. non-Romani native language (LIM_baseline == 12).
  model_lang_92_iv_romani<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                 data=data_reg %>% filter(LIM_baseline %in% c(12)),
                                 cluster=~SIRSUP_baseline)
  model_lang_92_iv_romanian<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                                   data=data_reg %>% filter(!LIM_baseline %in% c(12)),
                                   cluster=~SIRSUP_baseline)
  # Columns 5-6: Orthodox Christian (REL_baseline == 10) vs. other religion.
  model_rel_92_iv_trad<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                              data=data_reg %>% filter(REL_baseline %in% c(10)),
                              cluster=~SIRSUP_baseline)
  model_rel_92_iv_neo<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                             data=data_reg %>% filter(!REL_baseline %in% c(10)),
                             cluster=~SIRSUP_baseline)
  
  ## Panel 3: baseline age and household head status ----
  # GRUD_baseline: household head indicator (1 = head of household at baseline).
  # Columns 1-2: head vs. non-head at baseline.
  model_head_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                             data=data_reg %>% filter(GRUD_baseline==1),
                             cluster=~SIRSUP_baseline)
  model_nhead_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                              data=data_reg %>% filter(GRUD_baseline!=1),
                              cluster=~SIRSUP_baseline)
  # Column 3: head in both baseline and 2011 -- persistently high-status individuals.
  model_head_always<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                           data=data_reg %>% filter(GRUD_baseline==1 & GRUD_2011==1),
                           cluster=~SIRSUP_baseline)
  # Columns 4-7: age cohorts (10-20, 21-30, 31-40, 41+) at baseline.
  model_young_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                              data=data_reg %>% filter(AGE_baseline %in% 10:20),
                              cluster=~SIRSUP_baseline)
  model_20_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                           data=data_reg %>% filter(AGE_baseline %in% 21:30),
                           cluster=~SIRSUP_baseline)
  model_30_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                           data=data_reg %>% filter(AGE_baseline %in% 31:40),
                           cluster=~SIRSUP_baseline)
  model_old_baseline<-feols(ROMA_2011~1|SIRSUP_baseline^AA_baseline+census|years_2011~years_baseline,
                            data=data_reg %>% filter(AGE_baseline>=41),
                            cluster=~SIRSUP_baseline)
  
  ## Assemble the three panels into a single tabular ----
  # Panel 1 (8 data columns).
  f1 <- fstat_row(model_sex_92_iv_m, model_sex_92_iv_f,
                  model_sex_92_iv_m_parent, model_sex_92_iv_f_parent,
                  model_mom_educ_92_iv_h, model_mom_educ_92_iv_l,
                  model_pop_educ_92_iv_h, model_pop_educ_92_iv_l)
  body1 <- ms_body(list("m1"=model_sex_92_iv_m,
                        "m2"=model_sex_92_iv_f,
                        "m3"=model_sex_92_iv_m_parent,
                        "m4"=model_sex_92_iv_f_parent,
                        "m5"=model_mom_educ_92_iv_h,
                        "m6"=model_mom_educ_92_iv_l,
                        "m7"=model_pop_educ_92_iv_h,
                        "m8"=model_pop_educ_92_iv_l),
                   f1, variables, "lcccccccc")
  
  # Panel 2 (6 data columns; the Roma-spouse split is omitted -- covered by the
  # marriage tables). The 6 columns are right-aligned inside the 8-column master
  # tabular, occupying data columns 3-8.
  f2 <- fstat_row(model_migration_92_iv_m, model_migration_92_iv_nm,
                  model_lang_92_iv_romani, model_lang_92_iv_romanian,
                  model_rel_92_iv_trad, model_rel_92_iv_neo)
  body2 <- ms_body(list("m1"=model_migration_92_iv_m,
                        "m2"=model_migration_92_iv_nm,
                        "m3"=model_lang_92_iv_romani,
                        "m4"=model_lang_92_iv_romanian,
                        "m5"=model_rel_92_iv_trad,
                        "m6"=model_rel_92_iv_neo),
                   f2, variables, "lcccccc")
  # Pad each row with two leading empty data cells (cols 1-2) so the 6 columns
  # sit in data columns 3-8 (right-aligned within the 8-column master tabular).
  body2 <- sub("&", "& & &", body2, fixed = TRUE)
  
  # Panel 3 (7 models shown in data columns 2-8): pad each row with a leading
  # empty data cell so it aligns with the 8-column master tabular.
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
  body3 <- sub("&", "& &", body3, fixed = TRUE)
  
  # LaTeX column headers for each panel. Manually constructed so each panel can
  # carry its own descriptive header inside the shared tabular environment.
  p1_header <- c(
    "& \\multicolumn{8}{c}{Panel 1: Gender and Parental Education}\\\\",
    "& \\multicolumn{8}{c}{\\textit{Dependent Variable: Reported Roma Ethnicity (2011)}}\\\\",
    "& \\multicolumn{2}{c}{Sex} & \\multicolumn{2}{c}{Sex \\& Parental Educ.} & \\multicolumn{2}{c}{Mom Education} & \\multicolumn{2}{c}{Dad Education}\\\\",
    "& Male & Female & Male & Female & $\\geq 8$ Yrs & $<8$ Yrs & $\\geq 8$ Yrs & $<8$ Yrs \\\\",
    "& (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) \\\\")
  
  # Panel 2 header: 6 columns right-aligned in data columns 3-8 via two leading
  # empty cells.
  p2_header <- c(
    "& & & \\multicolumn{6}{c}{Panel 2: Other Individual and Family Characteristics}\\\\",
    "& & & \\multicolumn{6}{c}{\\textit{Dependent Variable: Reported Roma Ethnicity (2011)}}\\\\",
    "& & & \\multicolumn{2}{c}{Migrant} & \\multicolumn{2}{c}{Native Language} & \\multicolumn{2}{c}{Orthodox Religion}\\\\",
    "& & & Yes & No & Romani & Non-Romani & Yes & No \\\\",
    "& & & (1) & (2) & (3) & (4) & (5) & (6) \\\\")
  
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
  table2
}


# --- Town-Size Models (Appendix Table A08) ---

## Town size: 1992-2011 and 2002-2011 samples ----
# Six models: three town-size bins x two cohorts.
# Tests whether the education effect differs by urban vs. rural context;
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


# --- Write Table 02 (1992-2011) ---

#write heterogeneity table----
setwd(wd_output)
# Table 02: main-text heterogeneity table on the 1992-2011 linked Roma sample.
# The 2002-2011 counterpart (Table A15) is written by its own script.
writeLines(build_het_table(data_reg_92), "Table 02.tex")


# --- Appendix Table A08: Town Size ---

#Appendix: town size (6 columns)----
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

# Output: Table A08.tex -- appendix table of ethnic identification by town size.
setwd(wd_output)
writeLines(town_table, "Table A08.tex")
