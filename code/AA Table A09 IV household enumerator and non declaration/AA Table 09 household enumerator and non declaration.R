###############################################################################
# Script:  Table Different Specifications.R
# Purpose: Build the robustness table "Changes in Ethnic Identification by
#          Education -- Different Specifications (1992-2011)" (label tab:rob) as
#          a single LaTeX tabular. Combines the three specifications that were
#          previously produced by separate scripts:
#            - aa_02_iv_robustness_within_household.R  -> Household FE  (cols 1-2)
#            - aa_02_iv_robustness_enumerator.R        -> Enumerator FE (cols 3-4)
#            - aa_02_iv_non_declaration.R              -> Non-Declaration (cols 5-6)
#          Each specification reports OLS and IV on the 1992-2011 linked sample.
# Output:  - Table Different Specifications.tex
# Note:    IV instruments 2011 education with 1992 education. Household FE adds
#          HHID_1992; Enumerator FE adds MAPA (census-map) FE for 1992 and 2011;
#          Non-Declaration recodes undeclared 2011 ethnicity (ET==9999) as Roma.
###############################################################################

# --- Load Data ---
#load data----
setwd(wd_data_linked)
# data_1992_2011_r: linked individual records for Roma-identified persons in 1992,
# matched to their 2011 census record; one row per successfully linked individual.
data_1992_2011_r<-fread('data_1992_2011_roma_unique.csv')

# Non-declaration specification: recode undeclared 2011 ethnicity as Roma.
# ET==9999 codes persons who refused to declare ethnicity in 2011; treating them
# as Roma is a conservative upper bound on ethnic persistence (lower bound on passing).
data_nd<-data_1992_2011_r %>%
  mutate(ROMA_2011=ET_2011 %in% 1200:1299 | ET_2011==9999)


# --- Regressions ---
#regressions----
## Household FE (cols 1-2) ----
# HHID_1992 FE absorbs unobserved household-level factors (e.g., parental attitude
# toward ethnicity) that could confound the education-passing relationship.
# Filtered to non-missing years_1992 for OLS comparability; IV uses full sample.
model_ols_hh<-feols(ROMA_2011~years_2011|SIRSUP_1992^AA_1992+HHID_1992,
                    data=data_1992_2011_r %>% filter(!is.na(years_1992)),
                    cluster=~SIRSUP_1992)
# IV: years_1992 instruments for years_2011; SIRSUP_1992^AA_1992 = locality-by-birth-cohort FE.
# Clustering at the locality level to allow within-locality correlation of errors.
model_iv_hh<-feols(ROMA_2011~1|SIRSUP_1992^AA_1992+HHID_1992|years_2011~years_1992,
                   data=data_1992_2011_r,
                   cluster=~SIRSUP_1992)

## Enumerator FE (cols 3-4) ----
# MAPA_1992 and MAPA_2011 are census-map (enumerator district) codes for 1992 and 2011.
# Adding both absorbs systematic enumerator-level variation in ethnic recording in
# each census year, addressing the concern that enumerators differentially classify
# ethnicity for higher-educated individuals.
model_ols_enum<-feols(ROMA_2011~years_2011|SIRSUP_1992^AA_1992+MAPA_1992+MAPA_2011,
                      data=data_1992_2011_r %>% filter(!is.na(years_1992)),
                      cluster=~SIRSUP_1992)
model_iv_enum<-feols(ROMA_2011~1|SIRSUP_1992^AA_1992+MAPA_1992+MAPA_2011|years_2011~years_1992,
                     data=data_1992_2011_r,
                     cluster=~SIRSUP_1992)

## Non-Declaration (cols 5-6) ----
# Uses data_nd where ET_2011==9999 (undeclared) is recoded to Roma; baseline spec FE only.
model_ols_nd<-feols(ROMA_2011~years_2011|SIRSUP_1992^AA_1992,
                    data=data_nd %>% filter(!is.na(years_1992)),
                    cluster=~SIRSUP_1992)
model_iv_nd<-feols(ROMA_2011~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                   data=data_nd,
                   cluster=~SIRSUP_1992)


# --- Table Formatting Setup ---
#table formatting setup----
# Unified coefficient label: OLS uses years_2011 directly; IV uses fit_years_2011
# (the first-stage fitted value); years_1992 appears if reduced-form is ever added.
variables<-c('years_2011'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_1992'='Schooling Yrs')
# f_big: number formatter with thousands separator, used for N and F-stat cells.
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
options(modelsummary_format_numeric_latex = "plain")
# glance_custom.fixest: adds DV Mean as a GOF row by recovering fitted + residuals.
glance_custom.fixest <- function(x, ...) {
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# F-stat row: blank for OLS columns, first-stage F for IV columns.
f<-data.frame(n="F-stat",
              c1="",
              c2=fitstat(model_iv_hh,   "ivf")$ivf1$stat,
              c3="",
              c4=fitstat(model_iv_enum, "ivf")$ivf1$stat,
              c5="",
              c6=fitstat(model_iv_nd,   "ivf")$ivf1$stat)
f<-f_big(f)

# Run modelsummary -> latex_tabular and return only the body: the coefficient
# rows, the internal \midrule, and the GOF rows (N / R2 / DV Mean / F-stat).
# This helper is used so the outer shell can supply its own multi-column header.
ms_body <- function(models, fstat, coef_rename, align) {
  tab <- modelsummary(models,
                      estimate  = "{estimate}{stars}",
                      statistic = "std.error",
                      stars = c('$^{*}$'=0.1,'$^{**}$'=0.05,'$^{***}$'=0.01),
                      gof_map = list(list("raw"="nobs",      "clean"="N",      "fmt"=f_big),
                                     list("raw"="r.squared", "clean"="R$^2$",  "fmt"="%.2f"),
                                     list("raw"="Mean of DV","clean"="DV Mean","fmt"="%.2f")),
                      metrics = "R2",
                      add_rows = fstat,
                      output  = "latex_tabular",
                      coef_rename = coef_rename,
                      align = align,
                      escape = FALSE)
  if (!is.character(tab)) tab <- paste(as.character(tab), collapse = "\n")
  lines <- strsplit(tab, "\n")[[1]]

  # Extract only the tabular body, stripping the \begin/\end wrapper lines.
  beg <- grep("\\\\begin\\{tabular\\}", lines)[1]
  end <- grep("\\\\end\\{tabular\\}",   lines)[1]
  body <- lines[(beg + 1):(end - 1)]      # header + coefs + gof
  body <- body[-1]                        # drop modelsummary header row

  # insert \midrule before the GOF block (the "N &" row)
  n_row <- grep("^N &", body)[1]
  if (!is.na(n_row)) body <- append(body, "\\midrule", after = n_row - 1)

  body
}

# --- Build and Write Table ---
#build table----
# Pass all six models together so modelsummary aligns columns consistently.
body <- ms_body(list("m1"=model_ols_hh,
                     "m2"=model_iv_hh,
                     "m3"=model_ols_enum,
                     "m4"=model_iv_enum,
                     "m5"=model_ols_nd,
                     "m6"=model_iv_nd),
                f, variables, "lcccccc")

# Wrap the modelsummary body with a custom multi-column header; extra @{} spacing
# visually separates the three specification pairs (cols 1-2, 3-4, 5-6).
out <- c(
  "\\begin{tabular}{lcc@{\\hspace{12pt}}cc@{\\hspace{12pt}}cc}",
  "\\toprule",
  "& \\multicolumn{2}{c@{\\hspace{12pt}}}{Household FE} & \\multicolumn{2}{c@{\\hspace{12pt}}}{Enumerator FE} & \\multicolumn{2}{c}{Non-Declaration} \\\\",
  "& OLS & IV & OLS & IV & OLS & IV \\\\",
  "& (1) & (2) & (3) & (4) & (5) & (6) \\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}")

# Output: Table A09.tex saved to wd_output
setwd(wd_output)
writeLines(out, "Table A09.tex")
