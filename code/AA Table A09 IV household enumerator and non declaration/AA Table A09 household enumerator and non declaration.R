# =====================================================================
# Appendix Table A09 — Ethnic passing under alternative specifications
# Produces:  output/Table A09.tex
# Inputs:    data_1992_2011_roma_unique.csv (1992->2011 Roma panel)
# Summary:   Robustness table "Changes in Ethnic Identification by Education --
#            Different Specifications (1992-2011)" (label tab:rob), a single
#            LaTeX tabular combining three specifications, each reporting OLS and
#            IV on the 1992->2011 linked sample (2011 schooling instrumented by
#            1992 schooling):
#              - Household FE     (cols 1-2): adds HHID_1992 FE
#              - Enumerator FE    (cols 3-4): adds MAPA (census-map) FE for 1992
#                                             and 2011
#              - Non-Declaration  (cols 5-6): recodes undeclared 2011 ethnicity
#                                             (ET==9999) as Roma
# =====================================================================

#load data----
setwd(wd_data_linked)
data_1992_2011_r<-fread('data_1992_2011_roma_unique.csv')

# Non-declaration specification: recode undeclared 2011 ethnicity as Roma.
data_nd<-data_1992_2011_r %>%
  mutate(ROMA_2011=ET_2011 %in% 1200:1299 | ET_2011==9999)


#regressions----
## Household FE (cols 1-2) ----
model_ols_hh<-feols(ROMA_2011~years_2011|SIRSUP_1992^AA_1992+HHID_1992,
                    data=data_1992_2011_r %>% filter(!is.na(years_1992)),
                    cluster=~SIRSUP_1992)
model_iv_hh<-feols(ROMA_2011~1|SIRSUP_1992^AA_1992+HHID_1992|years_2011~years_1992,
                   data=data_1992_2011_r,
                   cluster=~SIRSUP_1992)

## Enumerator FE (cols 3-4) ----
model_ols_enum<-feols(ROMA_2011~years_2011|SIRSUP_1992^AA_1992+MAPA_1992+MAPA_2011,
                      data=data_1992_2011_r %>% filter(!is.na(years_1992)),
                      cluster=~SIRSUP_1992)
model_iv_enum<-feols(ROMA_2011~1|SIRSUP_1992^AA_1992+MAPA_1992+MAPA_2011|years_2011~years_1992,
                     data=data_1992_2011_r,
                     cluster=~SIRSUP_1992)

## Non-Declaration (cols 5-6) ----
model_ols_nd<-feols(ROMA_2011~years_2011|SIRSUP_1992^AA_1992,
                    data=data_nd %>% filter(!is.na(years_1992)),
                    cluster=~SIRSUP_1992)
model_iv_nd<-feols(ROMA_2011~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                   data=data_nd,
                   cluster=~SIRSUP_1992)


#table formatting setup----
variables<-c('years_2011'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_1992'='Schooling Yrs')
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
options(modelsummary_format_numeric_latex = "plain")
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
  
  beg <- grep("\\\\begin\\{tabular\\}", lines)[1]
  end <- grep("\\\\end\\{tabular\\}",   lines)[1]
  body <- lines[(beg + 1):(end - 1)]      # header + coefs + gof
  body <- body[-1]                        # drop modelsummary header row
  
  # insert \midrule before the GOF block (the "N &" row)
  n_row <- grep("^N &", body)[1]
  if (!is.na(n_row)) body <- append(body, "\\midrule", after = n_row - 1)
  
  body
}

#build table----
body <- ms_body(list("m1"=model_ols_hh,
                     "m2"=model_iv_hh,
                     "m3"=model_ols_enum,
                     "m4"=model_iv_enum,
                     "m5"=model_ols_nd,
                     "m6"=model_iv_nd),
                f, variables, "lcccccc")

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

setwd(wd_output)
writeLines(out, "Table A09.tex")
