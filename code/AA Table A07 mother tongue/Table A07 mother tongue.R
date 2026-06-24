# =====================================================================
# Appendix Table A07 — Ethnic passing using the mother-tongue (LIM) definition of Roma
# Produces:  output/Table A07.tex
# Inputs:    data_1992_2011_roma_unique.csv (1992->2011 Roma panel),
#            data_2002_2011_roma_unique.csv (2002->2011 Roma panel),
#            data_2011_clean_births.csv (2011 census + birth records)
# Summary:   Re-runs the passing analysis but defines Roma by declared mother
#            tongue (the census LIM code: LIM==12 in 1992, LIM==1201 in 2011)
#            rather than self-reported ethnicity. Estimates OLS and IV (2011
#            schooling instrumented by 1992 schooling) of the 2011 mother-tongue
#            Roma indicator on schooling, with locality x birth-year FE and
#            locality-clustered SEs. The two displayed columns (OLS, IV on the
#            1992->2011 panel) are written to a LaTeX table.
# =====================================================================

# ---- Load 1992->2011 Roma panel and build mother-tongue (LIM) Roma flags ----
setwd(wd_data_linked)
data_1992_2011_r<-fread('data_1992_2011_roma_unique.csv')
data_1992_2011_r<-data_1992_2011_r %>%
  mutate(ROMA_1992_LIM=LIM_1992==12,
         ROMA_2011_LIM=LIM_2011==1201)

setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
data_2011<-read_sample(filename)
data_2011<-data_2011 %>%
  select(id11,id11_MOM,id11_MOM_BC,nat,LIM,LIM_MOM,AA,years,SIRUTA,SIRSUP,ROMA,ROMA_MOM,HHID,scoala_m,years_MOM,years_POP,SEX,category,source)
data_2011<-read_data(filename,data_2011) %>%
  mutate(ROMA_bc=nat==12) %>%
  mutate(ROMA_LIM=LIM==1201) %>%
  mutate(ROMA_MOM_LIM=LIM==1201)
data_2011_kids<-data_2011 %>%
  filter(AA %in% 2002:2011)

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



# ---- Passing regressions, mother-tongue (LIM) Roma outcomes ----
# Baseline-Roma (LIM) sample; locality x birth-year FE; locality-clustered SEs.
# OLS: 2011 mother-tongue Roma on 2011 schooling.
model_ols_baseline_11_92<-feols(data=data_1992_2011_r %>%
                                  filter(!is.na(years_1992)) %>%
                                  filter(ROMA_1992_LIM==T),
                                ROMA_2011_LIM~years_2011|SIRSUP_1992^AA_1992,
                                cluster=~SIRSUP_1992)
summary(model_ols_baseline_11_92)
#baseline mom-11 mom
model_iv_baseline_11_92<-feols(data=data_1992_2011_r %>%
                                 filter(ROMA_1992_LIM==T),
                               ROMA_2011_LIM~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92)
#baseline mom-bc
model_iv_baseline_bc_92<-feols(data=data_1992_2011_mom %>% 
                                 filter(ROMA_1992_LIM==T),
                               ROMA_bc~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_bc_92)



#bc-11 mom
model_ols_bc_11<-feols(data=data_1992_2011_mom %>% 
                         filter(ROMA_bc==T),
                               ROMA_2011_LIM~years_2011|SIRSUP_2011^AA_2011,
                               cluster=~SIRSUP_2011)
summary(model_ols_bc_11)

#bc-11 mom
model_ols_bc_11_MOM<-feols(data=data_2011 %>% filter(ROMA_bc==T),
                      ROMA_MOM_LIM~years_MOM|SIRSUP^AA,
                      cluster=~SIRSUP)
summary(model_ols_bc_11_MOM)

#baseline mom-bc
model_iv_baseline_11_92_child<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992_LIM==T),
                                     ROMA_bc~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child)

#baseline mom-2011 child
model_iv_baseline_11_92_child2<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992_LIM==T),
                                     ROMA_LIM~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child2)




# ---- Assemble the LaTeX table ----
# Display labels for the schooling regressors and a thousands-separator formatter.
variables<-c('years_2011'='Schooling Yrs',
             'years_MOM'='Schooling Yrs',
             'years_1992'='Schooling Yrs',
             'years_2002'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_MOM_1992'="Schooling Yrs Mom (Baseline)",
             'years_POP_1992'="Schooling Yrs Dad (Baseline)",
             "fit_years_MOM_CHILD"='Schooling Yrs (Endline)')
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
options(modelsummary_format_numeric_latex = "plain")
glance_custom.fixest <- function(x, ...) {
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

f<-data.frame(n="F-stat",
              ols="",
              # ols2="",
              b=fitstat(model_iv_baseline_11_92, "ivf")$ivf1$stat
              # c="",
              # d=fitstat(model_iv_baseline_11_92_child, "ivf")$ivf1$stat,
              # e=fitstat(model_iv_baseline_11_92_child2, "ivf")$ivf1$stat
)
f<-f_big(f)


#IV table
setwd(wd_output)
tab <- modelsummary(list(
  "OLS '92-'11"=model_ols_baseline_11_92,
  # "OLS Birth-'11 Mom"=model_ols_bc_11_MOM,
  "IV '92-'11"=model_iv_baseline_11_92
  # "OLS 'VSN-'11"  = model_ols_bc_11_MOM,
  # "IV '92-Birth"=model_iv_baseline_bc_92,
  # "IV '92-'11"=model_iv_baseline_11_92_child,
  # "IV '92-'11"=model_iv_baseline_11_92_child2
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
align = "lcc",
escape=F)

lines <- strsplit(tab, "\n")[[1]]

# The Sample/Outcome block: each cell stacks 2-3 lines via \makecell
sample_outcome <- c(
  "\\midrule",
  "Sample & '92-'11 & '92-'11  \\\\",
  "Outcome & \\makecell{Individual\\\\Census '11} & \\makecell{Individual\\\\Census '11}  \\\\"
)

# insert right before the \midrule that precedes N (the GOF block)
# find the midrule that comes just before the "N &" row
n_row   <- grep("^N &", lines)[1]
mid_idx <- max(grep("midrule", lines[seq_len(n_row)]))   # last midrule before N

# ---- Save table and append column numbers ----
lines <- append(lines, sample_outcome, after = mid_idx - 1)
writeLines(lines, "Table A07.tex")

add_column_numbers("Table A07.tex")


