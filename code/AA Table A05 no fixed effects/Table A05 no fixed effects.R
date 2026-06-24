# =====================================================================
# Appendix Table A05 — Ethnic-passing results WITHOUT fixed effects
# Produces:  output/Table A05.tex
# Inputs:    data_1992_2011_roma_unique.csv (1992->2011 Roma panel),
#            data_2011_clean_births.csv (2011 census + birth records, to bridge
#            children to mothers)
# Summary:   Robustness re-estimation of the passing regressions dropping the
#            locality x birth-year fixed effects used in the main tables: 2011
#            reported-Roma (or birth-certificate Roma) regressed on schooling via
#            OLS and IV (2011 schooling instrumented by 1992 schooling), SEs
#            clustered by locality. Five columns spanning individual, mother, and
#            child outcomes are assembled into a LaTeX table.
# =====================================================================

# ---- Load 1992->2011 Roma panel and flag census-code Roma ----
setwd(wd_data_linked)
data_1992_2011_r<-fread('data_1992_2011_roma_unique.csv')
data_1992_2011_r<-data_1992_2011_r %>%
  mutate(ROMA_1992_LIM=LIM_1992==12,
         ROMA_2011_LIM=LIM_2011==1201) 

# ---- Load 2011 census + birth records and keep children born 2002-2011 ----
# ROMA_bc flags Roma on the birth certificate (nat==12); ROMA_lim flags census-code Roma.
setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
data_2011<-read_sample(filename)
data_2011<-data_2011 %>%
  select(id11,id11_MOM,id11_MOM_BC,nat,LIM,AA,years,SIRUTA,SIRSUP,ROMA,ROMA_MOM,HHID,scoala_m,years_MOM,years_POP,SEX,category,source)
data_2011<-read_data(filename,data_2011) %>%
  mutate(ROMA_bc=nat==12) %>%
  mutate(ROMA_lim=LIM==1201)
data_2011_kids<-data_2011 %>%
  filter(AA %in% 2002:2011)

# ---- Bridge children to their mothers in the 1992->2011 panel ----
# child's mother birth-certificate id (id11_MOM_BC) matches the mother's person id (id11).
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



# ---- Passing regressions, NO fixed effects ----
# Same outcomes as the main tables but with the SIRSUP x birth-year FE removed;
# SEs remain clustered by locality. (1) OLS, individual 2011 reported-Roma on
# 2011 schooling, 1992->2011 panel.
model_ols_baseline_11_92<-feols(data=data_1992_2011_r %>% filter(!is.na(years_1992)),
                                ROMA_2011~years_2011,
                                cluster=~SIRSUP_1992)
summary(model_ols_baseline_11_92)
# (2) IV: 2011 reported-Roma, 2011 schooling instrumented by 1992 schooling.
model_iv_baseline_11_92<-feols(data=data_1992_2011_r,
                               ROMA_2011~1|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92)
#baseline mom-bc
model_iv_baseline_bc_92<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                               ROMA_bc~1|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_bc_92)



#bc-11 mom
model_ols_bc_11<-feols(data=data_1992_2011_mom %>% filter(ROMA_bc==T),
                               ROMA_2011~years_2011,
                               cluster=~SIRSUP_2011)
summary(model_ols_bc_11)

# OLS, mother outcome: among children Roma on the birth certificate, regress the
# mother's 2011 reported-Roma status on the mother's schooling.
model_ols_bc_11_MOM<-feols(data=data_2011 %>% filter(ROMA_bc==T),
                      ROMA_MOM~years_MOM,
                      cluster=~SIRSUP)
summary(model_ols_bc_11_MOM)

#baseline mom-bc
model_iv_baseline_11_92_child<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA_bc~1|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child)

# IV, child 2011-census outcome: child's 2011 reported-Roma (ROMA) on mother's
# schooling, mother's 2011 schooling instrumented by her 1992 schooling.
model_iv_baseline_11_92_child2<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA~1|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child2)


# IV, child VSN outcome: moms from VSN birth records, 92-11 passing; mother's 2011
# schooling instrumented by her 1992 schooling.
model_iv_baseline_11_92_momz_iv<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
                                     ROMA_2011~1|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_momz_iv)

# #iv
# model_iv_baseline_11_92_momz<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T),
#                                     ROMA_2011~years_2011|SIRSUP_1992^AA_1992,
#                                     cluster=~SIRSUP_1992)
# summary(model_iv_baseline_11_92_momz)





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
              b=fitstat(model_iv_baseline_11_92, "ivf")$ivf1$stat,
              ols="",
              # c=fitstat(model_iv_baseline_bc_92, "ivf")$ivf1$stat,
              d=fitstat(model_iv_baseline_11_92_child, "ivf")$ivf1$stat,
              e=fitstat(model_iv_baseline_11_92_child2, "ivf")$ivf1$stat
)
f<-f_big(f)


#IV table
setwd(wd_output)
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

# ---- Save table and append column numbers ----
lines <- append(lines, sample_outcome, after = mid_idx - 1)
writeLines(lines, "Table A05.tex")

add_column_numbers("Table A05.tex")

