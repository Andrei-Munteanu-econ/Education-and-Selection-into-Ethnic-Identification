# =====================================================================
# Appendix Table A06 — Ethnic-passing results on the 2002->2011 linked sample
# Produces:  output/Table A06.tex
# Inputs:    data_2002_2011_roma_unique.csv (2002->2011 Roma panel),
#            data_2011_clean_births.csv (2011 census + birth records, to bridge
#            children to mothers)
# Summary:   Replicates the main passing analysis using 2002 (rather than 1992)
#            as the baseline: 2011 reported-Roma (or birth-certificate Roma)
#            regressed on schooling via OLS and IV (2011 schooling instrumented
#            by 2002 schooling), with locality x birth-year fixed effects and
#            locality-clustered SEs, across individual/mother/child outcomes.
#            Five columns are assembled into a LaTeX table.
# =====================================================================

# ---- Load 2002->2011 Roma panel and flag census-code Roma ----
setwd(wd_data_linked)
data_2002_2011_r<-fread('data_2002_2011_roma_unique.csv')
data_2002_2011_r<-data_2002_2011_r %>%
  mutate(ROMA_2002_LIM=LIM_2002==12,
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

# ---- Bridge children to their mothers in the 2002->2011 panel ----
# child's mother birth-certificate id (id11_MOM_BC) matches the mother's person id (id11).
data_2002_2011_mom<-data_2011_kids %>%
  inner_join(data_2002_2011_r %>%
               select(id11,years_2002,years_2011,
                      ROMA_2002,ROMA_2011,
                      ROMA_2002_LIM,ROMA_2011_LIM,
                      AA_2011,SEX_2011,
                      years_MOM_2011,years_POP_2011,HHID_2002,HHID_2011,years_MOM_2002,years_POP_2002,
                      SIRSUP_2011,SIRSUP_2002,AA_2002,AA_2011,
                      ET_SPOUSE_2011,
                      category_2002,source_2002,category_2011,source_2011),
             by=c("id11_MOM_BC"="id11"),
             suffix=c("_CHILD","_MOM"))



# ---- Passing regressions, 2002 baseline ----
# All models absorb locality x birth-year FE (SIRSUP_2002^AA_2002) and cluster SEs
# by locality. (1) OLS: individual 2011 reported-Roma on 2011 schooling.
model_ols_baseline_11_02<-feols(data=data_2002_2011_r %>% filter(!is.na(years_2002)),
                                ROMA_2011~years_2011|SIRSUP_2002^AA_2002,
                                cluster=~SIRSUP_2002)
summary(model_ols_baseline_11_02)
# (2) IV: 2011 reported-Roma, 2011 schooling instrumented by 2002 schooling.
model_iv_baseline_11_02<-feols(data=data_2002_2011_r,
                               ROMA_2011~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                               cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02)
#baseline mom-bc
model_iv_baseline_bc_02<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T),
                               ROMA_bc~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                               cluster=~SIRSUP_2002)
summary(model_iv_baseline_bc_02)



#bc-11 mom
model_ols_bc_11<-feols(data=data_2002_2011_mom %>% filter(ROMA_bc==T),
                       ROMA_2011~years_2011|SIRSUP_2011^AA_2011,
                       cluster=~SIRSUP_2011)
summary(model_ols_bc_11)

# OLS, mother outcome: among children Roma on the birth certificate, regress the
# mother's 2011 reported-Roma status on the mother's schooling.
model_ols_bc_11_MOM<-feols(data=data_2011 %>% filter(ROMA_bc==T),
                           ROMA_MOM~years_MOM|SIRSUP^AA,
                           cluster=~SIRSUP)
summary(model_ols_bc_11_MOM)

#baseline mom-bc
model_iv_baseline_11_02_child<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T),
                                     ROMA_bc~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                                     cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02_child)

# IV, child 2011-census outcome: child's 2011 reported-Roma (ROMA) on mother's
# schooling, mother's 2011 schooling instrumented by her 2002 schooling.
model_iv_baseline_11_02_child2<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T),
                                      ROMA~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                                      cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02_child2)


# IV, child VSN-mother outcome: moms from VSN birth records, 02-11 passing;
# mother's 2011 schooling instrumented by her 2002 schooling.
model_iv_baseline_11_02_momz_iv<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T),
                                       ROMA_2011~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                                       cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02_momz_iv)

# OLS counterpart (2011 reported-Roma on 2011 schooling)
model_ols_baseline_11_02_momz<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T),
                                    ROMA_2011~years_2011|SIRSUP_2002^AA_2002,
                                    cluster=~SIRSUP_2002)
summary(model_ols_baseline_11_02_momz)





# ---- Assemble the LaTeX table ----
# Display labels for the schooling regressors and a thousands-separator formatter.
variables<-c('years_2011'='Schooling Yrs',
             'years_MOM'='Schooling Yrs',
             'years_2002'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_MOM_2002'="Schooling Yrs Mom (Baseline)",
             'years_POP_2002'="Schooling Yrs Dad (Baseline)",
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
              b=fitstat(model_iv_baseline_11_02, "ivf")$ivf1$stat,
              ols="",
              # c=fitstat(model_iv_baseline_bc_02, "ivf")$ivf1$stat,
              d=fitstat(model_iv_baseline_11_02_child, "ivf")$ivf1$stat,
              e=fitstat(model_iv_baseline_11_02_child2, "ivf")$ivf1$stat
)
f<-f_big(f)


#IV table




setwd(wd_output)
tab <- modelsummary(list(
  "OLS '02-'11"=model_ols_baseline_11_02,
  # "OLS Birth-'11 Mom"=model_ols_bc_11_MOM,
  "IV '02-'11"=model_iv_baseline_11_02,
  "OLS 'VSN-'11"=model_ols_bc_11_MOM,
  # "IV '02-Birth"=model_iv_baseline_bc_02,
  "IV '02-'11"=model_iv_baseline_11_02_child,
  "IV '02-'11"=model_iv_baseline_11_02_child2
  
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
  "Sample & '02-'11 & '02-'11 & VSN-'11 & '02-VSN-'11 & '02-VSN-'11 \\\\",
  "Outcome & \\makecell{Individual\\\\Census '11} & \\makecell{Individual\\\\Census '11} & \\makecell{Mother\\\\Census '11} & \\makecell{Child\\\\VSN} & \\makecell{Child\\\\Census '11} \\\\"
)

# insert right before the \midrule that precedes N (the GOF block)
# find the midrule that comes just before the "N &" row
n_row   <- grep("^N &", lines)[1]
mid_idx <- max(grep("midrule", lines[seq_len(n_row)]))   # last midrule before N

# ---- Save table and append column numbers ----
lines <- append(lines, sample_outcome, after = mid_idx - 1)
writeLines(lines, "Table A06.tex")

add_column_numbers("Table A06.tex")


