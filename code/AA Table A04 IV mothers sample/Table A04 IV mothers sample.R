# =====================================================================
# Appendix Table A04 — IV ethnic-passing results on the mothers (VSN) sample
# Produces:  output/Table A04.tex
# Inputs:    data_1992_2011_roma_unique.csv, data_2002_2011_roma_unique.csv
#            (Roma linked census panels), data_2011_clean_births.csv (2011 census
#            + birth records, used to bridge children to their mothers)
# Summary:   Robustness check restricting the passing regressions to mothers
#            identified via the VSN birth-record linkage (mothers bridged to the
#            1992-2011 and 2002-2011 panels through their children). Estimates OLS
#            and IV (2011 schooling instrumented by baseline schooling) of 2011
#            reported-Roma status on schooling, with locality x birth-year fixed
#            effects and locality-clustered SEs. Assembled into a LaTeX table.
# =====================================================================

# ---- Load 1992->2011 Roma panel and flag census-code Roma ----
setwd(wd_data_linked)
data_1992_2011_r<-fread('data_1992_2011_roma_unique.csv')
data_1992_2011_r<-data_1992_2011_r %>%
  mutate(ROMA_1992_LIM=LIM_1992==12,
         ROMA_2011_LIM=LIM_2011==1201)

# ---- Load 2011 census + birth records and keep children born 2002-2011 ----
# read_sample()/read_data() are project helpers that load and stack census source files.
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

# ---- Bridge children to mothers in the 1992->2011 panel ----
# Join each child to its mother's panel record: the child's mother birth-certificate
# id (id11_MOM_BC) matches the mother's own person id (id11) in the linked panel.
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


# ---- Load 2002->2011 Roma panel and bridge the same children to mothers ----
setwd(wd_data_linked)
data_2002_2011_r<-fread('data_2002_2011_roma_unique.csv')
data_2002_2011_r<-data_2002_2011_r %>%
  mutate(ROMA_2002_LIM=LIM_2002==12,
         ROMA_2011_LIM=LIM_2011==1201)


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





# ---- Passing regressions on the mothers sample ----
# Robustness: estimate ethnic passing (2011 reported-Roma on schooling) on the
# mother sub-samples. For each baseline (2002, then 1992) we run the IV (2011
# schooling instrumented by baseline schooling) and the OLS counterpart, on
# baseline-Roma mothers, with locality x birth-year FE and locality-clustered SEs.
# moms from VSN: 02-11 passing -- IV
model_iv_baseline_11_02_momz_iv<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T) %>% filter(!is.na(years_2002)),
                                       ROMA_2011~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                                       cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02_momz_iv)

# OLS counterpart (2011 reported-Roma on 2011 schooling)
model_ols_baseline_11_02_momz<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T) %>% filter(!is.na(years_2002)),
                                    ROMA_2011~years_2011|SIRSUP_2002^AA_2002,
                                    cluster=~SIRSUP_2002)
summary(model_ols_baseline_11_02_momz)

# moms from VSN: 92-11 passing -- IV (2011 schooling instrumented by 1992 schooling)
model_iv_baseline_11_92_momz_iv<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T) %>% filter(!is.na(years_1992)),
                                       ROMA_2011~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                       cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_momz_iv)

# OLS counterpart (2011 reported-Roma on 2011 schooling)
model_ols_baseline_11_92_momz<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T) %>% filter(!is.na(years_1992)),
                                    ROMA_2011~years_2011|SIRSUP_1992^AA_1992,
                                    cluster=~SIRSUP_1992)
summary(model_ols_baseline_11_92_momz)






# ---- Assemble the LaTeX table ----
# Display labels for the schooling regressors and a thousands-separator formatter.
variables<-c('years_2011'='Schooling Yrs',
             'years_MOM'='Schooling Yrs',
             'years_2002'='Schooling Yrs',
             'years_1992'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_MOM_2002'="Schooling Yrs Mom (Baseline)",
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
              a=fitstat(model_iv_baseline_11_92_momz_iv, "ivf")$ivf1$stat,
              ols2="",
              b=fitstat(model_iv_baseline_11_02_momz_iv, "ivf")$ivf1$stat
)
f<-f_big(f)


#IV table
setwd(wd_output)
tab <- modelsummary(list(
  "OLS '92-'11"=model_ols_baseline_11_92_momz,
  # "OLS Birth-'11 Mom"=model_ols_bc_11_MOM,
  "IV '92-'11"=model_iv_baseline_11_92_momz_iv,
  # "IV '02-Birth"=model_iv_baseline_bc_02,
  "OLS '02-'11"=model_ols_baseline_11_02_momz,
  "IV '02-'11"=model_iv_baseline_11_02_momz_iv
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
align = "lcccc",
escape=F)

lines <- strsplit(tab, "\n")[[1]]

# The Sample/Outcome block: each cell stacks 2-3 lines via \makecell
sample_outcome <- c(
  "\\midrule",
  "Sample & '92-VSN-'11 & '92-VSN-'11 & '02-VSN-'11 & '02-VSN-'11\\\\",
  "\\multirow{2}{*}{\\centering Outcome} & Mothers & Mothers & Mothers & Mothers\\\\",
  "& Census '11 & Census '11 & Census '11 & Census '11\\\\"
)



# insert right before the \midrule that precedes N (the GOF block)
# find the midrule that comes just before the "N &" row
n_row   <- grep("^N &", lines)[1]
mid_idx <- max(grep("midrule", lines[seq_len(n_row)]))   # last midrule before N

# ---- Save table and append column numbers ----
lines <- append(lines, sample_outcome, after = mid_idx - 1)
writeLines(lines, "Table A04.tex")

add_column_numbers("Table A04.tex")


