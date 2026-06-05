# Produces Table A.6: Changes in Ethnic Identification by Education (2002–2011)

# --- Section 1: Load and Prepare Linked 2002–2011 Roma Sample ---

#load data
setwd(wd_data_linked)
# data_2002_2011_r: panel of Roma-identified individuals linked across the 2002 and 2011 censuses
data_2002_2011_r<-fread('data_2002_2011_roma_unique.csv')
# Construct language-based Roma flags for 2002 and 2011 to enable alternative-definition robustness checks
data_2002_2011_r<-data_2002_2011_r %>%
  mutate(ROMA_2002_LIM=LIM_2002==12,
         ROMA_2011_LIM=LIM_2011==1201)

# --- Section 2: Load 2011 Census and Restrict to Children Born 2002–2011 ---

setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
# Read a representative sample to determine the column subset needed
data_2011<-read_sample(filename)
# Retain only variables needed for mother–child linkage and outcome construction
data_2011<-data_2011 %>%
  select(id11,id11_MOM,id11_MOM_BC,nat,LIM,AA,years,SIRUTA,SIRSUP,ROMA,ROMA_MOM,HHID,scoala_m,years_MOM,years_POP,SEX,category,source)
# read_data() loads the full file, filtering to the columns selected above
data_2011<-read_data(filename,data_2011) %>%
  # ROMA_bc: Roma identification based on birth certificate / civil register (nat == 12), used as child outcome
  mutate(ROMA_bc=nat==12) %>%
  # ROMA_lim: language-based Roma flag (Romani mother tongue) in 2011
  mutate(ROMA_lim=LIM==1201)
# Restrict to children born during the intercensal period (2002–2011) to study intergenerational transmission
data_2011_kids<-data_2011 %>%
  filter(AA %in% 2002:2011)

# --- Section 3: Merge Mother's Linked Record onto Children ---

# Join children born 2002–2011 to their mother's linked 2002–2011 record via the mother's 2011 person ID
# suffix "_CHILD" / "_MOM" disambiguates variables that appear in both child and mother datasets
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



# --- Section 4: Core Regressions (Adult Panel, 2002–2011) ---

# OLS baseline: effect of 2011 schooling years on 2011 Roma self-identification
# Fixed effects: SIRSUP_2002 x AA_2002 (locality-by-birth-cohort cells from the baseline census)
# Restricts to individuals with non-missing 2002 schooling (for comparability with IV sample)
#baseline mom-11 mom
model_ols_baseline_11_02<-feols(data=data_2002_2011_r %>% filter(!is.na(years_2002)),
                                ROMA_2011~years_2011|SIRSUP_2002^AA_2002,
                                cluster=~SIRSUP_2002)
summary(model_ols_baseline_11_02)
# IV baseline: instruments years_2011 (endogenous) with years_2002 (2002 education level)
# Identification: 1992/2002 schooling affects 2011 ethnicity only through 2011 schooling
#baseline mom-11 mom
model_iv_baseline_11_02<-feols(data=data_2002_2011_r,
                               ROMA_2011~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                               cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02)
# IV on alternative (birth-certificate) Roma outcome, restricted to mothers who were Roma in 2002
#baseline mom-bc
model_iv_baseline_bc_02<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T),
                               ROMA_bc~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                               cluster=~SIRSUP_2002)
summary(model_iv_baseline_bc_02)



# --- Section 5: Mother–Child Linkage Regressions ---

# OLS: effect of mother's 2011 education on her child's 2011 Roma census identification
# Sample: children born 2002–2011 whose mothers were Roma by birth certificate
#bc-11 mom
model_ols_bc_11<-feols(data=data_2002_2011_mom %>% filter(ROMA_bc==T),
                       ROMA_2011~years_2011|SIRSUP_2011^AA_2011,
                       cluster=~SIRSUP_2011)
summary(model_ols_bc_11)

# OLS: effect of mother's 2011 schooling on the mother's own 2011 Roma census flag (ROMA_MOM)
# Uses the full 2011 census file (not the linked panel) — cross-sectional, not panel
#bc-11 mom
model_ols_bc_11_MOM<-feols(data=data_2011 %>% filter(ROMA_bc==T),
                           ROMA_MOM~years_MOM|SIRSUP^AA,
                           cluster=~SIRSUP)
summary(model_ols_bc_11_MOM)

# IV: instruments mother's 2011 schooling with her 2002 schooling; outcome = child's birth-certificate Roma flag
# Restricted to mothers Roma-identified in 2002; captures intergenerational transmission via passing
#baseline mom-bc
model_iv_baseline_11_02_child<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T),
                                     ROMA_bc~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                                     cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02_child)

# IV: same as above but outcome is the child's 2011 census Roma self-identification (ROMA) rather than birth certificate
#baseline mom-2011 child
model_iv_baseline_11_02_child2<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T),
                                      ROMA~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                                      cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02_child2)


# --- Section 6: Robustness Checks (Mothers with Linked Records) ---

#robustness check
#moms from VSN: 02-11 passing
#ols
# IV: replicates core 2002–2011 passing regression restricted to mothers with matched records
# Tests whether the main IV result is driven by sample differences (mothers vs. full panel)
model_iv_baseline_11_02_momz_iv<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T),
                                       ROMA_2011~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                                       cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02_momz_iv)

#iv
# OLS counterpart of the above IV — compares OLS and IV estimates on the same mother subsample
model_iv_baseline_11_02_momz<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T),
                                    ROMA_2011~years_2011|SIRSUP_2002^AA_2002,
                                    cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02_momz)




# --- Section 7: Table Formatting Setup ---

######
# variables: maps internal regressor names to display labels used in the LaTeX table
variables<-c('years_2011'='Schooling Yrs',
             'years_MOM'='Schooling Yrs',
             'years_2002'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_MOM_2002'="Schooling Yrs Mom (Baseline)",
             'years_POP_2002'="Schooling Yrs Dad (Baseline)",
             "fit_years_MOM_CHILD"='Schooling Yrs (Endline)')
# f_big: formats large integers with comma separators for the N row in the table
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
options(modelsummary_format_numeric_latex = "plain")
# glance_custom.fixest: appends the dependent-variable mean as a GOF row by recovering fitted + residual values
glance_custom.fixest <- function(x, ...) {
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# f: first-stage F-statistics for IV columns; "" fills cells for OLS columns (no first stage)
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


# --- Section 8: Assemble and Export LaTeX Table ---

# Output Table A06.tex to the designated output directory
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

# Split the latex_tabular string into individual lines for surgical insertion of header rows
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

lines <- append(lines, sample_outcome, after = mid_idx - 1)
# Output: replication_final/output/Table A06.tex — Table A.6 in the paper appendix
writeLines(lines, "Table A06.tex")

# add_column_numbers() post-processes the .tex file to insert "(1)", "(2)", ... header row
add_column_numbers("Table A06.tex")


