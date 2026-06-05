# Produces Table A.4: Changes in Ethnic Identification by Education (Mothers' Sample)

# --- Load and Prepare 1992-2011 Roma Panel ---

#load data
setwd(wd_data_linked)
# Linked panel of Roma individuals matched across the 1992 and 2011 censuses (unique matches only)
data_1992_2011_r<-fread('data_1992_2011_roma_unique.csv')
# Derive Roma flags under the language-based (LIM) definition: Roma mother tongue in 1992 (LIM_1992==12)
# and Roma mother tongue in 2011 (LIM_2011==1201 reflects the 2011 coding scheme)
data_1992_2011_r<-data_1992_2011_r %>%
  mutate(ROMA_1992_LIM=LIM_1992==12,
         ROMA_2011_LIM=LIM_2011==1201)

# --- Load 2011 Census Birth Records (children born 2002-2011) ---

setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
# read_sample() and read_data() are project-specific helpers that load and stack census source files
data_2011<-read_sample(filename)
# Retain only the columns needed to identify children, their mothers, and key demographics
data_2011<-data_2011 %>%
  select(id11,id11_MOM,id11_MOM_BC,nat,LIM,AA,years,SIRUTA,SIRSUP,ROMA,ROMA_MOM,HHID,scoala_m,years_MOM,years_POP,SEX,category,source)
data_2011<-read_data(filename,data_2011) %>%
  mutate(ROMA_bc=nat==12) %>%
  mutate(ROMA_lim=LIM==1201)
# Restrict to children born in the inter-census period 2002-2011; their mothers can be observed
# in the 2002-2011 linked panel, enabling an IV strategy based on the mother's earlier schooling
data_2011_kids<-data_2011 %>%
  filter(AA %in% 2002:2011)

# --- Build Mothers' Sample: 1992-2011 Panel ---

# Join children (born 2002-2011) to their mothers' records in the 1992-2011 linked panel.
# id11_MOM_BC (the child's mother birth-certificate ID in the 2011 census) matches id11 (the
# mother's own person ID in the linked panel), linking the mother across the two censuses.
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


# --- Load and Prepare 2002-2011 Roma Panel ---

#load data
setwd(wd_data_linked)
# Linked panel of Roma individuals matched across the 2002 and 2011 censuses (unique matches only)
data_2002_2011_r<-fread('data_2002_2011_roma_unique.csv')
# LIM_2002==12 uses the 2002 coding; LIM_2011==1201 uses the 2011 coding
data_2002_2011_r<-data_2002_2011_r %>%
  mutate(ROMA_2002_LIM=LIM_2002==12,
         ROMA_2011_LIM=LIM_2011==1201)


# --- Build Mothers' Sample: 2002-2011 Panel ---

# Same join logic as above but using the 2002-2011 panel; identifies mothers who were Roma in
# 2002 and can be tracked to 2011, providing a second (shorter) window for the IV
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




# --- Estimate IV and OLS Models ---

#robustness check
#moms from VSN: 02-11 passing
#ols
# IV specification for the 2002-2011 window: instruments years_2011 (endogenous, endline education)
# with years_2002 (baseline education); SIRSUP_2002^AA_2002 are locality-by-birth-cohort fixed effects
# Sample: mothers who were Roma in 2002 and have non-missing baseline schooling
model_iv_baseline_11_02_momz_iv<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T) %>% filter(!is.na(years_2002)),
                                       ROMA_2011~1|SIRSUP_2002^AA_2002|years_2011~years_2002,
                                       cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02_momz_iv)

#iv
# OLS counterpart for the 2002-2011 window (no instrumentation); used as a benchmark alongside IV
model_iv_baseline_11_02_momz<-feols(data=data_2002_2011_mom %>% filter(ROMA_2002==T) %>% filter(!is.na(years_2002)),
                                    ROMA_2011~years_2011|SIRSUP_2002^AA_2002,
                                    cluster=~SIRSUP_2002)
summary(model_iv_baseline_11_02_momz)

#robustness check
#moms from VSN: 92-11 passing
#ols
# IV specification for the longer 1992-2011 window: years_1992 instruments for years_2011;
# locality-by-birth-cohort FE absorb local schooling trends that might confound identification
model_iv_baseline_11_92_momz_iv<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T) %>% filter(!is.na(years_1992)),
                                       ROMA_2011~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                       cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_momz_iv)

#iv
# OLS counterpart for the 1992-2011 window
model_iv_baseline_11_92_momz<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992==T) %>% filter(!is.na(years_1992)),
                                    ROMA_2011~years_2011|SIRSUP_1992^AA_1992,
                                    cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_momz)




# --- Table Formatting Helpers ---

######
# Coefficient label map: harmonises display names across all four model columns
variables<-c('years_2011'='Schooling Yrs',
             'years_MOM'='Schooling Yrs',
             'years_2002'='Schooling Yrs',
             'years_1992'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_MOM_2002'="Schooling Yrs Mom (Baseline)",
             'years_POP_1992'="Schooling Yrs Dad (Baseline)",
             "fit_years_MOM_CHILD"='Schooling Yrs (Endline)')
# Helper to format large integers with comma separators for the N row
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
options(modelsummary_format_numeric_latex = "plain")
# Custom glance method: adds the mean of the dependent variable as a GOF row in modelsummary;
# reconstructs the DV as fitted + residual to be model-type agnostic
glance_custom.fixest <- function(x, ...) {
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# Extract first-stage F-statistics for the IV models to display in the table
# fitstat(..., "ivf") returns the Kleibergen-Paap/first-stage F; $ivf1$stat gives the scalar
f<-data.frame(n="F-stat",
              ols="",
              a=fitstat(model_iv_baseline_11_92_momz_iv, "ivf")$ivf1$stat,
              ols2="",
              b=fitstat(model_iv_baseline_11_02_momz_iv, "ivf")$ivf1$stat
)
f<-f_big(f)


# --- Render and Export Table A04 ---

#IV table
setwd(wd_output)
# Columns: OLS 1992-2011, IV 1992-2011, OLS 2002-2011, IV 2002-2011; all for the mothers' sample
tab <- modelsummary(list(
  "OLS '92-'11"=model_iv_baseline_11_92_momz,
  # "OLS Birth-'11 Mom"=model_ols_bc_11_MOM,
  "IV '92-'11"=model_iv_baseline_11_92_momz_iv,
  # "IV '02-Birth"=model_iv_baseline_bc_02,
  "OLS '02-'11"=model_iv_baseline_11_02_momz,
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

# Split the latex string into individual lines for post-processing
lines <- strsplit(tab, "\n")[[1]]

# The Sample/Outcome block: each cell stacks 2-3 lines via \makecell
# These rows are injected just above the GOF block so they appear between estimates and summary stats
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

lines <- append(lines, sample_outcome, after = mid_idx - 1)
# Output: Table A04.tex saved to wd_output; column numbers added by add_column_numbers()
writeLines(lines, "Table A04.tex")

add_column_numbers("Table A04.tex")

