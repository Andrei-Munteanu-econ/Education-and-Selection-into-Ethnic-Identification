# Produces Table A.7: Changes in Ethnic Identification by Education — Ethnicity Defined using Romani as Mother Tongue

# --- Section 1: Load and Prepare Linked 1992-2011 Panel (Language-Based Ethnicity) ---

# Switch to the directory containing linked census panel files
#load data
setwd(wd_data_linked)
# Load the unique matched Roma records linking 1992 and 2011 individual observations
data_1992_2011_r<-fread('data_1992_2011_roma_unique.csv')
# Construct language-based Roma flags: LIM_1992==12 flags Romani mother tongue in 1992;
# LIM_2011==1201 flags Romani mother tongue in 2011 (different coding scheme across censuses)
data_1992_2011_r<-data_1992_2011_r %>%
  mutate(ROMA_1992_LIM=LIM_1992==12,
         ROMA_2011_LIM=LIM_2011==1201)

# --- Section 2: Load and Prepare 2011 Census Data with Birth-Record Children ---

# Switch to the 2011 census data directory
setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
# Read a representative sample of the 2011 census using the project-standard sampling function
data_2011<-read_sample(filename)
# Retain only variables needed for mother-tongue and intergenerational analysis
data_2011<-data_2011 %>%
  select(id11,id11_MOM,id11_MOM_BC,nat,LIM,LIM_MOM,AA,years,SIRUTA,SIRSUP,ROMA,ROMA_MOM,HHID,scoala_m,years_MOM,years_POP,SEX,category,source)
# Read the full data for this file, then derive language-based ethnicity flags
# ROMA_bc: child born 2002-2011 identified as Roma by nat (birth record ethnic code)
# ROMA_LIM: child has Romani as mother tongue in 2011 (LIM==1201)
# ROMA_MOM_LIM: incorrectly labeled but used as a child-level LIM flag (LIM==1201); see model usage
data_2011<-read_data(filename,data_2011) %>%
  mutate(ROMA_bc=nat==12) %>%
  mutate(ROMA_LIM=LIM==1201) %>%
  mutate(ROMA_MOM_LIM=LIM==1201)
# Restrict to children born between 2002 and 2011 to identify mother–child pairs in 2011 census
data_2011_kids<-data_2011 %>%
  filter(AA %in% 2002:2011)

# --- Section 3: Build Intergenerational Dataset (Mother Linked to 1992-2011 Panel) ---

# Join children born 2002-2011 (from 2011 census) to their mothers' panel records (1992-2011).
# id11_MOM_BC identifies the mother's 2011 census person ID via birth records, matched to id11
# in the panel. This enables analysis of how the mother's education affects the child's ethnic
# identification, and how the mother's own re-identification can be inferred from her child's birth record.
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



# --- Section 4: OLS and IV Regressions — Main Panel (1992-2011, Language-Based Outcome) ---

# OLS: effect of 2011 schooling on probability of retaining Romani mother tongue in 2011,
# among individuals language-identified as Roma in 1992. Fixed effects: locality x birth-year cell.
#baseline mom-11 mom
model_ols_baseline_11_92<-feols(data=data_1992_2011_r %>%
                                  filter(!is.na(years_1992)) %>%
                                  filter(ROMA_1992_LIM==T),
                                ROMA_2011_LIM~years_2011|SIRSUP_1992^AA_1992,
                                cluster=~SIRSUP_1992)
summary(model_ols_baseline_11_92)
# IV: instruments years_2011 with years_1992 to address endogeneity of education choice.
# Sample: language-identified Roma in 1992 (ROMA_1992_LIM==T), no additional restriction on years_1992.
#baseline mom-11 mom
model_iv_baseline_11_92<-feols(data=data_1992_2011_r %>%
                                 filter(ROMA_1992_LIM==T),
                               ROMA_2011_LIM~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92)
# IV: same IV strategy but outcome is the child's Roma birth-record ethnicity (ROMA_bc),
# testing whether the mother's education (instrumented by 1992 schooling) predicts intergenerational
# ethnic transmission, using the language-based mother sample
#baseline mom-bc
model_iv_baseline_bc_92<-feols(data=data_1992_2011_mom %>%
                                 filter(ROMA_1992_LIM==T),
                               ROMA_bc~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                               cluster=~SIRSUP_1992)
summary(model_iv_baseline_bc_92)



# --- Section 5: Supplementary Regressions — Birth-Record Sample (Cross-Checks) ---

# OLS: among children with a Roma birth-record mother (ROMA_bc==T), regresses the child's own
# Romani mother-tongue status in 2011 on mother's years of schooling (years_2011 from panel),
# with locality x mother birth-year fixed effects
#bc-11 mom
model_ols_bc_11<-feols(data=data_1992_2011_mom %>%
                         filter(ROMA_bc==T),
                               ROMA_2011_LIM~years_2011|SIRSUP_2011^AA_2011,
                               cluster=~SIRSUP_2011)
summary(model_ols_bc_11)

# OLS: using the 2011-only cross-section; among children with a Roma birth-record mother,
# tests whether the mother's years of schooling (years_MOM from 2011 census) predicts whether
# the mother herself declares Romani as mother tongue in 2011 (ROMA_MOM_LIM)
#bc-11 mom
model_ols_bc_11_MOM<-feols(data=data_2011 %>% filter(ROMA_bc==T),
                      ROMA_MOM_LIM~years_MOM|SIRSUP^AA,
                      cluster=~SIRSUP)
summary(model_ols_bc_11_MOM)

# IV: effect of mother's education (instrumented by 1992 schooling) on intergenerational transmission
# measured by the child's Roma birth-record status; language-identified Roma 1992 mothers only
#baseline mom-bc
model_iv_baseline_11_92_child<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992_LIM==T),
                                     ROMA_bc~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child)

# IV: same IV setup but outcome is the child's own Romani mother-tongue status (ROMA_LIM) in 2011,
# capturing a second generation language-based passing outcome
#baseline mom-2011 child
model_iv_baseline_11_92_child2<-feols(data=data_1992_2011_mom %>% filter(ROMA_1992_LIM==T),
                                     ROMA_LIM~1|SIRSUP_1992^AA_1992|years_2011~years_1992,
                                     cluster=~SIRSUP_1992)
summary(model_iv_baseline_11_92_child2)




# --- Section 6: Table Formatting Utilities ---

######
# Map internal regressor names to display labels for the regression table
variables<-c('years_2011'='Schooling Yrs',
             'years_MOM'='Schooling Yrs',
             'years_1992'='Schooling Yrs',
             'years_2002'='Schooling Yrs',
             'fit_years_2011'='Schooling Yrs',
             'years_MOM_1992'="Schooling Yrs Mom (Baseline)",
             'years_POP_1992'="Schooling Yrs Dad (Baseline)",
             "fit_years_MOM_CHILD"='Schooling Yrs (Endline)')
# Helper to format large numbers with commas and fixed decimal places
f_big<-function(x) format(x, big.mark=",", scientific=FALSE, nsmall=1,digits=1)
options(modelsummary_format_numeric_latex = "plain")
# Custom glance method for fixest objects: recovers the dependent variable mean
# by summing fitted values and residuals, then formats it for the GOF block
glance_custom.fixest <- function(x, ...) {
  dv1 <- x$fitted.values
  dv2 <- x$residuals
  dv <- sprintf("%.2f", base::mean(dv1+dv2, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# Extract first-stage F-statistic from the IV model for the F-stat row in the table;
# OLS column gets an empty string since there is no first stage
f<-data.frame(n="F-stat",
              ols="",
              # ols2="",
              b=fitstat(model_iv_baseline_11_92, "ivf")$ivf1$stat
              # c="",
              # d=fitstat(model_iv_baseline_11_92_child, "ivf")$ivf1$stat,
              # e=fitstat(model_iv_baseline_11_92_child2, "ivf")$ivf1$stat
)
f<-f_big(f)


# --- Section 7: Produce LaTeX Table and Post-Process ---

# Switch to the output directory before writing the table file
#IV table
setwd(wd_output)
# Build the latex_tabular string with OLS and IV columns; stars follow AEJ conventions
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

# Split the single latex string into a character vector of lines for surgical insertion
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

# Splice the sample/outcome descriptor rows just above the GOF block separator
lines <- append(lines, sample_outcome, after = mid_idx - 1)
# Output: Table A7.tex — LaTeX tabular fragment for Table A.7 in the appendix
writeLines(lines, "Table A7.tex")

# Post-process the .tex file to prepend a (1), (2), ... column-number header row
add_column_numbers("Table A7.tex")


