# Produces Table A.3: Conditional Independence of Reported Education and Ethnicity for Mismatched Records

# --- Purpose ---
# Table A.3 is a falsification test for the IV exclusion restriction.
# The instrument (years_1992) must not directly predict 2011 outcomes through
# channels other than 2011 education. If mismatched records (individuals whose
# reported sex differs between 1992 and 2011, indicating likely false census
# links) drive the main results, the IV strategy would be invalid.
# This table shows that even among mismatched records, years_1992 predicts
# years_2011 (the first stage remains valid) but does NOT predict ROMA_2011
# (the exclusion restriction holds among false matches as well).

# --- Data Loading ---
setwd(wd_data_linked)
# filename: the genderless-cell linked dataset (individuals matched across
# 1992 and 2011 censuses without conditioning on sex, yielding a broader
# match set that includes some sex-discordant, potentially false, links)
filename <- 'data_1992_2011_unique_genderless.csv'
# Read a random sample first (via read_sample) to infer column types cheaply,
# then read the full dataset retaining only the variables needed for this table
data_92 <- read_sample(filename) %>%
  select(years_2011, years_1992, SIRSUP_2011, AA_2011, SEX_2011, SEX_1992, ROMA_1992, ROMA_2011)
data_92 <- read_data(filename, data_92)

# --- Model Estimation ---
# The mismatch sample is defined as records where SEX_2011 != SEX_1992.
# A genuine match should always agree on sex; disagreement signals a false link.
# All four models use locality-by-birth-year fixed effects (SIRSUP_2011^AA_2011)
# to absorb local cohort-level variation, and cluster SEs at the locality level.

# Col (1): First stage among ALL sex-mismatched records
# Tests whether 1992 schooling predicts 2011 schooling even for likely false matches
model92 <- feols(data_92 %>% filter(SEX_2011 != SEX_1992),
                 years_2011 ~ years_1992 | SIRSUP_2011^AA_2011,
                 vcov = ~SIRSUP_2011)
summary(model92)
# Col (2): First stage restricted to mismatched records that were Roma in 1992
# Checks whether the instrument remains relevant in the Roma-only mismatch subsample
model92r <- feols(data_92 %>% filter(SEX_2011 != SEX_1992 & ROMA_1992 == T),
                  years_2011 ~ years_1992 | SIRSUP_2011^AA_2011,
                  vcov = ~SIRSUP_2011)
summary(model92r)
# Col (3): Exclusion restriction test -- does years_1992 directly predict ROMA_2011
# among ALL mismatched records? Under the null of no direct effect, the coefficient
# should be near zero. A large significant coefficient would indicate the instrument
# affects ethnic identification through something other than schooling.
model92_ROMA <- feols(data_92 %>% filter(SEX_2011 != SEX_1992),
                      ROMA_2011 ~ years_1992 | SIRSUP_2011^AA_2011,
                      vcov = ~SIRSUP_2011)
summary(model92_ROMA)
# Col (4): Same exclusion restriction test, restricted to Roma-baseline mismatch records
model92r_ROMA <- feols(data_92 %>% filter(SEX_2011 != SEX_1992 & ROMA_1992 == T),
                       ROMA_2011 ~ years_1992 | SIRSUP_2011^AA_2011,
                       vcov = ~SIRSUP_2011)
summary(model92r_ROMA)


# --- Table Metadata ---
# Rename the single regressor for display
variables <- c('years_1992' = 'Baseline Years of Schooling')

# DV means for the add_rows block (one per model, in order)
# Recover the raw dependent variable as fitted + residual, then average
dv_means <- sapply(list(model92, model92r, model92_ROMA, model92r_ROMA),
                   function(m) sprintf("%.2f", mean(m$fitted.values + m$residuals, na.rm = TRUE)))
# dv_row: a one-row data frame that modelsummary will splice into the GOF block
dv_row <- data.frame(n = "DV Mean", c1 = dv_means[1], c2 = dv_means[2],
                     c3 = dv_means[3], c4 = dv_means[4])

# --- Raw LaTeX Generation via modelsummary ---
# output = "latex_tabular" returns only the tabular environment (no floating wrapper),
# making post-processing easier. escape = FALSE is required because star symbols
# and math mode are embedded in the stars argument.
tab <- modelsummary(list(model92, model92r, model92_ROMA, model92r_ROMA),
                    estimate  = "{estimate}{stars}",
                    statistic = "std.error",
                    stars     = c('$^{*}$' = 0.1, '$^{**}$' = 0.05, '$^{***}$' = 0.01),
                    gof_map = list(
                      list("raw" = "nobs",       "clean" = "N",       "fmt" = f_big),
                      list("raw" = "r.squared",  "clean" = "R$^2$",   "fmt" = "%.2f")
                    ),
                    add_rows      = dv_row,
                    metrics       = "R2",
                    output        = "latex_tabular",
                    coef_rename   = variables,
                    align         = "lcccc",
                    escape        = FALSE)

# --- LaTeX Post-Processing ---
# Split the tabular string into a character vector, one element per line,
# so individual rows can be located by regex and the structure reassembled
lines <- strsplit(as.character(tab), "\n")[[1]]

# Locate structural landmarks in the raw LaTeX
begt     <- grep("\\\\begin\\{tabular\\}", lines)[1]
endt     <- grep("\\\\end\\{tabular\\}",   lines)[1]
colnum   <- grep("^& \\(1\\)", lines)[1]   # modelsummary's "& (1) & ..." row

# body = everything between the col-number row and \end{tabular}
body <- lines[(colnum + 1):(endt - 1)]

# split body into coef block (coef + se rows) and gof block (N onward)
n_idx  <- grep("^N\\s*&", body)[1]
coefs  <- body[1:(n_idx - 1)]
gof    <- body[n_idx:length(body)]

# Build a two-tier column header:
# Row 1: groups cols 1-4 under "Dependent Variable:"
# Row 2: spans cols 1-2 for "Yrs of Schooling (2011)", cols 3-4 for "Reported Roma (2011)"
# Rows 3-4: sub-labels "(All)" and "(Baseline Roma)" split across two lines for readability
# Row 5: numeric column identifiers (1)-(4)
custom_header <- c(
  "& \\multicolumn{4}{c}{\\textit{Dependent Variable:}}\\\\",
  "& \\multicolumn{2}{c}{\\textit{Yrs of Schooling (2011)}} & \\multicolumn{2}{c}{\\textit{Reported Roma (2011)}}\\\\",
  "& (All) & (Baseline & (All) & (Baseline\\\\",
  "&  & Roma) &  & Roma)\\\\",
  "& (1) & (2) & (3) & (4) \\\\"
)

# Reassemble the full tabular, inserting custom header and booktabs rules
lines <- c(
  "\\begin{tabular}[t]{lcccc}",
  "\\toprule",
  custom_header,
  "\\midrule",
  coefs,
  "\\midrule",
  gof,
  "\\bottomrule",
  "\\end{tabular}"
)

# --- Output ---
# Write the final LaTeX tabular to output/tables/Table A03.tex
setwd(wd_output)
writeLines(lines, "Table A03.tex")
