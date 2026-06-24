# =====================================================================
# Appendix Table A03 — Conditional-independence check on sex-mismatched records
# Produces:  output/Table A03.tex
# Inputs:    data_1992_2011_unique_genderless.csv (gender-blind 1992->2011 linked panel)
# Summary:   Restricts to records whose recorded sex flips between 1992 and 2011
#            (i.e. likely mis-links). If linkage error is independent of the
#            outcomes of interest, baseline (1992) schooling should not predict
#            2011 schooling or 2011 reported-Roma status among these records.
#            Estimates four feols specifications (all mismatches vs. baseline-Roma
#            mismatches; outcomes = 2011 years of schooling and reported Roma),
#            all with SIRSUP x birth-year fixed effects and locality-clustered SEs,
#            then hand-builds a four-column LaTeX tabular via modelsummary.
# =====================================================================

# ---- Load gender-blind linked panel and select the needed variables ----
setwd(wd_data_linked)
filename <- 'data_1992_2011_unique_genderless.csv'
data_92 <- read_sample(filename) %>%
  select(years_2011, years_1992, SIRSUP_2011, AA_2011, SEX_2011, SEX_1992, ROMA_1992, ROMA_2011)
data_92 <- read_data(filename, data_92)

# ---- Specifications: does baseline schooling predict outcomes among mis-linked records? ----
# All four models keep only sex-inconsistent records (SEX_2011 != SEX_1992), absorb
# locality x birth-year fixed effects (SIRSUP_2011^AA_2011), and cluster SEs by locality.
# (1) all mismatches, outcome = 2011 years of schooling.
model92 <- feols(data_92 %>% filter(SEX_2011 != SEX_1992),
                 years_2011 ~ years_1992 | SIRSUP_2011^AA_2011,
                 vcov = ~SIRSUP_2011)
summary(model92)
# (2) mismatches among baseline (1992) Roma only, outcome = 2011 years of schooling.
model92r <- feols(data_92 %>% filter(SEX_2011 != SEX_1992 & ROMA_1992 == T),
                  years_2011 ~ years_1992 | SIRSUP_2011^AA_2011,
                  vcov = ~SIRSUP_2011)
summary(model92r)
# (3) all mismatches, outcome = 2011 reported-Roma indicator.
model92_ROMA <- feols(data_92 %>% filter(SEX_2011 != SEX_1992),
                      ROMA_2011 ~ years_1992 | SIRSUP_2011^AA_2011,
                      vcov = ~SIRSUP_2011)
summary(model92_ROMA)
# (4) mismatches among baseline (1992) Roma only, outcome = 2011 reported-Roma indicator.
model92r_ROMA <- feols(data_92 %>% filter(SEX_2011 != SEX_1992 & ROMA_1992 == T),
                       ROMA_2011 ~ years_1992 | SIRSUP_2011^AA_2011,
                       vcov = ~SIRSUP_2011)
summary(model92r_ROMA)


# ---- Assemble the LaTeX table ----
# Display label for the single regressor of interest (baseline schooling).
variables <- c('years_1992' = 'Baseline Years of Schooling')

# DV means for the add_rows block (one per model, in order)
dv_means <- sapply(list(model92, model92r, model92_ROMA, model92r_ROMA),
                   function(m) sprintf("%.2f", mean(m$fitted.values + m$residuals, na.rm = TRUE)))
dv_row <- data.frame(n = "DV Mean", c1 = dv_means[1], c2 = dv_means[2],
                     c3 = dv_means[3], c4 = dv_means[4])

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

# ---- Post-process modelsummary output: strip its header and splice in a custom one ----
# Split the rendered tabular into lines, locate the begin/end markers and the
# auto-generated "& (1) & ..." column-number row.
lines <- strsplit(as.character(tab), "\n")[[1]]

begt     <- grep("\\\\begin\\{tabular\\}", lines)[1]
endt     <- grep("\\\\end\\{tabular\\}",   lines)[1]
colnum   <- grep("^& \\(1\\)", lines)[1]   # modelsummary's "& (1) & ..." row

# body = everything between the col-number row and \end{tabular}
body <- lines[(colnum + 1):(endt - 1)]

# split body into coef block (coef + se rows) and gof block (N onward)
n_idx  <- grep("^N\\s*&", body)[1]
coefs  <- body[1:(n_idx - 1)]
gof    <- body[n_idx:length(body)]

# Custom multi-row header: groups the four columns by dependent variable
# (years of schooling vs. reported Roma) and by sample (All vs. Baseline Roma).
custom_header <- c(
  "& \\multicolumn{4}{c}{\\textit{Dependent Variable:}}\\\\",
  "& \\multicolumn{2}{c}{\\textit{Yrs of Schooling (2011)}} & \\multicolumn{2}{c}{\\textit{Reported Roma (2011)}}\\\\",
  "& (All) & (Baseline & (All) & (Baseline\\\\",
  "&  & Roma) &  & Roma)\\\\",
  "& (1) & (2) & (3) & (4) \\\\"
)

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

# ---- Save table ----
setwd(wd_output)
writeLines(lines, "Table A03.tex")