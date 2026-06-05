# Produces Table A.2: Census Linkages for Baseline Roma

# --- Load Linked Census Data ---
#estimate match rates using genderless matches----
setwd(wd_data_linked)
# data_2002_2011_unique_genderless.csv: genderless-matched panel linking 2002 and 2011 census records
data_2002_2011<-fread('data_2002_2011_unique_genderless.csv')
# data_1992_2011_unique_genderless.csv: genderless-matched panel linking 1992 and 2011 census records
data_1992_2011<-fread('data_1992_2011_unique_genderless.csv')

# --- Compute Mismatch Rates by Education: 1992-2011 Panel ---
#stats roma educ
# Mismatch is defined as an inconsistency in reported sex between the two census waves for the same
# matched record pair. Because matching is genderless (cell_id_genderless), some false matches will
# pair records from different individuals of different sexes; the sex-inconsistency rate is therefore
# a proxy for the false-match (mismatch) rate used in the mismatch correction formula.
stats_1992_educ_roma<-data_1992_2011 %>%
  # Collapse the fine-grained EDUC harmonisation into five broad groups for the table
  mutate(EDUC_2011 = case_when(EDUC_2011 %in% c("Higher Short","Higher Long") ~ "Postsecondary",
                               EDUC_2011 %in% c("Specialized HS","General HS","Vocational","Postsec") ~ "High School or Vocational",
                               EDUC_2011 %in% c("Gym") ~ "Middle School",
                               EDUC_2011 %in% c("No formal") ~ "None",
                               EDUC_2011 %in% c("Primary") ~ "Primary School",
                               TRUE ~ EDUC_2011
  )) %>%
  # Enforce ascending education order so table rows read from lowest to highest
  mutate(EDUC_2011=factor(EDUC_2011,levels=c("None","Primary School","Middle School","High School or Vocational","Postsecondary" ))) %>%
  # mismatch: TRUE when the sex reported in 1992 differs from the sex reported in 2011 — flags likely false matches
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  # Restrict to individuals self-identified as Roma in 1992 — the baseline population of interest
  filter(ROMA_1992==T) %>%
  group_by(EDUC_2011,mismatch) %>%
  summarise(n=n())%>%
  # p: percent of matched records within each education-mismatch cell that have inconsistent sex;
  # n is replaced with the total observations per education group (denominator for p)
  mutate(p=n/sum(n)*100,n=sum(n)) %>%
  # Keep only the mismatch == TRUE row per education group (the false-match rate row)
  filter(mismatch==T) %>%
  select(-mismatch)

# --- Compute Mismatch Rates by Education: 2002-2011 Panel ---
#stats roma educ
# Same logic as above but for the 2002-2011 linkage; uses SEX_2002 vs SEX_2011 and ROMA_2002 flag
stats_2002_educ_roma<-data_2002_2011 %>%
  mutate(EDUC_2011 = case_when(EDUC_2011 %in% c("Higher Short","Higher Long") ~ "Postsecondary",
                               EDUC_2011 %in% c("Specialized HS","General HS","Vocational","Postsec") ~ "High School or Vocational",
                               EDUC_2011 %in% c("Gym") ~ "Middle School",
                               EDUC_2011 %in% c("No formal") ~ "None",
                               EDUC_2011 %in% c("Primary") ~ "Primary School",
                               TRUE ~ EDUC_2011
  )) %>%
  mutate(EDUC_2011=factor(EDUC_2011,levels=c("None","Primary School","Middle School","High School or Vocational","Postsecondary" ))) %>%
  mutate(mismatch=SEX_2002!=SEX_2011) %>%
  filter(ROMA_2002==T) %>%
  group_by(EDUC_2011,mismatch) %>%
  summarise(n=n())%>%
  mutate(p=n/sum(n)*100,n=sum(n)) %>%
  filter(mismatch==T) %>%
  select(-mismatch)


# --- Merge Both Panels into a Single Summary Table ---
# stats: one row per education group with mismatch counts and rates for both census linkages side-by-side
stats<-stats_1992_educ_roma %>%
  left_join(stats_2002_educ_roma,by="EDUC_2011",suffix=c("_1992_2011","_2002_2011"))


# setwd(wd_output)
# fileConn<-file("03_table_3_match_roma_education.txt")
# writeLines(print(xtable(stats, type = "latex",digits=c(NA,NA,0,1,0,1)),
#                  include.rownames=FALSE,
#                  format.args=list(big.mark = ","), suffix = "%")
#            , fileConn)
# close(fileConn)
#
# stats <- stats_1992_educ_roma %>%
#   left_join(stats_2002_educ_roma, by = "EDUC_2011", suffix = c("_1992_2011", "_2002_2011"))

# ---- emit bare tabular for \input ------------------------------------------
# fmt_n: formats observation counts with thousands separators and no scientific notation
fmt_n   <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
# fmt_pct: formats mismatch percentages to one decimal place
fmt_pct <- function(x) sprintf("%.1f", x)

# Build one LaTeX table row per education group, combining both panels' counts and mismatch rates
body <- stats %>%
  mutate(line = sprintf("%s & %s & %s & %s & %s \\\\",
                        EDUC_2011,
                        fmt_n(n_1992_2011),  fmt_pct(p_1992_2011),
                        fmt_n(n_2002_2011),  fmt_pct(p_2002_2011))) %>%
  pull(line)

# Assemble the complete booktabs tabular environment with grouped column headers
out <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "& \\multicolumn{2}{c}{1992--2011} & \\multicolumn{2}{c}{2002--2011} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "Education & Observations & \\makecell{Inconsistent Sex\\\\(\\%)} & Observations & \\makecell{Inconsistent Sex\\\\(\\%)} \\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}"
)

# Output: Table A02.tex — LaTeX tabular of sex-mismatch rates by education group for Roma-identified individuals
setwd(wd_output)
writeLines(out, "Table A02.tex")

# Post-process the .tex file to insert column-number headers (e.g., (1), (2), ...) expected by the paper template
add_column_numbers("Table A02.tex")
