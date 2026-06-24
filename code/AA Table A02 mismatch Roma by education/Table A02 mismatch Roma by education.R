# =====================================================================
# Appendix Table A02 — Inconsistent-sex (mismatch) rate among baseline Roma, by education
# Produces:  output/Table A02.tex
# Inputs:    data_2002_2011_unique_genderless.csv, data_1992_2011_unique_genderless.csv
#            (gender-blind linked census panels, 1992->2011 and 2002->2011)
# Summary:   Among individuals reported Roma at baseline (1992 or 2002), tabulates the
#            share whose recorded sex flips across census waves (a proxy for linkage/
#            measurement error), broken out by 2011 education level. Emits a bare
#            LaTeX tabular with observation counts and inconsistent-sex percentages.
# =====================================================================

# ---- Load gender-blind linked census panels ----
# Match rates estimated using "genderless" matches (links made without using sex),
# so the sex variable can later be used as an independent consistency check.
setwd(wd_data_linked)
data_2002_2011<-fread('data_2002_2011_unique_genderless.csv')
data_1992_2011<-fread('data_1992_2011_unique_genderless.csv')

# ---- 1992->2011 panel: inconsistent-sex share by education, baseline Roma only ----
# Collapse the detailed 2011 education categories into five ordered groups, flag
# sex mismatches across waves, keep only those reported Roma in 1992, then compute
# the within-education share whose sex is inconsistent.
stats_1992_educ_roma<-data_1992_2011 %>%
  mutate(EDUC_2011 = case_when(EDUC_2011 %in% c("Higher Short","Higher Long") ~ "Postsecondary",
                               EDUC_2011 %in% c("Specialized HS","General HS","Vocational","Postsec") ~ "High School or Vocational",
                               EDUC_2011 %in% c("Gym") ~ "Middle School",
                               EDUC_2011 %in% c("No formal") ~ "None",
                               EDUC_2011 %in% c("Primary") ~ "Primary School",
                               TRUE ~ EDUC_2011
  )) %>%
  mutate(EDUC_2011=factor(EDUC_2011,levels=c("None","Primary School","Middle School","High School or Vocational","Postsecondary" ))) %>%
  mutate(mismatch=SEX_1992!=SEX_2011) %>%
  filter(ROMA_1992==T) %>%
  group_by(EDUC_2011,mismatch) %>%
  summarise(n=n())%>%
  mutate(p=n/sum(n)*100,n=sum(n)) %>%
  filter(mismatch==T) %>%
  select(-mismatch)

# ---- 2002->2011 panel: same inconsistent-sex share by education, baseline Roma only ----
# Identical construction as above but for the 2002->2011 link and the ROMA_2002 flag.
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


# ---- Merge the two panels side by side, one row per education level ----
stats<-stats_1992_educ_roma %>%
  left_join(stats_2002_educ_roma,by="EDUC_2011",suffix=c("_1992_2011","_2002_2011"))


# ---- Assemble the LaTeX table ----
# Build a bare tabular meant to be \input into the paper. Trivial number-formatting
# helpers: counts with thousands separators, percentages to one decimal place.
fmt_n   <- function(x) format(round(x), big.mark = ",", scientific = FALSE)
fmt_pct <- function(x) sprintf("%.1f", x)

body <- stats %>%
  mutate(line = sprintf("%s & %s & %s & %s & %s \\\\",
                        EDUC_2011,
                        fmt_n(n_1992_2011),  fmt_pct(p_1992_2011),
                        fmt_n(n_2002_2011),  fmt_pct(p_2002_2011))) %>%
  pull(line)

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

# ---- Save table and append column numbers ----
setwd(wd_output)
writeLines(out, "Table A02.tex")

# Shared helper that inserts a (1)(2)... column-numbering row into the saved tabular.
add_column_numbers("Table A02.tex")
