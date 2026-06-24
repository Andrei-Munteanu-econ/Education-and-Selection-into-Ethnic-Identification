# In-text statistic: ease of identifying Roma (from survey); also writes text easy to identify Roma.tex

# --- Setup: Load Survey Data ---

# Switch to the directory containing the anonymized survey CSVs
setwd(wd_data_survey_processed)

# The V4 ease-of-identification distribution below uses the no-priming arm only.
data1 <- fread("survey_no_priming.csv") %>% mutate(priming = "No Priming")


# --- Construct Roma Flag ---

# roma flag (.do line 210): self/other declared Roma in VII1_3 or VII2_3
# VII1_3 and VII2_3 are survey items where respondents self-declare (VII1)
# or an enumerator classifies (VII2) ethnicity; code 1 = Roma in both items
dat <- data1 %>%
  # [roma]: 1 if the respondent or the enumerator identifies the person as Roma
  mutate(roma = as.integer(VII1_3 == 1 | VII2_3 == 1))

# --- V4 frequency distribution (in-text "ease of identification" statistic) -
# 5 ordered levels; labels translated from the Romanian in the .do comment.
# V4 asks enumerators how easy it was to determine the respondent's ethnicity
# (1 = very easy ... 5 = very difficult). This distribution is the in-text
# exhibit "text easy to identify Roma.tex" (NOT Table A13, which is a separate
# 2x2 cross-tab produced by "AA Table A13 .../Table A13.R").
v4_labels <- c(
  "1" = "Very easy to know the person's ethnicity",
  "2" = "Relatively easy",
  "3" = "Neither easy nor difficult",
  "4" = "Relatively difficult",
  "5" = "Very difficult to know the person's ethnicity"
)

# Tabulate response counts and compute row percentages
v4_tab <- dat %>%
  filter(!is.na(V4)) %>%
  count(V4 = as.integer(V4), name = "N") %>%
  arrange(V4) %>%
  mutate(pct = 100 * N / sum(N))

# [v4_n]: total valid responses to V4 (denominator for the percent column)
v4_n <- sum(v4_tab$N)

# Build one LaTeX row per response category using the human-readable label
v4_body <- v4_tab %>%
  mutate(line = sprintf("%s & %s & %.1f \\\\",
                        v4_labels[as.character(V4)],
                        format(N, big.mark = ","), pct)) %>%
  pull(line)

# Assemble the complete booktabs LaTeX table as a character vector
tab3 <- c(
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "& Freq. & Percent \\\\",
  "\\midrule",
  v4_body,
  "\\midrule",
  sprintf("Total & %s & 100.0 \\\\", format(v4_n, big.mark = ",")),
  "\\bottomrule",
  "\\end{tabular}"
)

# --- Output ---

setwd(wd_output)
# Write the in-text ease-of-identification distribution to disk
writeLines(tab3, file.path(wd_output, "text easy to identify Roma.tex"))
