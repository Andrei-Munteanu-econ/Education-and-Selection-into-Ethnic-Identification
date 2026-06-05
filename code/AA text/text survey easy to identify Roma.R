# In-text statistic: ease of identifying Roma (from survey); also writes text easy to identify Roma.tex

# --- Setup: Load Survey Data ---

# Switch to the directory containing the raw survey SPSS files
setwd(wd_data_survey)

# Load the two survey arms (Link1 = no priming, Link2 = priming) and tag each
# with its experimental condition so they can be distinguished after stacking
data1<-read_sav("baza_Link1.sav") %>% mutate(priming="No Priming")
data2<-read_sav("baza_Link2.sav") %>% mutate(priming="Priming")

# Stack both arms into one master file for descriptive operations
data_master<-bind_rows(data1,data2)


# Rebuild master with decoded locality labels (den_loc and jud are labelled
# SPSS variables; as_factor() extracts their value labels as character strings)
data_master <- bind_rows(data2, data1) %>%
  # decode den_loc -> town, jud (labelled -> character, uppercased)
  mutate(
    # [town]: survey respondent's municipality name (standardised to uppercase)
    town = toupper(as.character(as_factor(den_loc))),
    # [jud]: county (judet) name, uppercase for consistent merging
    jud  = toupper(as.character(as_factor(jud)))
  )


# --- Construct Roma Flag ---

# roma flag (.do line 210): self/other declared Roma in VII1_3 or VII2_3
# VII1_3 and VII2_3 are survey items where respondents self-declare (VII1)
# or an enumerator classifies (VII2) ethnicity; code 1 = Roma in both items
dat <- data1 %>%
  # [roma]: 1 if the respondent or the enumerator identifies the person as Roma
  mutate(roma = as.integer(VII1_3 == 1 | VII2_3 == 1))

# --- Table 3: V4 frequency distribution -------------------------------------
# 5 ordered levels; labels translated from the Romanian in the .do comment.
# V4 asks enumerators how easy it was to determine the respondent's ethnicity
# (1 = very easy ... 5 = very difficult); this distribution is Table A13
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
# Write Table A13 (enumerator ease-of-identification distribution) to disk
writeLines(tab3, file.path(wd_output, "Table A13.tex"))
