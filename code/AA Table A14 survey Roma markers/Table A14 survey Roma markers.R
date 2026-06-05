# Produces Table A.14: Perceived Markers of Roma Ethnicity
# This table uses data from two survey waves (with and without ethnicity priming)
# to show which visual/social cues respondents associate with Roma identification.
# The survey asked respondents to rate the relevance of six characteristics
# for identifying someone as Roma; each V5_x variable is a binary or rated response.

#load survey data
# The two .sav files correspond to two experimental arms of the survey:
# Link1 = no ethnicity priming (control), Link2 = ethnicity priming (treatment).
setwd(wd_data_survey)
data1 <- read_sav("baza_Link1.sav") %>% mutate(priming = "No Priming")
data2 <- read_sav("baza_Link2.sav") %>% mutate(priming = "Priming")
# Pool both arms into a single dataset; the 'priming' column preserves arm membership.
data_master <- bind_rows(data1, data2)

# --- Compute share of respondents citing each characteristic ---
# Columns V5_1 through V5_6 are survey items asking whether each characteristic
# helps identify a person as Roma; averaging yields the proportion citing each cue.
x <- data_master %>%
  summarise(`Physical Aspect` = mean(V5_1, na.rm = T),
            `Skin Color`      = mean(V5_2, na.rm = T),
            `Clothing`        = mean(V5_3, na.rm = T),
            `Name`            = mean(V5_4, na.rm = T),
            `Way of Speaking` = mean(V5_5, na.rm = T),
            `Family History`  = mean(V5_6, na.rm = T)) %>%
  # Reshape to long format so each row is one characteristic and its proportion.
  pivot_longer(cols = everything(), values_to = "Proportion", names_to = "Characteristic") %>%
  # Sort descending so the most commonly cited marker appears first in the table.
  arrange(-Proportion)
x

# ---- bare tabular for \input ------------------------------------------------
# Construct each data row as a raw LaTeX string: "Characteristic & proportion \\"
# sprintf("%.3f") rounds proportions to three decimal places for display.
body <- x %>%
  mutate(line = sprintf("%s & %.3f \\\\", Characteristic, Proportion)) %>%
  pull(line)

# Assemble a self-contained booktabs tabular environment that can be \input{} directly.
out <- c(
  "\\begin{tabular}{lc}",
  "\\toprule",
  "Characteristic & Proportion \\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}"
)

# Output: Table A14.tex — LaTeX tabular saved to the output directory for \input{} in the paper.
setwd(wd_output)
writeLines(out, "Table A14.tex")
