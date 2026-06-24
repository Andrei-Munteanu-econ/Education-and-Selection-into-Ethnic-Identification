# =====================================================================
# Appendix Table A14 — Perceived markers of Roma ethnicity (survey)
# Produces:  output/Table A14.tex
# Inputs:    survey_no_priming.csv (no-priming arm), survey_priming.csv (priming arm)
#            (anonymized survey waves, built by 00_anonymize_survey.R)
# Summary:   Pools the two survey arms and reports, for each candidate cue
#            (physical aspect, skin color, clothing, name, way of speaking,
#            family history), the share of respondents who name it as a marker
#            by which Roma are identified. Outputs a bare LaTeX tabular sorted
#            from most- to least-cited characteristic.
# =====================================================================

# ---- Load and pool the two survey arms ----
setwd(wd_data_survey_processed)
data1 <- fread("survey_no_priming.csv") %>% mutate(priming = "No Priming")
data2 <- fread("survey_priming.csv")    %>% mutate(priming = "Priming")
data_master <- bind_rows(data1, data2)

# ---- Mean endorsement of each Roma-identifying characteristic ----
# V5_1..V5_6 are indicators for whether the respondent cites each cue; take the
# mean of each (the cited share), reshape to long, and sort descending.
x <- data_master %>%
  summarise(`Physical Aspect` = mean(V5_1, na.rm = T),
            `Skin Color`      = mean(V5_2, na.rm = T),
            `Clothing`        = mean(V5_3, na.rm = T),
            `Name`            = mean(V5_4, na.rm = T),
            `Way of Speaking` = mean(V5_5, na.rm = T),
            `Family History`  = mean(V5_6, na.rm = T)) %>%
  pivot_longer(cols = everything(), values_to = "Proportion", names_to = "Characteristic") %>%
  arrange(-Proportion)
x

# ---- bare tabular for \input ------------------------------------------------
body <- x %>%
  mutate(line = sprintf("%s & %.3f \\\\", Characteristic, Proportion)) %>%
  pull(line)

out <- c(
  "\\begin{tabular}{lc}",
  "\\toprule",
  "Characteristic & Proportion \\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}"
)

# ---- Save table ----
setwd(wd_output)
writeLines(out, "Table A14.tex")