# =====================================================================
# Table 04 — Survey beliefs about education and Roma identity passing
# Produces:  output/Table 04.tex
# Inputs:    survey_no_priming.csv / survey_priming.csv (anonymized survey waves,
#            built by 00_anonymize_survey.R; carry per-row locality `roma` share
#            and a `rural` flag)
# Summary:   Tabulates the share of respondents who believe reported non-Roma are
#            less / equally / more educated than reported Roma (item II13), for the
#            full sample and broken out by priming arm, attentiveness, rural/urban,
#            and whether the locality is above/below the in-sample median Roma share.
#            Each cell is a percentage with the underlying observation count.
# =====================================================================

# ---- Load anonymized survey waves ----
# Two files correspond to the experiment's two arms (no-priming / priming). They
# already carry the per-row locality `roma` share and a `rural` flag (derived in
# 00_anonymize_survey.R); survey items are stored as numeric codes.
setwd(wd_data_survey_processed)
data1 <- fread("survey_no_priming.csv") %>% mutate(priming = "No Priming")
data2 <- fread("survey_priming.csv")    %>% mutate(priming = "Priming")
data_master <- bind_rows(data2, data1)

# roma_above_median_insample: above in-sample median of `roma` (locality share)
data_master <- data_master %>%
  mutate(roma_above_median_insample = as.integer(roma > median(roma, na.rm = TRUE)))

# ---- Build outcome and subgroup indicators ----
# II13 is the key survey item: respondent's belief about whether reported non-Roma
# are less (1) / equally (2) / more (3) educated than reported Roma. II15==3 flags
# "attentive" respondents; `rural` (precomputed) flags rural localities.
data_master <- data_master %>%
  mutate(
    main_less = as.integer(II13 == 1),
    main_same = as.integer(II13 == 2),
    main_more = as.integer(II13 == 3),
    attentive = as.integer(II15 == 3)    # "Persoane rome, indif. de etnia decl"
  )

# ---- Compute table rows (full sample and subgroups) ----
# row_stats: for a (possibly filtered) subset, return the percentage of respondents
# choosing less/same/more on II13 and the number of non-missing responses (N).
row_stats <- function(d) {
  d %>%
    summarise(
      less = sprintf("%.1f", 100 * mean(main_less, na.rm = TRUE)),
      same = sprintf("%.1f", 100 * mean(main_same, na.rm = TRUE)),
      more = sprintf("%.1f", 100 * mean(main_more, na.rm = TRUE)),
      n    = format(sum(!is.na(main_less)), big.mark = ",")
    )
}

rows <- list(
  "Full Sample"              = row_stats(data_master),
  "Priming"                  = row_stats(filter(data_master, priming == "Priming")),
  "No priming"               = row_stats(filter(data_master, priming == "No Priming")),
  "Attentive"                = row_stats(filter(data_master, attentive == 1)),
  "Not attentive"            = row_stats(filter(data_master, attentive == 0)),
  "Rural"                    = row_stats(filter(data_master, rural == 1)),
  "Urban"                    = row_stats(filter(data_master, rural == 0)),
  "High Share Roma Locality" = row_stats(filter(data_master, roma_above_median_insample == 1)),
  "Low Share Roma Locality"  = row_stats(filter(data_master, roma_above_median_insample == 0))
)

# ---- Assemble and write the LaTeX table ----
# Format each named row into a LaTeX tabular line: label & less & same & more & N.
main_body <- vapply(names(rows), function(nm) {
  r <- rows[[nm]]
  sprintf("%s & %s & %s & %s & %s \\\\", nm, r$less, r$same, r$more, r$n)
}, character(1))

# Build the full tabular: multi-line column header describing the II13 question,
# then the formatted rows grouped (full sample, then each subgroup split) with
# spacing rules between groups.
tab2 <- c(
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  "\\addlinespace",
  "& \\multicolumn{4}{c}{Reported non-Roma are \\rule[-0.5ex]{2em}{0.5pt} educated} \\\\",
  "& \\multicolumn{4}{c}{compared to reported Roma} \\\\",
  "& \\multicolumn{4}{c}{(\\% among reported Roma in previous census)} \\\\",
  "& \\multicolumn{4}{c}{(Incentivized)} \\\\",
  "\\addlinespace",
  "Sample & Less & Equally & More & Obs. \\\\",
  "\\midrule",
  "\\addlinespace",
  main_body[1],                 # Full Sample
  "\\addlinespace",
  main_body[2:3],               # priming / no priming
  "\\addlinespace",
  main_body[4:5],               # attentive / not
  "\\addlinespace",
  main_body[6:7],               # rural / urban
  "\\addlinespace",
  main_body[8:9],               # high / low share roma
  "\\bottomrule",
  "\\end{tabular}"
)

setwd(wd_output)
writeLines(tab2, file.path(wd_output, "Table 04.tex"))