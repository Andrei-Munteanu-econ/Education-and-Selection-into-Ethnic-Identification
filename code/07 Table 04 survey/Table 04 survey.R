# Produces Table 4: Beliefs Regarding Roma Changes in Ethnic Identification and Education

# --- Load locality-level Roma population shares ---

setwd(wd_data_11_other)
# siruta3_roma_survey.csv: locality-level share of Roma residents, used to split
# survey respondents into high- vs. low-Roma-share localities
roma_share <- fread( "siruta3_roma_survey.csv")

# --- Load survey data from two experimental arms ---

setwd(wd_data_survey)
# baza_Link1.sav / baza_Link2.sav: incentivized survey on beliefs about Roma passing;
# Link1 = no priming arm, Link2 = priming arm (respondents primed about Roma identity)
data1<-read_sav("baza_Link1.sav") %>% mutate(priming="No Priming")
data2<-read_sav("baza_Link2.sav") %>% mutate(priming="Priming")
data_master<-bind_rows(data1,data2)


# Re-stack with priming arm first (Link2), then decode labelled SPSS variables to strings
data_master <- bind_rows(data2, data1) %>%
  # decode den_loc -> town, jud (labelled -> character, uppercased)
  mutate(
    # town: locality name from SPSS value labels, uppercased for merging with census locality file
    town = toupper(as.character(as_factor(den_loc))),
    # jud: county name from SPSS value labels, uppercased for merging
    jud  = toupper(as.character(as_factor(jud)))
  )

# Uppercase town and county in the Roma-share lookup table to match the survey strings
roma_share <- roma_share %>%
  mutate(
    town = toupper(as.character(town)),
    jud  = toupper(as.character(jud))
  )

# m:1 merge on (town, jud); Stata: drop if _m==2 (keep master rows only).
# Stata asserts town=="RURAL" <=> unmatched-from-master (_m==1).
data_master <- data_master %>%
  left_join(roma_share, by = c("town", "jud")) %>%
  # roma_above_median_insample: above in-sample median of `roma` (locality share)
  # Split respondents at the in-sample median so the two groups are balanced
  mutate(roma_above_median_insample = as.integer(roma > median(roma, na.rm = TRUE)))

###
# --- Construct indicator variables for table rows ---
data_master <- data_master %>%
  mutate(
    # II13: survey question asking whether non-Roma are less (1), equally (2), or more (3)
    # educated than Roma — the core belief outcome variable
    main_less = as.integer(II13 == 1),
    main_same = as.integer(II13 == 2),
    main_more = as.integer(II13 == 3),
    attentive = as.integer(II15 == 3),   # "Persoane rome, indif. de etnia decl"
    # rural: respondents from localities recorded as "RURAL" in the survey geography
    rural     = as.integer(town == "RURAL")
  )

# row_stats: helper to compute the three belief shares and sample size for any subsample;
# each statistic is pre-formatted for direct LaTeX insertion
row_stats <- function(d) {
  d %>%
    summarise(
      less = sprintf("%.1f", 100 * mean(main_less, na.rm = TRUE)),
      same = sprintf("%.1f", 100 * mean(main_same, na.rm = TRUE)),
      more = sprintf("%.1f", 100 * mean(main_more, na.rm = TRUE)),
      n    = format(sum(!is.na(main_less)), big.mark = ",")
    )
}

# --- Compute statistics for each subgroup used in Table 4 ---
# Each entry corresponds to one row in the published table
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

# Assemble one LaTeX table row per subgroup: "Label & less% & same% & more% & N \\"
main_body <- vapply(names(rows), function(nm) {
  r <- rows[[nm]]
  sprintf("%s & %s & %s & %s & %s \\\\", nm, r$less, r$same, r$more, r$n)
}, character(1))

# --- Build the complete LaTeX table string ---
# Manually constructed because the table layout (multicolumn header, addlinespace groupings)
# is not easily expressible via modelsummary or kbl
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

# --- Write output ---
# Output: output/Table 04.tex — Table 4 in the paper
setwd(wd_output)
writeLines(tab2, file.path(wd_output, "Table 04.tex"))
