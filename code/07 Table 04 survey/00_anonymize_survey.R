# =====================================================================
# Anonymize survey data -> shippable processed CSVs
# Produces:  data/processed/survey/survey_no_priming.csv  (from baza_Link1.sav)
#            data/processed/survey/survey_priming.csv     (from baza_Link2.sav)
# Inputs:    baza_Link1.sav / baza_Link2.sav (raw survey waves, CONFIDENTIAL),
#            data/processed/survey/siruta3_roma_survey.csv (PROVIDED locality
#            Roma-share lookup, built by 00_roma_share_by_locality.R)
# Summary:   Reads the two raw SPSS survey waves and writes anonymized CSVs that
#            keep ONLY the variables the replication exhibits use, as numeric
#            codes. All direct identifiers, free-text ("other, specify") answers,
#            timestamps, interview metadata, consent text, and town/village names
#            are dropped. Fine geography is replaced by two fields:
#              - roma  : locality Roma share, attached by merging the provided
#                        siruta3_roma_survey.csv on (town, jud); NA where the
#                        locality did not match (which, reproducing the original
#                        pipeline, includes every rural row since its key is
#                        "RURAL").
#              - rural : 1 if the respondent's chosen locality label is "RURAL".
#            County (jud) is kept (coarse). The resulting CSVs are the public
#            inputs consumed by the survey analysis scripts; the raw .sav files
#            remain confidential and are not shipped.
#
#            Because the locality Roma share comes from the provided lookup, this
#            step does NOT need the confidential census -- only the raw .sav.
# =====================================================================

# Survey items retained for analysis (kept as numeric codes). II8/II10 exist only
# in the priming wave (Link2); any_of() tolerates their absence in the other wave.
survey_items <- c("II1", "II8", "II10", "II13", "II15", "V4",
                  "V5_1", "V5_2", "V5_3", "V5_4", "V5_5", "V5_6",
                  "VII1_3", "VII2_3")

# ---- Load provided locality Roma-share lookup ----
# Aggregate (town, jud) -> roma share; keys uppercased for the per-row merge below
# (mirrors the original Table 04 merge, which uppercased both sides).
roma_share <- fread(file.path(wd_data_survey_processed, "siruta3_roma_survey.csv")) %>%
  mutate(town = toupper(as.character(town)),
         jud  = toupper(as.character(jud))) %>%
  select(town, jud, roma)

# ---- Anonymize one survey wave ----
# Reads a raw .sav, attaches roma (via the lookup) and rural, keeps only the
# retained survey items (as numeric codes), drops everything else, and writes a CSV.
anonymize_wave <- function(sav_file, out_file) {
  raw <- read_sav(file.path(wd_data_survey, sav_file)) %>%
    # decode the labelled geography fields; `town` is the Table 04 merge key.
    mutate(
      town = toupper(as.character(as_factor(den_loc))),
      jud  = toupper(as.character(as_factor(jud)))
    ) %>%
    # attach per-row commune Roma share; rural rows (town == "RURAL") do not match
    # the lookup (whose entries are town/village names) and so get roma = NA.
    left_join(roma_share, by = c("town", "jud")) %>%
    mutate(rural = as.integer(town == "RURAL"))

  # Keep only county + derived geography + retained survey items (numeric codes).
  out <- raw %>%
    mutate(across(any_of(survey_items), ~ as.numeric(zap_labels(.)))) %>%
    select(jud, rural, roma, any_of(survey_items))

  if (!dir.exists(wd_data_survey_processed)) {
    dir.create(wd_data_survey_processed, recursive = TRUE)
  }
  fwrite(out, file.path(wd_data_survey_processed, out_file))
}

anonymize_wave("baza_Link1.sav", "survey_no_priming.csv")
anonymize_wave("baza_Link2.sav", "survey_priming.csv")
