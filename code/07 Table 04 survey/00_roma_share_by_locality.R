# =====================================================================
# Generator: locality Roma share for the survey sample (author-only; no exhibit)
# Produces:  data/processed/survey/siruta3_roma_survey.csv
#            (PROVIDED aggregate lookup, committed to the repo and read by
#             00_anonymize_survey.R; keyed on (town, jud))
# Inputs:    baza_Link1.sav / baza_Link2.sav (survey waves, for surveyed towns),
#            siruta.dta (SIRUTA locality-code dictionary),
#            data_2011_clean.csv (2011 census, for locality Roma shares)
# Summary:   Matches each surveyed town/county to its SIRUTA locality code, then
#            attaches the 2011-census Roma population share of that locality (commune
#            level, SIRSUP) plus an above-/below-median indicator. Output keyed on
#            (town, jud) is later merged into the survey by 00_anonymize_survey.R.
#
#            This step needs the confidential 2011 census and is run by the authors
#            on the HPC cluster; its output is provided so that replicators do not
#            need the census to attach the locality Roma share. In main.R it is
#            gated behind `regenerate_siruta_share` (default FALSE).
# =====================================================================

# ---- Load survey data ----
setwd(wd_data_survey)
library(foreign)
data1 <- read.spss("baza_Link1.sav", to.data.frame = TRUE)
data2 <- read.spss("baza_Link2.sav", to.data.frame = TRUE)


data<-rbindlist(list(data1,data2),fill=T)


# Resolve the locality name: use den_loc, but fall back to the village name
# (Den_vill) when den_loc is "RURAL"; strip any "_duplicated..." suffix.
data<-data %>%
  mutate(town=ifelse(den_loc!="RURAL",as.character(den_loc),as.character(Den_vill))) %>%
  mutate(town = sub("_duplicated.*", "", town))


# ---- Load SIRUTA locality dictionary ----
setwd(wd_data_11_other)
siruta<-read_dta("siruta.dta")



# ---- Build town/county lookup from the SIRUTA dictionary ----
# County (judet) names: niv==1 rows, stripping the "JUDETUL "/"MUNICIPIUL " prefix.
siruta_cty<-siruta%>%
  filter(niv==1) %>%
  mutate(jud_name=gsub("JUDETUL |MUNICIPIUL ","",denloc,perl=T)) %>%
  select(jud,jud_name)

# Town list: one row per locality (niv 2 or 3), names cleaned of "ORAS "/"MUNICIPIUL "
# prefixes, joined to county names, with a few manual name fixes for known mismatches.
siruta_towns<-siruta%>%
  group_by(FS2) %>%
  arrange(niv) %>%
  slice(1) %>%
  filter(niv %in% c(2,3)) %>%
  mutate(denloc=gsub("ORAS ","",denloc))  %>%
  mutate(denloc=trimws(gsub("MUNICIPIUL ","",denloc))) %>%
  left_join(siruta_cty) %>%
  mutate(denloc=case_when(denloc=="CAMPULUNG" & jud_name=="ARGES" ~ "CAMPULUNG MUSCEL",
                          denloc=="BIRA" & jud_name=="NEAMT" ~ "BARA",
                        TRUE ~ denloc))

# ---- Match surveyed towns to SIRUTA codes ----
# Distinct (town, county) pairs appearing in the survey.
towns<-data %>%
  distinct(town,jud) %>%
  mutate(jud=toupper(jud))

# Join surveyed towns to the SIRUTA dictionary to recover each town's locality code.
match<-towns %>%
  inner_join(siruta_towns,by=c("town"="denloc","jud"="jud_name"))

# Sanity check: surveyed towns that failed to match the SIRUTA dictionary.
unmatched<-towns %>%
  anti_join(match)

# ---- Compute locality Roma shares from the 2011 census ----
setwd(wd_data_11)
data_2011<-fread("data_2011_clean.csv")

# Roma population share of each commune (SIRSUP): fraction with a Roma ethnicity code.
roma_sirsup<-data_2011 %>%
  group_by(SIRSUP) %>%
  summarise(roma=mean(ET %in% 1200:1299,na.rm=T))

roma_median <- median(roma_sirsup$roma, na.rm = TRUE)

# Flag communes above the (census-wide) median Roma share.
roma_sirsup <- roma_sirsup %>%
  mutate(roma_above_median = if_else(roma > roma_median, 1, 0))



# ---- Attach Roma share to surveyed towns and write output ----
# Join the commune Roma share onto each matched survey town (siruta == SIRSUP).
match<-match %>%
  left_join(roma_sirsup,by=c("siruta"="SIRSUP"))
match<-match %>%
  select(town,jud,roma,roma_above_median)

if (!dir.exists(wd_data_survey_processed)) {
  dir.create(wd_data_survey_processed, recursive = TRUE)
}
fwrite(match, file.path(wd_data_survey_processed, "siruta3_roma_survey.csv"))
