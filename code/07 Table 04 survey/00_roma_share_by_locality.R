# Helper: computes Roma population share per locality from survey data; called before Table 04 survey.R

# --- Load Survey Data ---

#load survey data
setwd(wd_data_survey)
library(foreign)
# baza_Link1.sav / baza_Link2.sav: two waves or sub-samples of the survey dataset in SPSS format
data1 <- read.spss("baza_Link1.sav", to.data.frame = TRUE)
data2 <- read.spss("baza_Link2.sav", to.data.frame = TRUE)


# Stack the two survey sub-samples into a single data.table; fill=T handles any column mismatches
data<-rbindlist(list(data1,data2),fill=T)


# --- Harmonise Locality Name ---

# den_loc: urban locality name; Den_vill: village name used when the record is rural
# Assign the appropriate name depending on whether the respondent lives in an urban or rural unit
data<-data %>%
  mutate(town=ifelse(den_loc!="RURAL",as.character(den_loc),as.character(Den_vill))) %>%
  # Remove the "_duplicated..." suffix that arises when SPSS value labels collide across merge keys
  mutate(town = sub("_duplicated.*", "", town))


# --- Load SIRUTA Reference Table ---

#siruta
setwd(wd_data_11_other)
# siruta.dta: the official Romanian locality classification table used to map locality names to codes
siruta<-read_dta("siruta.dta")



# --- Build County-Level and Town-Level SIRUTA Look-up Tables ---

#siruta county
# niv==1 selects the county (judet) rows; strip the administrative prefix so the name matches survey strings
siruta_cty<-siruta%>%
  filter(niv==1) %>%
  mutate(jud_name=gsub("JUDETUL |MUNICIPIUL ","",denloc,perl=T)) %>%
  select(jud,jud_name)

# FS2: the parent-locality code; grouping on FS2 and keeping niv==2 or 3 gives one row per urban commune or town
# slice(1) after arrange(niv) keeps the highest-level (lowest niv) entry when multiple rows share an FS2 code
siruta_towns<-siruta%>%
  group_by(FS2) %>%
  arrange(niv) %>%
  slice(1) %>%
  filter(niv %in% c(2,3)) %>%
  # Strip administrative prefixes so names are comparable to the survey's den_loc strings
  mutate(denloc=gsub("ORAS ","",denloc))  %>%
  mutate(denloc=trimws(gsub("MUNICIPIUL ","",denloc))) %>%
  left_join(siruta_cty) %>%
  # Manual disambiguation: two towns share the name CAMPULUNG; BIRA/BARA is a known SIRUTA encoding quirk
  mutate(denloc=case_when(denloc=="CAMPULUNG" & jud_name=="ARGES" ~ "CAMPULUNG MUSCEL",
                          denloc=="BIRA" & jud_name=="NEAMT" ~ "BARA",
                        TRUE ~ denloc))

# Unique town x county combinations present in the survey (upper-cased for case-insensitive join)
towns<-data %>%
  distinct(town,jud) %>%
  mutate(jud=toupper(jud))
# %>%
#   mutate(town=case_when(town=="CAMPULUNG MUSCEL" ~ "CAMPULUNG",
#                         town=="BARA" & jud=="NEAMT" ~ "BIRA",
#                         TRUE ~ town))

# Link survey towns to their official SIRUTA locality codes via name + county identifier
match<-towns %>%
  inner_join(siruta_towns,by=c("town"="denloc","jud"="jud_name"))

#check that all is matched
# Any row in towns not found in siruta_towns would signal a naming mismatch requiring a further case_when fix
unmatched<-towns %>%
  anti_join(match)

# --- Compute Roma Population Share from 2011 Census ---

#
#load 2011 census
setwd(wd_data_11)
data_2011<-fread("data_2011_clean.csv")

# ET: raw ethnicity code; Roma codes occupy the range 1200–1299 in the 2011 classification
# roma: share of the locality's census population that self-identified as Roma (used as a locality-level control)
roma_sirsup<-data_2011 %>%
  group_by(SIRSUP) %>%
  summarise(roma=mean(ET %in% 1200:1299,na.rm=T))

# Median Roma share across localities — threshold for the above/below-median heterogeneity split in Table 04
roma_median <- median(roma_sirsup$roma, na.rm = TRUE)

# add indicator above/below
# roma_above_median: binary indicator used to split the Table 04 sample into high- vs low-Roma-share localities
roma_sirsup <- roma_sirsup %>%
  mutate(roma_above_median = if_else(roma > roma_median, 1, 0))



# --- Attach Roma Share to Matched Survey Localities ---

#sirsup roma
# siruta (the integer SIRUTA code in the match table) corresponds to SIRSUP in the 2011 census
match<-match %>%
  left_join(roma_sirsup,by=c("siruta"="SIRSUP"))
# Retain only the columns needed downstream: locality identifier, county, Roma share, and the median indicator
match<-match %>%
  select(town,jud,roma,roma_above_median)

# --- Save Output ---

setwd(wd_data_11_other)
# Output: siruta3_roma_survey.csv — locality-level Roma share file read by Table 04 survey.R
fwrite(match, "siruta3_roma_survey.csv")
