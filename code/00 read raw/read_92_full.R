# --- 1992 Census: Stable Population File ---
# Reads, renames, and cleans the 1992 Romanian census microdata for the
# "stable population" (i.e., usual residents, as opposed to migrants).
# Output: data/processed/1992/data_1992_full.csv

#Read and clean the 1992 census (stable population)

# --- Load Geographic Reference Tables ---

#load counties
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/")
counties<-read_dta("judete.dta")

#Code for reading other parts of folder
# setwd(wd_data_raw)
# setwd("./rpl_1992_microdate/NOM92/")
# activity<-read.dbf("ACTIVITATE92.dbf")

# setwd(wd_data_raw)
# setwd("./rpl_1992_microdate/DOM92/")
# dom<-read.dbf("dom_92.DBF")
#
# setwd(wd_data_raw)
# setwd("./rpl_1992_microdate/LOC92/")
# dom<-read.dbf("loc01_92.dbf")
#
# setwd(wd_data_raw)
# setwd("./rpl_1992_microdate/COMUN92/")
# dom<-read.dbf("comun01_92.dbf")
#
# setwd(wd_data_raw)
# setwd("./rpl_1992_microdate/CLAD92/")
# dom<-read.dbf("clad01_92.dbf")

#load foreign country codes
# cet: citizenship/nationality codes used to identify foreign nationals (excluded from main analysis)
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/NOM92/")
cet<-read.dbf("CETATENIE92.DBF")

#load town codes
# siruta_92: 1992-vintage locality codes needed to map raw records to geographic units
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/SIRUTA92/")
siruta_92<-read.dbf("siruta_92.DBF")

# siruta (s7_92): building-level geographic crosswalk linking the MAPA building register
# number to its SIRUTA locality code; used below to attach SIRUTA to each person record
#
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/s7_92/")
siruta<-read.dbf("s7_92.DBF")

#load 2011 town codes
# siruta_mapping: maps 1992/2002 SIRUTA codes to the 2011 SIRSUP supralocality code (LAU2)
# and to the 2011 county identifier (jud), enabling cross-census locality merging
setwd(wd_data_11_other)
siruta_mapping<-fread('siruta.CSV') %>%
  select(siruta,sirsup,jud)

# --- Read Raw Population DBF Files ---

#set wd to population files
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/POP92")


# List all DBF files starting with "PPL"
# Each file corresponds to one county; the naming convention PPL<county>.DBF is
# standard for the 1992 Romanian census microdata release
files <- list.files(pattern = "PPL.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
# data_1992_full_v2: raw, unprocessed union of all county-level population files
data_1992_full_v2 <- bind_rows(data_list)

# --- Rename Variables and Apply Initial Recodes ---
#clean data
# Rename cryptic census field codes to the harmonised variable names used
# throughout the replication package (see glossary in paper appendix)
data_1992_full_v3<-data_1992_full_v2 %>%
  # Drop auxiliary trailing columns that are artefacts of the DBF format
  select(-matches("X.|X$")) %>%
  select(MAPA=NRMAPA,       # building register number (links to s7_92 crosswalk for SIRUTA)
         CLAD=NRCLADIRE,    # building serial number within locality
         LOC=NRLOCUINTA,    # dwelling number within building
         GOSP=P00,          # household serial number within dwelling
         NOP=PNR,           # person line number within household
         AA_SLR=P05,        # year of last change of residence (99 = missing)
         LOCN=P101,         # LAU2 code of previous residence locality
         LOCN_URB=P102,     # urban/rural classification of previous locality
         JLOCN=P101,        # county of previous residence (overwritten below for special codes)
         AA=P091,           # birth year (stored as 2-digit offset; 999 = missing)
         LL=P092,           # sex: 1 = male, 2 = female
         ZZ=P093,           # raw ethnicity code (12 = Roma; see glossary)
         SEX=P08,           # sex (duplicate field retained for compatibility)
         SCA=P181,          # highest education level completed (1992 scale)
         SCU=P182,          # vocational/apprenticeship education detail
         STATUT=P19,        # labour-market status (0 = missing)
         SECT=P23,          # economic sector of activity
         ET=P15,            # ethnicity code (same encoding as ZZ; used in figure scripts)
         LIM=P16,           # mother tongue / language spoken at home
         REL=P17,           # religion
         COPII=P13,         # number of children born (women only)
         GRUD=P01,          # household type / relationship to head
         POPLOC=P071,       # line number of father within household (0 = absent)
         MOMLOC=P072,       # line number of mother within household (0 = absent)
         OCUP=P20,          # occupation code
         SPLOC=P073         # line number of spouse within household (0 = absent)
         ) %>%
  # AA_SLR == 99 is the raw missing code; recode to NA so numeric operations are clean
  mutate(AA_SLR=ifelse(AA_SLR==99,NA,AA_SLR)) %>%
  # LOCN == 90 means "same county as current residence"; substitute the county code (JUD)
  # LOCN == 99 means the previous locality is unknown; recode to NA
  mutate(JLOCN=case_when(LOCN==99 ~ NA,
                         LOCN==90 ~ JUD,
                         TRUE ~ JLOCN)) %>%
  # Birth year is stored as a 2-digit number (e.g., 72 → 1972); add 1000 to recover
  # the correct 4-digit year. 999 was used as a missing code and must not become 1999.
  mutate(AA=case_when(AA==999 ~ NA, # before, 999 was not changed to NA -> 1999
                      TRUE ~ 1000+AA)) %>%
  # Retain only valid ethnicity codes (1–31 span all recognised groups in the 1992 form);
  # codes outside this range are data errors and are set to NA
  mutate(ZZ=case_when(ZZ %in% 1:31 ~ ZZ, # before, this was not done
                      TRUE ~ NA)) %>%
  # Labour-market status: 0 is the raw missing code in this vintage; recode to NA
  mutate(STATUT=case_when(STATUT==0 ~ NA, # changed 0 to NA
                         TRUE ~ STATUT)) %>%
  # Sector of activity: 0 is missing; code 1 maps to primary sector (consistent with 2011)
  mutate(SECT=case_when(SECT==0 ~ NA, # does not really match 2011
                        SECT==1 ~ 1,
                          TRUE ~ SECT)) %>%
  # Attach SIRUTA locality code via the building-level crosswalk (MAPA → SIRUTA)
  left_join(siruta %>% select(MAPA,SIRUTA)) %>%
  # Map 1992 SIRUTA to the 2011 SIRSUP and county code so that 1992 records can be
  # merged with 2002 and 2011 records using a consistent geographic identifier
  left_join(siruta_mapping ,by=c("SIRUTA"="siruta")) %>% #this inputs the 2011 county (correct one)
  rename(SIRSUP=sirsup,
         JUD=jud)  %>%
  # Apply the same special-code logic to LOCN itself (was done to JLOCN above)
  mutate(LOCN=case_when(LOCN==90 ~ JUD,
                        LOCN==99 ~ NA,
                        TRUE ~ LOCN
  )) %>%
  # Tag all records from this file so downstream stacking can identify the data source
  # source == "full" = stable population (as opposed to "mig" for migrants)
  mutate(source="full")


# --- Save Cleaned 1992 Stable Population File ---
# Output: data_1992_full.csv — used as the base file for 1992 cohort construction
#save data
setwd(wd_data_92)
fwrite(data_1992_full_v3,"data_1992_full.csv")
