#Read and clean the 1992 census (population in common dwellings)

# --- Load County Reference Table ---
#load counties
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/")
# counties: administrative county (judet) codes and names, used for geographic joins
counties<-read_dta("judete.dta")

#Code for reading other parts of folder if necessary
# setwd(wd_data_raw)
# setwd("./rpl_1992_microdate/NOM92/")
# activity<-read.dbf("ACTIVITATE92.dbf")
#
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
#
# #load foreign country codes
# setwd(wd_data_raw)
# setwd("./rpl_1992_microdate/NOM92/")
# cet<-read.dbf("CETATENIE92.DBF")

# --- Load SIRUTA Locality Code Tables ---
#load town codes
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/SIRUTA92/")
# siruta_92: the official 1992 SIRUTA locality code table (year-specific locality identifiers)
siruta_92<-read.dbf("siruta_92.DBF")

setwd(wd_data_raw)
setwd("./rpl_1992_microdate/s7_92/")
# siruta: crosswalk table linking the map area code (MAPA/NRMAPA) used inside
# the 1992 population files to the official SIRUTA locality code
siruta<-read.dbf("s7_92.DBF")

#load 2011 town codes
# siruta_mapping: maps 1992 SIRUTA codes to 2011 SIRSUP (LAU2) codes and county (jud),
# enabling cross-census linkage of locality-level records despite nomenclature changes
setwd(wd_data_11_other)
siruta_mapping<-fread('siruta.CSV') %>%
  select(siruta,sirsup,jud)

# --- Read 1992 Population Microdata (Common Dwellings) ---
#set wd to population files
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/POP92")



# List all DBF files starting with "PPL"
# PPC01* files contain 1992 census records for persons in common (private) dwellings,
# split across multiple DBF files by geographic unit (county or region)
files <- list.files(pattern = "PPC01.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
data_1992_full<- bind_rows(data_list)

# --- Rename, Select, and Recode Variables ---
data_1992_full<-data_1992_full %>%
  # Drop artifact columns produced by read.dbf for empty trailing fields
  select(-matches("X.|X$")) %>%
  # Rename raw DBF field codes to meaningful variable names;
  # commented-out lines retain the mapping for reference but those fields are not used
  select(MAPA=NRMAPA,
         # CLAD=NRCAMIN,
         # LOC: internal locality index within the map area (MAPA)
         LOC=NRCAMIN,
         # GOSP: household serial number within the locality (used to construct HHID)
         GOSP=NRFOR,
         # NOP: person line number within the household
         NOP=PNR,
         # AA_SLR: year of last change of residence (used to classify residential mobility)
         AA_SLR=P05,
         # LOCN: locality code of previous residence (origin for migrants)
         LOCN=P101,
         LOCN_URB=P102,
         # JLOCN=P101,
         # AA: birth year (raw field stores only last 3 digits; recoded below to full year)
         AA=P091,
         # LL: sex (1 = male, 2 = female)
         LL=P092,
         # ZZ: ethnicity code (12 = Roma; used to define ROMA flag)
         ZZ=P093,
         SEX=P08,
         # SCA: education level code on the 1992/2002 census scale (used to derive years_1992)
         SCA=P181,
         # SCU: secondary education sub-code; combined with SCA to distinguish HS types
         SCU=P182,
         STATUT=P19,
         SECT=P23,
         # ET: ethnicity code (same content as ZZ; kept separately for figure scripts)
         ET=P15,
         # LIM: mother-tongue (language) code (used to construct ROMA_LANG)
         LIM=P16,
         REL=P17,
         COPII=P13,
         # GRUD=P01,
         # POPLOC=P071,
         # MOMLOC=P072,
         OCUP=P20#,
         # SPLOC=P073
         ) %>%
  # AA_SLR == 99 is the census "not applicable / never moved" sentinel; set to NA
  mutate(AA_SLR=ifelse(AA_SLR==99,NA,AA_SLR)) %>%
  # mutate(JLOCN=case_when(LOCN==99 ~ NA,
  #                        LOCN==90 ~ JUD,
  #                        TRUE ~ JLOCN)) %>%
  # Birth year is stored as last 3 digits (e.g., 960 = 1960); 999 = missing
  # Adding 1000 recovers the full 4-digit year; 999 → NA avoids a phantom year 1999
  mutate(AA=case_when(AA==999 ~ NA, # before, 999 was not changed to NA -> 1999
                      TRUE ~ 1000+AA)) %>%
  # ZZ codes outside 1-31 are invalid (non-standard or blank entries); set to NA
  mutate(ZZ=case_when(ZZ %in% 1:31 ~ ZZ, # before, this was not done
                      TRUE ~ NA)) %>%
  # STATUT == 0 is an uncodeable marital-status entry; treat as missing
  mutate(STATUT=case_when(STATUT==0 ~ NA, # changed 0 to NA
                         TRUE ~ STATUT)) %>%
  # SECT (economic sector): 0 is invalid; code 1 retained; higher codes kept as-is
  # Note: the 1992 sector coding does not map cleanly to the 2011 version
  mutate(SECT=case_when(SECT==0 ~ NA, # does not really match 2011
                        SECT==1 ~ 1,
                          TRUE ~ SECT)) %>%
  # Attach the official SIRUTA locality code via the map-area crosswalk
  left_join(siruta %>% select(MAPA,SIRUTA)) %>%
  # Map 1992 SIRUTA → 2011 SIRSUP and county code so records can be linked
  # to 2002 and 2011 censuses; this is the key cross-year geography bridge
  left_join(siruta_mapping ,by=c("SIRUTA"="siruta")) %>% #this inputs the 2011 county (correct one)
  rename(SIRSUP=sirsup,
         JUD=jud)  %>%
  # LOCN == 90 encodes "same county" (no finer locality known); replace with county code
  # LOCN == 99 is missing / not applicable
  mutate(LOCN=case_when(LOCN==90 ~ JUD,
                        LOCN==99 ~ NA,
                        TRUE ~ LOCN
  )) %>%
  # Tag all records from this file as coming from the "common dwellings" sub-universe
  # (distinguishes from "full" stable-population files and "mig" migrant files)
  mutate(source="common")



# --- Save Cleaned 1992 Common-Dwellings File ---
# Output: data_1992_common.csv — individual-level 1992 census records for persons
# in common dwellings, with harmonised variable names and 2011 locality codes attached
setwd(wd_data_92)
fwrite(data_1992_full,"data_1992_common.csv")


