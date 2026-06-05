# Reads 2002 census full (stable) population (DBF); outputs data_2002_full.csv

# --- Setup: Load Supporting Reference Tables ---

setwd(wd_data_raw)
setwd("./rpl_2002_microdate/")
# counties: Stata lookup table mapping county codes to county names (not used in the main merge below,
# but loaded here for reference/diagnostic checks)
counties<-read_dta("judete.dta")

# setwd(wd_data_raw)
# setwd("./rpl_2002_microdate/NOM02/")
# activity<-read.dbf("ACTIVITATE02.dbf")

# setwd(wd_data_raw)
# setwd("./rpl_2002_microdate/DOM02/")
# dom<-read.dbf("dom_02.DBF")
#
# setwd(wd_data_raw)
# setwd("./rpl_2002_microdate/LOC02/")
# dom<-read.dbf("loc01_02.dbf")
#
# setwd(wd_data_raw)
# setwd("./rpl_2002_microdate/COMUN02/")
# dom<-read.dbf("comun01_02.dbf")
#
# setwd(wd_data_raw)
# setwd("./rpl_2002_microdate/CLAD02/")
# dom<-read.dbf("clad01_02.dbf")


# cet: citizenship/nationality nomenclature table for 2002 (loaded for reference; not merged below)
setwd(wd_data_raw)
setwd("./rpl_2002_microdate/NOM02/")
cet<-read.dbf("CETATENIE02.DBF")

# siruta_02: official SIRUTA locality classification table for 2002 (loaded for reference)
setwd(wd_data_raw)
setwd("./rpl_2002_microdate/SIRUTA02/")
siruta_02<-read.dbf("siruta_02.DBF")

#map census numbers to SIRUTA
# siruta: links the internal census map unit (MAPA) to the official SIRUTA locality code;
# needed because the PPL person files use MAPA as locality identifier, not SIRUTA
setwd(wd_data_raw)
setwd("./rpl_2002_microdate/s7_02/")
siruta<-read.dbf("s7_02.DBF") %>%
  select(MAPA,SIRUTA) %>%
  distinct()


# siruta_mapping: maps 2002 SIRUTA codes to the 2011 SIRSUP (LAU2) codes and county (JUD);
# essential for linking 2002 records to 2011 records across censuses
setwd(wd_data_11_other)
siruta_mapping<-fread('siruta.CSV') %>%
  select(siruta,sirsup,jud)


# --- Load 2002 Full-Population (Stable Residents) Person Files ---

# The 2002 full-population data are split into county-level DBF files prefixed "PPL"
setwd(wd_data_raw)
setwd("./rpl_2002_microdate/POP02")



#do the PPl files
# List all DBF files starting with "PPL"
files <- list.files(pattern = "PPL.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
data_2002_full <- bind_rows(data_list)

# --- Select and Rename Variables; Merge Locality Codes ---

# Rename raw census field codes to interpretable variable names and drop filler columns;
# raw 2002 field names (P00, PNR, P12, …) follow the INS census questionnaire numbering
data_2002_full<-data_2002_full %>%
  select(-matches("X.|X$")) %>%
  select(MAPA=MAPA,
         CLAD,
         LOC,
         # GOSP: household serial number within locality (used to construct HHID)
         GOSP=P00,
         # NOP: person line number within household (used to resolve MOMLOC/POPLOC/SPLOC links)
         NOP=PNR,
         # AA_SLR: year of last change of residence (used to classify residential mobility)
         AA_SLR=P12,
         # LOCN: locality of previous residence code (origin locality for movers)
         LOCN=P13,
         LOCN_URB=P14,
         # JLOCN=P101,
         # AA: birth year (age proxy; used in cell_id construction for cross-census matching)
         AA=P16,
         # LL: sex (1 = male, 2 = female; used in cell_id construction)
         LL=P17,
         # ZZ: raw ethnicity code (12 = Roma; key outcome variable)
         ZZ=P18,
         SEX=P15,
         # SCA: education level code from 2002 census form (maps to harmonised EDUC and years_1992)
         SCA=P28,
         # SCU: secondary education subtype code in 2002 (distinguishes specialised vs. general HS)
         SCU=P30,
         # STATUT: marital status code
         STATUT=P32,
         # SECT: economic sector of main occupation
         SECT=P35,
         # ET: ethnicity code (same content as ZZ; used interchangeably in figure scripts)
         ET=P25,
         # LIM: mother tongue language code (used to construct ROMA_LANG alternative ethnic definition)
         LIM=P26,
         # REL: religion code
         REL=P27,
         # COPII: number of children born (for women)
         COPII=P22,
         # GRUD: household relationship to household head
         GRUD=P01,
         # POPLOC: person line number of father within the same household (0 if absent)
         POPLOC=P02,
         # MOMLOC: person line number of mother within the same household (0 if absent)
         MOMLOC=P03,
         OCUP=P34,
         # SPLOC: person line number of spouse within the same household (0 if absent)
         SPLOC=P04
  ) %>%
  left_join(siruta %>% select(MAPA,SIRUTA)) %>% #maps a mapa to a siruta
  left_join(siruta_mapping ,by=c("SIRUTA"="siruta")) %>% #this inputs the 2011 county - JUD (correct one)
  rename(SIRSUP=sirsup,
         JUD=jud)  %>%
  # Recode LOCN==90 (coded as "abroad/foreign") to the respondent's own county (JUD),
  # and set LOCN==99 (unknown/not applicable) to NA
  mutate(LOCN=case_when(LOCN==90 ~ JUD,
                        LOCN==99 ~ NA,
                        TRUE ~ LOCN
  )) %>%
  # JLOCN: county of previous residence, defined only when LOCN is a valid Romanian county code (1-52)
  mutate(JLOCN=case_when(LOCN<=52 ~ LOCN,
                        TRUE ~ NA
  )) %>%
  # Tag all records from this file as "full" (stable population), distinguishing from
  # "common" (common-dwelling) and "mig" (migrant/temporarily absent) sources
  mutate(source="full")



# --- Output: Save cleaned 2002 full-population file ---
#save data
setwd(wd_data_02)
fwrite(data_2002_full,"data_2002_full.csv")
