# Reads 2011 census full population (DBF); outputs data_2011_full.csv

# --- Setup: navigate to 2011 census raw data folder ---
setwd(wd_data_raw)
setwd("./RPL_2011/")
# counties<-read_dta("judete.dta")

 # setwd(wd_data_raw)
 # setwd("./rpl_2011_microdate/NOM11/")
# activity<-read.dbf("ACTIVITATE11.dbf")

# setwd(wd_data_raw)
# setwd("./rpl_2011_microdate/DOM11/")
# dom<-read.dbf("dom_11.DBF")
#
# setwd(wd_data_raw)
# setwd("./rpl_2011_microdate/LOC11/")
# dom<-read.dbf("loc01_11.dbf")
#
# setwd(wd_data_raw)
# setwd("./rpl_2011_microdate/LOC11/")
# dom<-read.dbf("loc01_11.dbf")
#
# setwd(wd_data_raw)
# setwd("./rpl_2011_microdate/COMUN11/")
# dom<-read.dbf("comun01_11.dbf")
#
# setwd(wd_data_raw)
# setwd("./rpl_2011_microdate/CLAD11/")
# dom<-read.dbf("clad01_11.dbf")


# --- Load citizenship file (used for household/person linkage metadata) ---
setwd(wd_data_raw)
setwd("./RPL_2011/microdate_rpl_2011_dbf/")
# cet: citizenship register; links individuals to citizenship status, used for auxiliary checks
cet<-read.dbf("CETATENIE11.DBF")

# --- Load 2011 SIRUTA locality nomenclature ---
setwd(wd_data_raw)
setwd("./RPL_2011/nomenclatoare/")
# siruta_11: maps each SIRUTA locality code to its LAU2 superior code (SIRSUP) and urban/rural status (MEDIU)
# SIRSUP is needed to merge records across census years (SIRUTA codes change; SIRSUP is stable)
siruta_11<-read.dbf("siruta.DBF") %>%
  select(SIRUTA,SIRSUP,MEDIU)



# --- Load cross-year SIRUTA mapping (links 2011 codes to county identifiers) ---
setwd(wd_data_11_other)
# siruta_mapping: external CSV reconciling SIRUTA codes across census waves; jud = county NUTS3 code
siruta_mapping<-fread('siruta.CSV') %>%
  select(siruta,sirsup,jud)


# --- Navigate to 2011 microdata DBF folder for person-level files ---
setwd(wd_data_raw)
setwd("./RPL_2011/microdate_rpl_2011_dbf/")



#do the PPl files
# List all DBF files starting with "PPL"
# PPL files are the 2011 census stable (full) population files, one per county
files <- list.files(pattern = "PPL.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
# Result is the raw 2011 full-population microdata, covering all counties
data_2011_full <- bind_rows(data_list)

# --- Variable selection and renaming ---
# Drop spurious columns (DBF padding columns named "X.", "X") then retain only the variables
# needed for analysis; rename household-linkage pointers to POPLOC/MOMLOC/SPLOC (matching
# the naming convention used in 1992 and 2002 files) so downstream merge code is uniform.
data_2011_full<-data_2011_full %>%
  select(-matches("X.|^X$")) %>%
  select(JUD,
         SIRUTA,
         MEDIU,
        MAPA,
        LITERA,
        NRF,
         GOSP,
         # NOP: number of persons in household
         NOP,
         # AA_SLR: year of last change of residence (used to classify residential mobility)
         AA_SLR,
         LOCN,
        RESA,
        FPRSCA,
        MASTA,
        LOCN_S,
         # LOCN_URB,
         # JLOCN=P101,
         # AA: birth year (used to construct the matching cell and to define cohorts)
         AA,
         # LL: sex code (1 = male, 2 = female); combined with AA and ZZ to form cell_id
         LL,
         # ZZ: raw ethnicity code (12 = Roma); the key outcome variable drives ROMA_2011
         ZZ,
         SEX,
         # SCA: education level from the 1992/2002 form; kept here for comparability checks
         SCA,
         # SCU: education level from the 2011 form (different ordinal scale than SCA)
         SCU,
         STATUT,
         SECT,
         # ET: same ethnicity code as ZZ, used in figure scripts
         ET,
         # LIM: mother tongue code; used to construct ROMA_LANG (language-based Roma flag)
         LIM,
         REL,
         COPII,
         GRUD,
         # POPLOC (raw: TA): household line number of father; 0 if father not in household
         POPLOC=TA,
         # MOMLOC (raw: MA): household line number of mother; 0 if mother not in household
         MOMLOC=MA,
         OCUP,
         # SPLOC (raw: SOPA): household line number of spouse; 0 if not present
         SPLOC=SOPA,
        ANC1,
        ANC2,
        SIREC,
        AA_ALM,
        LL_ALM,
        DIF,
        # Disability variables (VEDERE_N to COM_N): natural/lifelong impairments
        VEDERE_N,
        AUZ_N,
        MERS_N,
        MEMO_N,
        INGR_N,
        COM_N,
        VARSTA,
        PFI,
        TIMPLP,
        STAP,
        FPSS,
        # Disability variables (VEDERE_C to COM_C): acquired impairments
        VEDERE_C,
        AUZ_C,
        MERS_C,
        MEMO_C,
        INGR_C,
        COM_C,
        AJ
         )  %>%
  left_join(siruta_11 ,by=c("SIRUTA"="SIRUTA")) %>% #add mediu and sirsup
  # LOCN==900 is the DBF sentinel value meaning "same locality as residence"; replace with actual SIRUTA
  mutate(LOCN=case_when(LOCN==900 ~ SIRUTA,
                        TRUE ~ LOCN
  )) %>%
  # LOCN_URB: urban/rural status of the locality of enumeration (from siruta_11 via SIRUTA)
  left_join(siruta_11 %>%
              select(SIRUTA,LOCN_URB=MEDIU),
            by=c("SIRUTA"="SIRUTA")) %>%
  # JUD_SECT preserves the original county code before Bucharest sectors are collapsed
  mutate(JUD_SECT=JUD) %>%
  # Bucharest has 6 sector codes (40-46); collapse all to 40 so the city is treated as one unit
  mutate(JUD=ifelse(JUD %in% 40:46,40,JUD)) %>%
  # source flag distinguishes stable-population records from "common" and "mig" files in later stacks
  mutate(source="full")


# --- Output: write cleaned 2011 full-population file ---
setwd(wd_data_11)
fwrite(data_2011_full,"data_2011_full.csv")

