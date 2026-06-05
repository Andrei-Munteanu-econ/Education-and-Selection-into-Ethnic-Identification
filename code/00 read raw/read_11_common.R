# Reads 2011 census common-dwellings population (DBF); outputs data_2011_common.csv
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
# setwd("./rpl_2011_microdate/COMUN11/")
# dom<-read.dbf("comun01_11.dbf")
#
# setwd(wd_data_raw)
# setwd("./rpl_2011_microdate/CLAD11/")
# dom<-read.dbf("clad01_11.dbf")


# --- Load citizenship supplement and locality crosswalk ---

# cet: citizenship file — not used in main analysis but read here for reference
setwd(wd_data_raw)
setwd("./RPL_2011/microdate_rpl_2011_dbf/")
cet<-read.dbf("CETATENIE11.DBF")

# siruta_11: official 2011 SIRUTA nomenclature; provides SIRSUP (LAU2 code) and MEDIU (urban/rural)
# SIRSUP is the stable locality identifier used for merging across census years
setwd(wd_data_raw)
setwd("./RPL_2011/nomenclatoare/")
siruta_11<-read.dbf("siruta.DBF") %>%
  select(SIRUTA,SIRSUP,MEDIU)


# siruta_mapping: alternative crosswalk (CSV) mapping year-specific SIRUTA codes to SIRSUP
# and county (jud); used alongside siruta_11 to ensure consistent locality identification
setwd(wd_data_11_other)
siruta_mapping<-fread('siruta.CSV') %>%
  select(siruta,sirsup,jud)


# --- Read 2011 common-dwellings person-level DBF files ---

setwd(wd_data_raw)
setwd("./RPL_2011/microdate_rpl_2011_dbf/")



#do the PPl files
# List all DBF files starting with "pers_lc"
# The 2011 census stores person records by county, each as a separate DBF named pers_lc_1*.dbf
files <- list.files(pattern = "pers_lc_1*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
# bind_rows handles minor column-type differences across county files
data_2011_full <- bind_rows(data_list)

# --- Select and rename variables needed for analysis ---
# Drop DBF padding columns (X. / X) and retain only analysis-relevant fields
data_2011_full<-data_2011_full %>%
  select(-matches("X.|^X$")) %>%
  select(JUD,
         JUD_SECT,
         SIRUTA,
         MEDIU,
        MAPA,
        LITERA,
        NRF,
         GOSP,
         NOP,
         # AA_SLR: year of last change of residence — used to classify residential mobility (stayers vs. movers)
         AA_SLR,
         LOCN,
        RESA,
        FPRSCA,
        MASTA,
        LOCN_S,
         # LOCN_URB,
         # JLOCN=P101,
         # AA: birth year — used to compute age and construct cell_id for cross-census matching
         AA,
         # LL: sex code (1 = male, 2 = female) — part of cell_id; also used to compute gender-specific statistics
         LL,
         # ZZ: raw ethnicity code (12 = Roma) — primary variable for defining ROMA flag
         ZZ,
         SEX,
         # SCA: education level code on the 1992/2002 scale (not used here but kept for harmonisation)
         SCA,
         # SCU: education level code on the 2011 scale — will be harmonised to EDUC and years_2011
         SCU,
         STATUT,
         SECT,
         # ET: ethnicity code (same content as ZZ; used in figure scripts)
         ET,
         # LIM: mother-tongue language code — used to construct ROMA_LANG (alternative ethnic definition, Table A.7)
         LIM,
         REL,
         COPII,
         GRUD,
         # POPLOC (renamed from TA): household line number of father (0 if absent) — used for parental co-residence checks
         POPLOC=TA,
         # MOMLOC (renamed from MA): household line number of mother (0 if absent)
         MOMLOC=MA,
         OCUP,
         # SPLOC (renamed from SOPA): household line number of spouse (0 if absent)
         SPLOC=SOPA,
        ANC1,
        ANC2,
        SIREC,
        AA_ALM,
        LL_ALM,
        DIF,
        # Disability severity indicators (numeric scale): vision, hearing, mobility, memory, self-care, communication
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
        # Disability category indicators (coded category): vision, hearing, mobility, memory, self-care, communication
        VEDERE_C,
        AUZ_C,
        MERS_C,
        MEMO_C,
        INGR_C,
        COM_C,
        AJ
         )  %>%
  left_join(siruta_11 ,by=c("SIRUTA"="SIRUTA")) %>% #add mediu and sirsup
  # LOCN==900 is a census sentinel meaning "same as current locality of residence"; replace with actual SIRUTA
  mutate(LOCN=case_when(LOCN==900 ~ SIRUTA,
                        TRUE ~ LOCN
  )) %>%
  # Attach urban/rural status of the locality of enumeration (LOCN_URB) by joining on SIRUTA
  left_join(siruta_11 %>%
              select(SIRUTA,LOCN_URB=MEDIU),
            by=c("SIRUTA"="SIRUTA")) %>%
  # Overwrite JUD_SECT with JUD before Bucharest sector recode, preserving the sector-level code
  mutate(JUD_SECT=JUD) %>%
  # Bucharest is split into six administrative sectors (codes 40–46) in the raw data;
  # recode all to JUD=40 so the capital is treated as one geographic unit in county-level analyses
  mutate(JUD=ifelse(JUD %in% 40:46,40,JUD))


# --- Output: write cleaned 2011 common-dwellings file ---
# data_2011_common.csv is the input for subsequent matching and analysis scripts
setwd(wd_data_11)
fwrite(data_2011_full,"data_2011_common.csv")
