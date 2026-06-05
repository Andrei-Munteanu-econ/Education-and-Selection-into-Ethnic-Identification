# Reads 2011 census migrant/temporarily-absent population (DBF); outputs data_2011_migrants.csv
# --- Setup: Navigate to 2011 Census Raw Data Folder ---
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


# --- Load Citizenship Table (used for coverage checks, not merged here) ---
setwd(wd_data_raw)
setwd("./RPL_2011/microdate_rpl_2011_dbf/")
# cet: citizenship codes for 2011 census respondents; loaded for reference/diagnostics
cet<-read.dbf("CETATENIE11.DBF")

# --- Load 2011 SIRUTA Locality Nomenclature ---
setwd(wd_data_raw)
setwd("./RPL_2011/nomenclatoare/")
# siruta_11: maps each year-specific SIRUTA locality code to its stable SIRSUP supralocality
# code and MEDIU (urban/rural indicator); SIRSUP is required for cross-year merging
siruta_11<-read.dbf("siruta.DBF") %>%
  select(SIRUTA,SIRSUP,MEDIU)


# --- Read Migrant / Temporarily-Absent Population DBF Files ---
setwd(wd_data_raw)
setwd("./RPL_2011/microdate_rpl_2011_dbf/")



#do the PPl files
# List all DBF files starting with "PPL"
# PPI files contain the migrant/temporarily-absent population (source = "mig");
# these individuals were enumerated at their locality of usual residence even though
# they were physically absent at the time of the census — a distinct universe from
# the common-dwelling (PPL) and full stable-population files read elsewhere
files <- list.files(pattern = "PPI.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
data_2011_full <- bind_rows(data_list)

# --- Select Key Variables and Merge Locality Metadata ---
data_2011_full<-data_2011_full %>%
  # Drop unnamed filler columns that DBF format sometimes appends (artifact of the DBF reader)
  select(-matches("X.|^X$")) %>%
  select(JUD,       # county code
         SIRUTA,    # year-specific LAU2 locality code (mapped to SIRSUP below)
         MEDIU,     # urban/rural indicator from the raw DBF
        MAPA,       # map sheet identifier
        LITERA,     # enumeration district letter
        NRF,        # building/address sequential number
         GOSP,      # household serial number within locality (combined with SIRUTA to form HHID)
        ID,         # line number within household (used to resolve MOMLOC/POPLOC/SPLOC links)
         AA,        # birth year (used to derive age and construct cell_id for cross-census matching)
         LL,        # sex (1 = male, 2 = female)
         ZZ,        # raw ethnicity code (12 = Roma; used to define ROMA flag)
         SEX,       # sex variable (alternative encoding; kept alongside LL for compatibility)
        ET,         # ethnicity code (same as ZZ; used in figure scripts)
        GRUD,       # household relationship code
        SPLOC,      # household line number of spouse (0 if absent; used for household structure)
        MOMLOC,     # household line number of mother (0 if not present in household)
        AA_SLR,     # year of last change of residence (identifies recent movers for the mover category)
        NIVED       # education level code (corresponds to SCU in 2011 census documentation)
        )  %>%
  left_join(siruta_11 ,by=c("SIRUTA"="SIRUTA")) %>% #add mediu and sirsup
  # Second join brings in LOCN_URB: an additional urban/rural indicator at the SIRUTA level,
  # used to distinguish between the individual's own locality type and a supralocality type
  left_join(siruta_11 %>%
              select(SIRUTA,LOCN_URB=MEDIU),
            by=c("SIRUTA"="SIRUTA")) %>%
  # JUD_SECT preserves the original county code including Bucharest sector codes (40-46);
  # the next line collapses all Bucharest sectors into a single county code (40)
  mutate(JUD_SECT=JUD) %>%
  mutate(JUD=ifelse(JUD %in% 40:46,40,JUD)) %>%
  # Tag all records from this file as migrants/temporarily-absent; used downstream
  # to distinguish from "common" and "full" population sources when stacking census files
  mutate(source="mig")


# --- Output: Write Migrant Population to Processed Data Folder ---
# Saved as data_2011_full.csv in wd_data_11; consumed by the main 2011 assembly script
setwd(wd_data_11)
fwrite(data_2011_full,"data_2011_full.csv")

