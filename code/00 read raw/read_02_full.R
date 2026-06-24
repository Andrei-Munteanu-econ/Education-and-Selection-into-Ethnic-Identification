# =====================================================================
# Read and clean the 2002 census microdata ("full" sample)
# Produces:  data_2002_full.csv  (intermediate; written to wd_data_02)
# Inputs:    raw 2002 census DBF files (PPL* person records, SIRUTA/locality
#            code tables, county codes) plus the 2011 SIRUTA mapping CSV
# Summary:   Reads all PPL person-level DBF files, renames raw P-codes to
#            harmonized variable names, attaches locality (SIRUTA) and 2011
#            county codes, derives LOCN/JLOCN, and writes a combined CSV.
#            No paper exhibit.
# =====================================================================

# ---- Read lookup / code tables ----
# Raw 2002 census files live under rpl_2002_microdate/, one DBF per subfolder:
#   NOM02/    -> CETATENIE02.DBF (citizenship), ACTIVITATE02.dbf (activity)
#   SIRUTA02/ -> siruta_02.DBF   (locality SIRUTA codes)
#   s7_02/    -> s7_02.DBF       (census-number to SIRUTA map)
#   DOM02/ LOC02/ COMUN02/ CLAD02/ -> dwelling / locality / commune / building tables
#   judete.dta -> county lookup. Only the subset read below is used here.


setwd(wd_data_raw)
setwd("./rpl_2002_microdate/NOM02/")
cet<-read.dbf("CETATENIE02.DBF")

setwd(wd_data_raw)
setwd("./rpl_2002_microdate/SIRUTA02/")
siruta_02<-read.dbf("siruta_02.DBF")

#map census numbers to SIRUTA
setwd(wd_data_raw)
setwd("./rpl_2002_microdate/s7_02/")
siruta<-read.dbf("s7_02.DBF") %>%
  select(MAPA,SIRUTA) %>%
  distinct()
  

setwd(wd_data_11_other)
siruta_mapping<-fread('siruta.CSV') %>%
  select(siruta,sirsup,jud)


setwd(wd_data_raw)
setwd("./rpl_2002_microdate/POP02")



# ---- Read and combine person-level DBF files ----
#do the PPl files
# List all DBF files starting with "PPL"
files <- list.files(pattern = "PPL.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
data_2002_full <- bind_rows(data_list)

# ---- Recode / clean variables and attach locality codes ----
# Rename raw census P-codes to harmonized names and join SIRUTA + 2011 county.
data_2002_full<-data_2002_full %>%
  select(-matches("X.|X$")) %>%
  select(MAPA=MAPA,
         CLAD,
         LOC,
         GOSP=P00,
         NOP=PNR,
         AA_SLR=P12,
         LOCN=P13,
         LOCN_URB=P14,
         # JLOCN=P101,
         AA=P16,
         LL=P17,
         ZZ=P18,
         SEX=P15,
         SCA=P28,
         SCU=P30,
         STATUT=P32,
         SECT=P35,
         ET=P25,
         LIM=P26,
         REL=P27,
         COPII=P22,
         GRUD=P01,
         POPLOC=P02,
         MOMLOC=P03,
         OCUP=P34,
         SPLOC=P04
  ) %>%
  left_join(siruta %>% select(MAPA,SIRUTA)) %>% #maps a mapa to a siruta
  left_join(siruta_mapping ,by=c("SIRUTA"="siruta")) %>% #this inputs the 2011 county - JUD (correct one)
  rename(SIRSUP=sirsup,
         JUD=jud)  %>%
  mutate(LOCN=case_when(LOCN==90 ~ JUD,
                        LOCN==99 ~ NA,
                        TRUE ~ LOCN
  )) %>%
  mutate(JLOCN=case_when(LOCN<=52 ~ LOCN,
                        TRUE ~ NA
  )) %>%
  mutate(source="full")



# ---- Write combined 2002 "full" census file ----
#save data
setwd(wd_data_02)
# fwrite(data_2002_full,"data_2002_full_bkp.csv")
fwrite(data_2002_full,"data_2002_full.csv")
