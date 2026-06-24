# =====================================================================
# Read and clean the 2002 census microdata (population in common dwellings)
# Produces:  data_2002_common.csv  (intermediate; written to wd_data_02)
# Inputs:    raw 2002 census DBF files (PPL* person records for common
#            dwellings, SIRUTA/locality code tables) plus the 2011 SIRUTA
#            mapping CSV
# Summary:   Same pipeline as read_02_full.R but for the common-dwellings
#            sample; renames raw P-codes, attaches locality and 2011 county
#            codes, and writes a combined CSV. No paper exhibit.
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
files <- list.files(pattern = "PPC.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
data_2002_full<- bind_rows(data_list)

# ---- Recode / clean variables and attach locality codes ----
# Rename raw census P-codes to harmonized names and join SIRUTA + 2011 county.
data_2002_full <- data_2002_full %>%
  select(-matches("X.|X$")) %>%
  select(MAPA,
         CLAD_COMMON = ULC,
         FORM_COMMON = NRPCPH,
         NOP   = PERS,
         AA_SLR = P12,
         LOCN  = P13,
         LOCN_URB = P14,
         AA = P16, LL = P17, ZZ = P18,
         SEX = P15,
         SCA = P28, SCU = P30, FPRSCA = P29,
         OCUP = P34, STATUT = P32, SECT = P38,
         ET = P25, LIM = P26, REL = P27,MEDIU=MDOM,
         COPII = P22) %>%
  left_join(siruta %>% select(MAPA, SIRUTA)) %>%          # s7_02: MAPA -> SIRUTA
  left_join(siruta_mapping, by = c("SIRUTA" = "siruta")) %>%
  rename(SIRSUP = sirsup, JUD = jud) %>%
  mutate(LOCN = case_when(LOCN == 90 ~ JUD,
                          LOCN == 99 ~ NA,
                          TRUE ~ LOCN)) %>%
  mutate(JLOCN = case_when(LOCN <= 52 ~ LOCN, TRUE ~ NA)) %>%
  mutate(source = "common")



# ---- Write combined 2002 "common dwellings" census file ----
#save data
setwd(wd_data_02)
fwrite(data_2002_full,"data_2002_common.csv")




