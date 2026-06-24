# =====================================================================
# Read and clean the 1992 census microdata (population in common dwellings)
# Produces:  data_1992_common.csv  (intermediate; written to wd_data_92)
# Inputs:    raw 1992 census DBF files (PPC01* person records for collective/
#            common dwellings, SIRUTA/locality code tables) plus the 2011
#            SIRUTA mapping CSV
# Summary:   Same pipeline as read_92_full.R but reads the "common dwellings"
#            (institutional/collective) person files (PPC01*). Renames raw
#            P-codes, recodes special values, attaches locality and 2011
#            county codes, and writes a combined CSV. No paper exhibit.
# =====================================================================

# ---- Read lookup / code tables ----
#load counties
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/")
counties<-read_dta("judete.dta")

# Other raw 1992 census tables (not needed here) live under rpl_1992_microdate/:
#   NOM92/    -> ACTIVITATE92.dbf (activity), CETATENIE92.DBF (citizenship)
#   DOM92/ LOC92/ COMUN92/ CLAD92/ -> dwelling / locality / commune / building tables

#load town codes
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/SIRUTA92/")
siruta_92<-read.dbf("siruta_92.DBF")

setwd(wd_data_raw)
setwd("./rpl_1992_microdate/s7_92/")
siruta<-read.dbf("s7_92.DBF")

#load 2011 town codes
setwd(wd_data_11_other)
siruta_mapping<-fread('siruta.CSV') %>%
  select(siruta,sirsup,jud)

#set wd to population files
setwd(wd_data_raw)
setwd("./rpl_1992_microdate/POP92")



# ---- Read and combine person-level DBF files (common dwellings) ----
# List the common-dwelling person DBF files (PPC01*)
files <- list.files(pattern = "PPC.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
data_1992_full<- bind_rows(data_list)

# ---- Recode / clean variables and attach locality codes ----
# Rename raw census P-codes to harmonized names, fix special/missing codes,
# and join SIRUTA (locality) and 2011 county codes.
data_1992_full<-data_1992_full %>%
  select(-matches("X.|X$")) %>%
  select(MAPA=NRMAPA,
         # CLAD=NRCAMIN,
         LOC=NRCAMIN,
         GOSP=NRFOR,
         NOP=PNR,
         AA_SLR=P05,
         LOCN=P101,
         LOCN_URB=P102,
         # JLOCN=P101,
         AA=P091,
         LL=P092,
         ZZ=P093,
         SEX=P08,
         SCA=P181,
         SCU=P182,
         STATUT=P19,
         SECT=P23,
         ET=P15,
         LIM=P16,
         REL=P17,
         COPII=P13,
         # GRUD=P01,
         # POPLOC=P071,
         # MOMLOC=P072,
         OCUP=P20#,
         # SPLOC=P073
         ) %>%
  mutate(AA_SLR=ifelse(AA_SLR==99,NA,AA_SLR)) %>%
  # mutate(JLOCN=case_when(LOCN==99 ~ NA,
  #                        LOCN==90 ~ JUD,
  #                        TRUE ~ JLOCN)) %>%
  mutate(AA=case_when(AA==999 ~ NA, # before, 999 was not changed to NA -> 1999
                      TRUE ~ 1000+AA)) %>%
  mutate(ZZ=case_when(ZZ %in% 1:31 ~ ZZ, # before, this was not done
                      TRUE ~ NA)) %>%
  mutate(STATUT=case_when(STATUT==0 ~ NA, # changed 0 to NA
                         TRUE ~ STATUT)) %>%
  mutate(SECT=case_when(SECT==0 ~ NA, # does not really match 2011
                        SECT==1 ~ 1,
                          TRUE ~ SECT)) %>%
  left_join(siruta %>% select(MAPA,SIRUTA)) %>%
  left_join(siruta_mapping ,by=c("SIRUTA"="siruta")) %>% #this inputs the 2011 county (correct one)
  rename(SIRSUP=sirsup,
         JUD=jud)  %>%
  mutate(LOCN=case_when(LOCN==90 ~ JUD,
                        LOCN==99 ~ NA,
                        TRUE ~ LOCN
  )) %>%
  mutate(source="common")



# ---- Write combined 1992 "common dwellings" census file ----
#save data
setwd(wd_data_92)
fwrite(data_1992_full,"data_1992_common.csv")




