# =====================================================================
# Read and clean the 2011 census microdata ("full" sample)
# Produces:  data_2011_full.csv  (intermediate; written to wd_data_11)
# Inputs:    raw 2011 census DBF files (PPL* person records, siruta.DBF
#            locality table) plus the 2011 SIRUTA mapping CSV
# Summary:   Reads all PPL person-level DBF files, selects/renames the needed
#            variables, joins the SIRUTA table for MEDIU (urban/rural) and
#            SIRSUP, resolves LOCN, collapses Bucharest sectors (county 40-46)
#            into county 40, and writes a combined CSV. No paper exhibit.
# =====================================================================

# ---- Read lookup / code tables ----
setwd(wd_data_raw)
setwd("./RPL_2011/")
# Other raw 2011 census tables (not needed here) live alongside the SIRUTA file:
#   RPL_2011/microdate_rpl_2011_dbf/ -> CETATENIE11.DBF (citizenship)
#   rpl_2011_microdate/NOM11/        -> ACTIVITATE11.dbf (activity)
#   rpl_2011_microdate/DOM11/ LOC11/ COMUN11/ CLAD11/ -> dwelling / locality /
#                                       commune / building tables
#   judete.dta -> county lookup.

setwd(wd_data_raw)
setwd("./RPL_2011/nomenclatoare/")
siruta_11<-read.dbf("siruta.DBF") %>%
  select(SIRUTA,SIRSUP,MEDIU=MED)



setwd(wd_data_11_other)
siruta_mapping<-fread('siruta.CSV') %>%
  select(siruta,sirsup,jud)


setwd(wd_data_raw)
setwd("./RPL_2011/microdate_rpl_2011_dbf/")


# ---- Read and combine person-level DBF files ----
#do the PPl files
# List all DBF files starting with "PPL"
files <- list.files(pattern = "pers.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
data_2011_full <- bind_rows(data_list)

#remove people without bdays and people without households assigned to them
data_2011_full_v2<-data_2011_full %>%
  filter(!is.na(AA) & !is.na(LL) & !is.na(ZZ) & GID!=0)

# ---- Select variables, attach locality codes, collapse Bucharest sectors ----
data_2011_full_v2<-data_2011_full_v2 %>%
  select(-matches("X.|^X$")) %>%
  select(JUD,
         SIRUTA,
         MEDIU,
         ID,
         ALTARES,
        MAPA,
        LITERA,
        NRF,
         GOSP,
         NOP,
         AA_SLR,
         LOCN,
        JLOCN,
        RESA,
        FPRSCA,
        MASTA,
        LOCN_S,
         # LOCN_URB,
         # JLOCN=P101,
         AA,
         LL,
         ZZ,
         SEX,
         SCA,
         SCU,
         STATUT,
         SECT,
         ET,
         LIM,
         REL,
         COPII,
         GRUD,
         POPLOC=TA,
         MOMLOC=MA,
         OCUP,
         SPLOC=SOPA,
        ANC1,
        ANC2,
        SIREC,
        AA_ALM,
        LL_ALM,
        DIF,
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
        VEDERE_C,
        AUZ_C,
        MERS_C,
        MEMO_C,
        INGR_C,
        COM_C,
        AJ
         )  %>%
  left_join(siruta_11 %>% select(SIRUTA,SIRSUP) ,by=c("SIRUTA"="SIRUTA")) %>% #add mediu and sirsup
  mutate(LOCN=case_when(LOCN==900 ~ SIRUTA,
                        TRUE ~ LOCN
  )) %>%
  left_join(siruta_11 %>% 
              select(SIRUTA,LOCN_URB=MEDIU),
            by=c("SIRUTA"="SIRUTA")) %>%
  mutate(JUD_SECT=JUD) %>%
  mutate(JUD=ifelse(JUD %in% 40:46,40,JUD)) %>%
  mutate(source="full")




# ---- Write combined 2011 "full" census file ----
setwd(wd_data_11)
fwrite(data_2011_full_v2,"data_2011_full.csv")
