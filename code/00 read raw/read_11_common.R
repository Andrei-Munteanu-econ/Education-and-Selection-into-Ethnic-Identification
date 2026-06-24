# =====================================================================
# Read and clean the 2011 census microdata (population in common dwellings)
# Produces:  data_2011_common.csv  (intermediate; written to wd_data_11)
# Inputs:    raw 2011 census DBF files (pers_lc_1* common-dwelling person
#            records, siruta.DBF locality table) plus the 2011 SIRUTA
#            mapping CSV
# Summary:   Same pipeline as read_11_full.R but reads the common-dwellings
#            person files (pers_lc_1*); selects/renames variables, joins the
#            SIRUTA table, resolves LOCN, collapses Bucharest sectors into
#            county 40, and writes a combined CSV. No paper exhibit.
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



# ---- Read and combine person-level DBF files (common dwellings) ----
#do the PPl files
# List all DBF files starting with "pers_lc"
files <- list.files(pattern = "pers_sc.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
data_2011_full <- bind_rows(data_list)

# ---- select + harmonize ----
data_2011_full <- data_2011_full %>%
  select(-matches("X.|^X$")) %>%
  select(JUD,
         SIRUTA,
         MEDIU,
         MAPA,
         CLAD_COMMON = LITERA,   # <-- confirm (see note)
         FORM_COMMON = NRF,      # <-- confirm (see note)
         NOP,
         ID,
         ALTARES,
         AA_SLR,
         AA, LL, ZZ,
         SEX,
         LOCN,
         JLOCN,
         LOCN_S,
         RESA,
         STATUT, OCUP, SECT,
         SCA, SCU, FPRSCA, MASTA,
         COPII,
         ET, LIM, REL,
         ANC1, ANC2,
         SIREC,
         AA_ALM, LL_ALM,
         DIF,
         VEDERE_N, AUZ_N, MERS_N, MEMO_N, INGR_N, COM_N,
         VARSTA, PFI, TIMPLP, STAP, FPSS,
         VEDERE_C, AUZ_C, MERS_C, MEMO_C, INGR_C, COM_C,
         AJ) %>%
  left_join(siruta_11 %>% select(SIRUTA, SIRSUP),                # add SIRSUP only
            by = c("SIRUTA" = "SIRUTA")) %>%
  mutate(LOCN = case_when(LOCN == 900 ~ SIRUTA, TRUE ~ LOCN)) %>%
  left_join(siruta_11 %>% select(SIRUTA, LOCN_URB = MEDIU),      # add LOCN_URB
            by = c("SIRUTA" = "SIRUTA")) %>%
  mutate(JUD_SECT = JUD) %>%                                     # preserve sector
  mutate(JUD = ifelse(JUD %in% 40:46, 40, JUD)) %>%             # collapse Bucharest
  mutate(source = "common")


# ---- Write combined 2011 "common dwellings" census file ----
setwd(wd_data_11)
fwrite(data_2011_full,"data_2011_common_bkp.csv")

x<-fread("data_2011_common.csv")

