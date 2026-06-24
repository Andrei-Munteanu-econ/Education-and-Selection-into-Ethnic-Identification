# =====================================================================
# Read and clean the 2011 census migrants file
# Produces:  data_2011_migrants.csv  (intermediate; written to wd_data_11 via the
#            fwrite call below; read back by 00_01clean.R as the "mig" source)
# Inputs:    raw 2011 census migrants DBF files (PPI* records, siruta.DBF
#            locality table)
# Summary:   Reads all PPI person-level DBF files (migrants), selects/renames
#            the needed variables, joins the SIRUTA table for MEDIU/SIRSUP,
#            collapses Bucharest sectors into county 40, tags source="mig",
#            and writes a CSV. No paper exhibit.
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


setwd(wd_data_raw)
setwd("./RPL_2011/microdate_rpl_2011_dbf/")



# ---- Read and combine person-level DBF files (migrants) ----
#do the PPl files
# List the migrant person DBF files (PPI*)
files <- list.files(pattern = "ppi.*\\.dbf$")

# Read all dbf files into a list
data_list <- lapply(files, read.dbf)

# Combine into one dataframe
data_2011_full <- bind_rows(data_list)

# ---- Select variables, attach locality codes, collapse Bucharest sectors ----
data_2011_full<-data_2011_full %>%
  select(-matches("X.|^X$")) %>%
  select(JUD,
         SIRUTA,
         MEDIU,
        MAPA,
        LITERA,
        NRF,
         GOSP,
        ID,
        AA,
        LL,
        ZZ,
         SEX,
        ET,
        GRUD,
        SPLOC=SOPA,
        MOMLOC=MA,
        POPLOC=TA,
        # AA_SLR,
        NIVED
        )  %>%
  left_join(siruta_11 %>% select(SIRUTA,SIRSUP) ,by=c("SIRUTA"="SIRUTA")) %>% #add mediu and sirsup
  left_join(siruta_11 %>% 
              select(SIRUTA,LOCN_URB=MEDIU),
            by=c("SIRUTA"="SIRUTA")) %>%
  mutate(JUD_SECT=JUD) %>%
  mutate(JUD=ifelse(JUD %in% 40:46,40,JUD)) %>%
  mutate(source="mig")


# ---- Write 2011 migrants file ----
# Written as data_2011_migrants.csv (NOT data_2011_full.csv, which read_11_full.R
# writes); 00_01clean.R reads both separately and stacks them with source tags.
setwd(wd_data_11)
fwrite(data_2011_full,"data_2011_migrants.csv")

