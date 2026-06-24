# =====================================================================
# Helper for Figure A02 — per-locality sex and ethnicity proportions
# Produces:  data_1992_sex_ethnicity_proportion_per_cell.rds
#            data_2002_sex_ethnicity_proportion_per_cell.rds
#            data_2011_sex_ethnicity_proportion_per_cell_1992.rds
#            data_2011_sex_ethnicity_proportion_per_cell_2002.rds
# Inputs:    data_1992_clean.csv, data_2002_clean.csv, data_2011_clean_v2.csv
# Summary:   Called by Figure A02. For each town-cell, computes the female
#            share and the population share of Roma, German, Hungarian and
#            "other" groups (using the year-specific ethnicity codes). For
#            2011 these are computed within education group as well. The
#            resulting ratios let Figure A02 net out the chance of a
#            spurious cross-sex census link.
# =====================================================================

# ---- 1992: female share and group shares per town-cell ----
#get sex proportion for each cell
#################
setwd(wd_data_92)
data_1992<-fread('data_1992_clean.csv')
setDT(data_1992)

data_1992_v2 <- data_1992[
  ,
  .(
    cell_id_genderless,
    SEX,
    ROMA,
    ET
  )
][
  ,
  town_cell := sub("^([^-]+)-.*", "\\1", cell_id_genderless)
][
  ,
  {
    n_ROMA <- sum(ROMA == TRUE)
    n_GER  <- sum(ET %in% 13)
    n_HU   <- sum(ET %in% 11)
    n_OT   <- sum(ET %in% 13:90)
    
    .(
      p_female_ROMA = fifelse(n_ROMA > 0,
                              sum(SEX == 2 & ROMA == TRUE, na.rm = TRUE) / n_ROMA,
                              0),
      p_ROMA        = n_ROMA / .N,
      
      p_female_GER  = fifelse(n_GER > 0,
                              sum(SEX == 2 & ET %in% 13, na.rm = TRUE) / n_GER,
                              0),
      p_GER         = n_GER / .N,
      
      p_female_HU   = fifelse(n_HU > 0,
                              sum(SEX == 2 & ET %in% 11, na.rm = TRUE) / n_HU,
                              0),
      p_HU          = n_HU / .N,
      
      p_female_OT   = fifelse(n_OT > 0,
                              sum(SEX == 2 & ET %in% 13:90, na.rm = TRUE) / n_OT,
                              0),
      p_OT          = n_OT / .N
    )
  },
  by = town_cell
]

setwd(wd_data_linked)
saveRDS(data_1992_v2,'data_1992_sex_ethnicity_proportion_per_cell.rds')
rm(data_1992_v2)

# ---- 2002: female share and group shares per town-cell ----
############
setwd(wd_data_02)
data_2002<-fread('data_2002_clean.csv')
setDT(data_2002)

data_2002_v2 <- data_2002[
  ,
  .(
    cell_id_genderless,
    SEX,
    ROMA,
    ET
  )
][
  ,
  town_cell := sub("^([^-]+)-.*", "\\1", cell_id_genderless)
][
  ,
  {
    n_ROMA <- sum(ROMA == TRUE)
    n_GER  <- sum(ET %in% 1300:1399)
    n_HU   <- sum(ET %in% 1100:1199)
    n_OT   <- sum(ET %in% 1300:9000)
    
    .(
      p_female_ROMA = fifelse(n_ROMA > 0,
                              sum(SEX == 2 & ROMA == TRUE, na.rm = TRUE) / n_ROMA,
                              0),
      p_ROMA        = n_ROMA / .N,
      
      p_female_GER  = fifelse(n_GER > 0,
                              sum(SEX == 2 & ET %in% 1300:1399, na.rm = TRUE) / n_GER,
                              0),
      p_GER         = n_GER / .N,
      
      p_female_HU   = fifelse(n_HU > 0,
                              sum(SEX == 2 & ET %in% 1100:1199, na.rm = TRUE) / n_HU,
                              0),
      p_HU          = n_HU / .N,
      
      p_female_OT   = fifelse(n_OT > 0,
                              sum(SEX == 2 & ET %in% 1300:9000, na.rm = TRUE) / n_OT,
                              0),
      p_OT          = n_OT / .N
    )
  },
  by = town_cell
]

setwd(wd_data_linked)
saveRDS(data_2002_v2,'data_2002_sex_ethnicity_proportion_per_cell.rds')
rm(data_2002_v2)

# ---- 2011: female share and group shares per town-cell, by education ----
# Computed twice, keyed to the 1992 cell id and to the 2002 cell id, so the
# endline ratios can be merged onto either linked sample.
#######################
setwd(wd_data_11)
data_2011<-fread('data_2011_clean_v2.csv')
data_2011_92<-data_2011 %>%  
  select(cell_id_genderless_1992,SEX,ET,years,ROMA) %>%
  mutate(Education=case_when(years==0 ~ "None",
                             years %in% 2:4 ~ "Primary School",
                             years %in% 8:10  ~ "Middle School",
                             years %in% 12:13 ~ "High School or Vocational",
                             years %in% 14:16 ~ "Post- Secondary")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary"))) 

setDT(data_2011_92)
data_2011_92<-data_2011_92[ ,
.(
  cell_id_genderless_1992,
  SEX,
  ROMA,
  ET,
  Education
)
][
  ,
  town_cell := sub("^([^-]+)-.*", "\\1", cell_id_genderless_1992)
][
  ,
  {
    n_ROMA <- sum(ROMA == TRUE)
    n_GER  <- sum(ET %in% 1300:1399)
    n_HU   <- sum(ET %in% 1100:1199)
    n_OT   <- sum(ET %in% 1300:9000)
    
    .(
      p_female_ROMA = fifelse(n_ROMA > 0,
                              sum(SEX == 2 & ROMA == TRUE, na.rm = TRUE) / n_ROMA,
                              0),
      p_ROMA        = n_ROMA / .N,
      
      p_female_GER  = fifelse(n_GER > 0,
                              sum(SEX == 2 & ET %in% 1300:1399, na.rm = TRUE) / n_GER,
                              0),
      p_GER         = n_GER / .N,
      
      p_female_HU   = fifelse(n_HU > 0,
                              sum(SEX == 2 & ET %in% 1100:1199, na.rm = TRUE) / n_HU,
                              0),
      p_HU          = n_HU / .N,
      
      p_female_OT   = fifelse(n_OT > 0,
                              sum(SEX == 2 & ET %in% 1300:9000, na.rm = TRUE) / n_OT,
                              0),
      p_OT          = n_OT / .N
    )
  },
  by = .(town_cell,Education)
]


data_2011_02<-data_2011 %>%  
  select(cell_id_genderless_2002,ET,years,ROMA,SEX) %>%
  mutate(Education=case_when(years==0 ~ "None",
                             years %in% 2:4 ~ "Primary School",
                             years %in% 8:10  ~ "Middle School",
                             years %in% 12:13 ~ "High School or Vocational",
                             years %in% 14:16 ~ "Post- Secondary")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary")))


data_2011_02<-data_2011_02[ ,
                         .(
                           cell_id_genderless_2002,
                           SEX,
                           ROMA,
                           ET,
                           Education
                         )
][
  ,
  town_cell := sub("^([^-]+)-.*", "\\1", cell_id_genderless_2002)
][
  ,
  {
    n_ROMA <- sum(ROMA == TRUE)
    n_GER  <- sum(ET %in% 1300:1399)
    n_HU   <- sum(ET %in% 1100:1199)
    n_OT   <- sum(ET %in% 1300:9000)
    
    .(
      p_female_ROMA = fifelse(n_ROMA > 0,
                              sum(SEX == 2 & ROMA == TRUE, na.rm = TRUE) / n_ROMA,
                              0),
      p_ROMA        = n_ROMA / .N,
      
      p_female_GER  = fifelse(n_GER > 0,
                              sum(SEX == 2 & ET %in% 1300:1399, na.rm = TRUE) / n_GER,
                              0),
      p_GER         = n_GER / .N,
      
      p_female_HU   = fifelse(n_HU > 0,
                              sum(SEX == 2 & ET %in% 1100:1199, na.rm = TRUE) / n_HU,
                              0),
      p_HU          = n_HU / .N,
      
      p_female_OT   = fifelse(n_OT > 0,
                              sum(SEX == 2 & ET %in% 1300:9000, na.rm = TRUE) / n_OT,
                              0),
      p_OT          = n_OT / .N
    )
  },
  by = .(town_cell,Education)
]

setwd(wd_data_linked)
saveRDS(data_2011_92,'data_2011_sex_ethnicity_proportion_per_cell_1992.rds')
saveRDS(data_2011_02,'data_2011_sex_ethnicity_proportion_per_cell_2002.rds') 

# rm(data_2011)
# rm(data_1992)
# rm(data_2002)

