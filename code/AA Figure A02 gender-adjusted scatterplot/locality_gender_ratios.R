#This script is a helper script called by Figure A.2; it computes gender ratios
#per locality, which are used to adjust the results from Figure 01 to produce Figure A.2

# --- Motivation ---
# The main analysis uses cell_id_genderless (omitting sex from matching cells) to avoid
# conflating ethnic passing with gender-ratio differences across localities. Figure A.2
# checks robustness by re-estimating the passing rate after explicitly controlling for the
# female share within each ethnic group and locality. This script pre-computes those shares
# from each raw census file and saves them for use by the Figure A.2 plotting script.

# --- Section 1: 1992 Census Gender Ratios by Locality ---

#get sex proportion for each cell
#################
setwd(wd_data_92)
data_1992<-fread('data_1992_clean.csv')
setDT(data_1992)

# Retain only the columns needed for sex-by-ethnicity aggregation, then:
#   (1) extract the SIRSUP locality code from the leading segment of cell_id_genderless,
#   (2) collapse to locality level, computing female share and group share for each
#       of four ethnic categories: Roma, German, Hungarian, and Other minorities.
# town_cell: locality identifier extracted from the first '-'-delimited segment of cell_id_genderless
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
    # Count group members per locality before computing shares to avoid repeated sum() calls
    n_ROMA <- sum(ROMA == TRUE)
    # ET codes in 1992 census: 13 = German, 11 = Hungarian; 13:90 covers all non-Romanian minorities
    n_GER  <- sum(ET %in% 13)
    n_HU   <- sum(ET %in% 11)
    n_OT   <- sum(ET %in% 13:90)

    .(
      # p_female_ROMA: share of Roma in this locality who are female; 0 if no Roma present (avoids 0/0)
      p_female_ROMA = fifelse(n_ROMA > 0,
                              sum(SEX == 2 & ROMA == TRUE, na.rm = TRUE) / n_ROMA,
                              0),
      # p_ROMA: Roma share of the total local population in this matching cell
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

# Output: locality-level sex-by-ethnicity proportions from the 1992 census
setwd(wd_data_linked)
saveRDS(data_1992_v2,'data_1992_sex_ethnicity_proportion_per_cell.rds')
rm(data_1992_v2)

# --- Section 2: 2002 Census Gender Ratios by Locality ---

############
# Note: ET codes in 2002 use a 4-digit scheme (e.g., 1300-1399 for Germans, 1100-1199 for
# Hungarians) rather than the 2-digit codes used in the 1992 file. The logic is otherwise
# identical to Section 1.
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

# Output: locality-level sex-by-ethnicity proportions from the 2002 census
setwd(wd_data_linked)
saveRDS(data_2002_v2,'data_2002_sex_ethnicity_proportion_per_cell.rds')
rm(data_2002_v2)

# --- Section 3: 2011 Census Gender Ratios by Locality and Education Group ---

#######################
# The 2011 file is processed twice: once using 1992-vintage matching cells
# (cell_id_genderless_1992, for the 1992-to-2011 passing comparison) and once
# using 2002-vintage matching cells (cell_id_genderless_2002, for the
# 2002-to-2011 comparison). Crucially, gender ratios are broken out by Education
# group here (unlike the simpler locality-only aggregation above), because the
# gender-adjustment in Figure A.2 is applied within education cells to ensure
# the robustness check accounts for any education-gender correlation.
setwd(wd_data_11)
data_2011<-fread('data_2011_clean_v2.csv')
# Subset to the 1992-vintage matching key; recode continuous years-of-schooling
# into the five coarse education bins used throughout the paper's figures.
data_2011_92<-data_2011 %>%
  select(cell_id_genderless_1992,SEX,ET,years,ROMA) %>%
  mutate(Education=case_when(years==0 ~ "None",
                             years %in% 2:4 ~ "Primary School",
                             years %in% 8:10  ~ "Middle School",
                             years %in% 12:13 ~ "High School or Vocational",
                             years %in% 14:16 ~ "Post- Secondary")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary")))

setDT(data_2011_92)
# Aggregate to (town_cell x Education) cells; structure mirrors Sections 1-2 but
# adds Education as a second grouping key so the caller can adjust passing rates
# within narrow education-by-locality bins.
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


# Second pass: 2002-vintage matching key, for the 1992-2002 -> 2002-2011 panel comparison
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

# Output: locality-by-education gender ratios from the 2011 census, keyed to both
# the 1992 and 2002 vintage matching cells, for use by the Figure A.2 plotting script
setwd(wd_data_linked)
saveRDS(data_2011_92,'data_2011_sex_ethnicity_proportion_per_cell_1992.rds')
saveRDS(data_2011_02,'data_2011_sex_ethnicity_proportion_per_cell_2002.rds')

# rm(data_2011)
# rm(data_1992)
# rm(data_2002)

