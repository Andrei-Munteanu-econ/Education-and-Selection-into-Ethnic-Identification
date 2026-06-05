# Cleans 2011 census microdata; merges common, full, and migrant files; outputs data_2011_clean.csv
#clean data

# --- 2011 Census Cleaning ---
#2011 ----
setwd(wd_data_11)
# read_sample() determines column types from a small sample; read_data() reads the full file with those types
filename<-'data_2011_full.csv'
# source='all' flags this sub-file as the stable/full-population register (vs. common dwellings or migrants)
data_2011_full<-read_sample(filename)
data_2011_full<-read_data(filename,data_2011_full)



filename<-'data_2011_common.csv'
data_2011_common<-read_sample(filename)
data_2011_common<-read_data(filename,data_2011_common)

filename<-'data_2011_migrants.csv'
data_2011_migrants<-read_sample(filename)
data_2011_migrants<-read_data(filename,data_2011_migrants)

# Stack all three sub-files; fill=T handles columns present in only one source
# source: "all" = full/stable population register, "common" = common dwellings, "mig" = temporarily absent/migrants
data_2011<-rbindlist(list(data_2011_full %>% mutate(source='all'),data_2011_common %>% mutate(source='common'),data_2011_migrants %>% mutate(source='mig')),fill=T)

###nomenclator
# School nomenclature files are used later to harmonise school codes across census rounds
setwd(wd_data_02_other)
school_2002<-read_dta('SCOALA02.DTA')
setwd(wd_data_92_other)
school_1992<-read.dbf('SCOALA92.DBF')
setwd(wd_data_11_other)
school_2011<-read.csv('nomenclator_scoli.CSV')


# --- Roma Flag ---
##Roma----
# ET is the ethnicity code; codes 1200-1299 cover all Roma sub-categories in the 2011 census coding scheme
# ROMA: 1 if self-identified as Roma in this census round
data_2011<-data_2011 %>%
  mutate(ROMA=ifelse(ET>=1200 & ET<1300,T,F))

# --- Residential Mobility Categories and Matching Cell Construction ---
##category----
# category classifies each person by residential history; this determines which locality code
# (current SIRSUP vs. origin RESA) to use when building the inter-census matching cell
# AA_SLR: year of last change of residence; used to infer which census round a move occurred in
# ALTARES==2 flags individuals enumerated at their permanent address (stayers)
data_2011<-data_2011 %>%
  mutate(category=case_when(
    source=='mig' & AA_SLR>=2002 ~ "migrant post-2002",
    source=='mig' & AA_SLR>=1992 & AA_SLR<2002  ~ "migrant 1992-2002",
    source=='mig' & AA_SLR<1992 ~ "migrant pre-1992",
    ALTARES==2 ~ "stayer",
    AA_SLR>=2002 ~ "mover post-2002",
    AA_SLR>=1992 & AA_SLR<2002  ~ "mover 1992-2002",
    AA_SLR<1992 ~ "mover pre-1992")) %>%
  # cell_id_2002: matching cell used to link the 2011 record back to 2002 census records
  # For post-2002 movers, use RESA (origin locality) so the cell aligns with the 2002 address
  mutate(cell_id_2002=case_when(
    category=="mover post-2002" ~ paste(RESA,AA,LL,ZZ,SEX,sep="-"),
    T ~ paste(SIRSUP,AA,LL,ZZ,SEX,sep="-"))) %>%
  # cell_id_1992: matching cell for 2011-to-1992 linkage; movers before 2002 also get RESA as locality
  mutate(cell_id_1992=case_when(
    category=="mover post-2002" | category=="mover 1992-2002" ~ paste(RESA,AA,LL,ZZ,SEX,sep="-"),
    T ~ paste(SIRSUP,AA,LL,ZZ,SEX,sep="-")))
#add "no gender" cells
# cell_id_genderless omits SEX to avoid bias from differential sex ratios across ethnic groups;
# used as the primary matching cell in the main IV analysis (see paper Section II)
data_2011<-data_2011 %>%
  mutate(cell_id_genderless_2002=case_when(
    category=="mover post-2002" ~ paste(RESA,AA,LL,ZZ,sep="-"),
    T ~ paste(SIRSUP,AA,LL,ZZ,sep="-"))) %>%
  mutate(cell_id_genderless_1992=case_when(
    category=="mover post-2002" | category=="mover 1992-2002" ~ paste(RESA,AA,LL,ZZ,sep="-"),
    T ~ paste(SIRSUP,AA,LL,ZZ,sep="-")))

# --- Household ID ---
#HHID
# HHID: locality + building + staircase + household serial; uniquely identifies a dwelling
# id11: row-level unique key for within-dataset self-joins (parental/spouse info below)
data_2011<-data_2011 %>%
  mutate(HHID=case_when(source != 'mig' ~ paste(MAPA,LITERA,NRF,GOSP,sep="-"),
                        source=="mig" ~ paste(MAPA,LITERA,NRF,GOSP,sep="-"))) %>%
  mutate(id11=row_number())

# --- Education Harmonisation ---
##Educ----
# SCA is the 2011 education code (SCU in 2011 form; labelled SCA here after renaming in read_data)
# Migrant sub-file uses a coarser NIVED code, so it is mapped separately before the main SCA mapping
# "Sub 10" = person born after 2000 still in compulsory schooling; treated as missing in regressions
# The ordered factor levels define the ordinal scale used throughout the analysis
data_2011<-data_2011 %>%
  mutate(EDUC=case_when(source=='mig' & NIVED==1 ~ "No formal",
                        source=='mig' & NIVED==2 ~ "Primary",
                        source=='mig' & NIVED==3 ~ "Gym",
                        source=='mig' & NIVED==4 ~ "General HS",
                        source=='mig' & NIVED==5 ~ "Postsec",
                        source=='mig' & NIVED==6 ~ "Higher Long",
                        SCA==0 & AA>2000 ~ "Sub 10",
                        SCA>=1 & SCA<=40 ~"Higher Long",
                        SCA>=41 & SCA<=52 ~"Higher Short",
                        SCA>=53 & SCA<=65 ~ "Postsec",
                        SCA>=66 & SCA<=67 ~ "General HS",
                        SCA>=68 & SCA<=83 ~ "Specialized HS",
                        SCA>=84 & SCA<=92 ~ "Vocational",
                        SCA==93 ~ "Gym",
                        SCA %in% 94:95 ~ "Primary",
                        SCA>=96 & SCA<=98 ~ "No formal",
                        SCA==99 | (SCA==0 & AA<=1990) ~ "Undeclared")) %>%
  mutate(EDUC=factor(EDUC,levels=c("Sub 10","Undeclared","No formal","Primary","Gym",
                                   "Specialized HS","General HS","Vocational",
                                   "Postsec","Higher Short","Higher Long",'Higher')))

# years: continuous years-of-schooling imputed from EDUC category (used as years_2011 in IV regressions)
# Mapping reflects Romanian educational system durations; Vocational/Postsec set to 13 (10+3)
data_2011<-data_2011 %>%
  mutate(years=
           case_when(EDUC=="None" ~ 0,
                     EDUC=="Sub 10" ~ 2,
                     EDUC=="Higher Long" ~ 16,
                     EDUC=="Higher Short" ~ 14,
                     EDUC=="Higher" ~ 15,
                     EDUC=="General HS" ~ 12,
                     EDUC=="Specialized HS" ~ 12,
                     EDUC=="Vocational" ~ 13,
                     EDUC=="Gym" ~ 8,
                     EDUC=="Primary" ~ 4,
                     EDUC=="Postsec" ~ 13,
                     EDUC=="Undeclared" ~ NA_real_,
                     EDUC=="No formal" ~ 0))

# --- Parental and Spouse Characteristics ---
##add parental and spouse info----
# Self-join on household + SPLOC/MOMLOC/POPLOC line numbers to attach co-resident
# spouse, mother, and father characteristics; suffixes _SPOUSE/_MOM/_POP distinguish them
# all.x=T preserves persons whose family member is absent from the household
data_2011<-base::merge(data_2011,
                       data_2011 %>% select(id11,SIRUTA,MAPA,LITERA,NRF,GOSP,NOP,ET,EDUC,SCA,AA,LL,ZZ,OCUP,LIM,REL,ROMA,LOCN,LOCN_URB,years),
                       by.x=c("SIRUTA","MAPA","LITERA","NRF","GOSP","SPLOC"),
                       by.y=c("SIRUTA","MAPA","LITERA","NRF","GOSP","NOP"),
                       all.x=T,suffixes=c("","_SPOUSE"))
data_2011<-base::merge(data_2011,
                       data_2011 %>% select(id11,SIRUTA,MAPA,LITERA,NRF,GOSP,NOP,ET,EDUC,SCA,AA,LL,ZZ,OCUP,LIM,REL,ROMA,LOCN,LOCN_URB,years,STATUT,OCUP,SECT,COPII,AA_ALM,LL_ALM,DIF,ANC1,ANC2),
                       by.x=c("SIRUTA","MAPA","LITERA","NRF","GOSP","MOMLOC"),
                       by.y=c("SIRUTA","MAPA","LITERA","NRF","GOSP","NOP"),
                       all.x=T,suffixes=c("","_MOM"))
data_2011<-base::merge(data_2011,
                       data_2011 %>% select(id11,SIRUTA,MAPA,LITERA,NRF,GOSP,NOP,ET,EDUC,SCA,AA,LL,ZZ,OCUP,LIM,REL,ROMA,LOCN,LOCN_URB,years,STATUT,OCUP,SECT),
                       by.x=c("SIRUTA","MAPA","LITERA","NRF","GOSP","POPLOC"),
                       by.y=c("SIRUTA","MAPA","LITERA","NRF","GOSP","NOP"),
                       all.x=T,suffixes=c("","_POP"))
#add populations
# Town-level population counts used to construct share-Roma and population-density controls
data_2011<-data_2011 %>%
  group_by(SIRUTA) %>%
  mutate(pop_SIRUTA_2011=n()) %>%
  group_by(SIRSUP) %>%
  mutate(pop_SIRSUP_2011=n())


# Output: cleaned 2011 census file; freed from memory immediately to limit RAM usage
setwd(wd_data_11)
fwrite(data_2011,"data_2011_clean.csv")
rm(list=ls(pattern="data_2011"))
gc()


# --- 2002 Census Cleaning ---
#2002----
setwd(wd_data_02)
filename<-'data_2002_full.csv'
data_2002_full<-read_sample(filename)
data_2002_full<-read_data(filename,data_2002_full)

filename<-'data_2002_common.csv'
data_2002_common<-read_sample(filename)
data_2002_common<-read_data(filename,data_2002_common)

# 2002 has no migrant sub-file; only full-population and common-dwellings registers
data_2002<-rbindlist(list(data_2002_full %>% mutate(source='all'),data_2002_common %>% mutate(source='common')),fill=T)

##Roma----
# In the 2002 census, ethnicity is a single integer; ET==12 is the Roma code (no sub-codes)
data_2002<-data_2002 %>% mutate(ROMA=ifelse(ET==12,T,F))

# --- Residential Mobility and Matching Cells (2002) ---
##category----
# LOCN==90 means the person was enumerated at their usual address; AA_SLR==0 means no recorded move
# Simpler binary stayer/mover classification compared to 2011 (fewer source sub-files available)
data_2002<-data_2002 %>%
  mutate(category=case_when(
    LOCN==90  & AA_SLR==0 ~ "stayer",
    LOCN!=90 | AA_SLR!=0 ~ "mover")) %>%
  # cell_id: matching key = SIRSUP + birth year + locality code + ethnicity + sex
  mutate(cell_id= paste(SIRSUP,AA,LL,ZZ,SEX,sep="-"))

data_2002<-data_2002 %>%
  mutate(cell_id_genderless= paste(SIRSUP,AA,LL,ZZ,sep="-"))

# --- Household ID (2002) ---
##HHID----
# 2002 address fields differ from 2011 (CLAD and LOC replace LITERA and NRF)
data_2002<-data_2002 %>%
  mutate(HHID=paste(MAPA,CLAD,LOC,GOSP,sep="-")) %>%
  mutate(id02=row_number())


# --- Education Harmonisation (2002) ---
##Educ----
# SCA in 2002 uses a different code range than 2011; mapping is census-form specific
# "Sub 10": born after 1991, so could not have completed primary by 2002 (age < 11)
data_2002<-data_2002 %>%
  mutate(EDUC=case_when(SCA==0 & AA>1991 ~ "Sub 10",
                        SCA>=1 & SCA<=37 ~"Higher Long",
                        SCA>=38 & SCA<=49 ~"Higher Short",
                        SCA>=50 & SCA<=61 ~ "Postsec",
                        SCA>=63 & SCA<=64 ~ "General HS",
                        SCA>=65 & SCA<=80 ~ "Specialized HS",
                        SCA>=81 & SCA<=89 ~ "Vocational",
                        SCA>=90 & SCA<=91 ~ "Gym",
                        SCA>=92  & SCA<=94 ~ "Primary",
                        SCA>=95 & SCA<=97 ~ "No formal",
                        SCA==99 | (SCA==0 & AA<=1990) ~ "Undeclared")) %>%
  mutate(EDUC=factor(EDUC,levels=c("Sub 10","Undeclared","No formal","Primary","Gym",
                                   "Specialized HS","General HS","Vocational",
                                   "Postsec","Higher Short","Higher Long")))

data_2002<-data_2002 %>%
  mutate(years=
           case_when(EDUC=="None" ~ 0,
                     EDUC=="Sub 10" ~ 2,
                     EDUC=="Higher Long" ~ 16,
                     EDUC=="Higher Short" ~ 14,
                     EDUC=="General HS" ~ 12,
                     EDUC=="Specialized HS" ~ 12,
                     EDUC=="Vocational" ~ 13,
                     # "Treapta I" was a transitional post-Gym credential in the 1990s Romanian system (~10 years)
                     EDUC=="Treapta I" ~ 10,
                     EDUC=="Gym" ~ 8,
                     EDUC=="Primary" ~ 4,
                     EDUC=="Postsec" ~ 13,
                     EDUC=="Undeclared" ~ NA_real_,
                     EDUC=="No formal" ~ 0,
           ))

# --- Parental and Spouse Characteristics (2002) ---
##add parental and spouse info----
# Same self-join logic as 2011; 2002 address key uses CLAD+LOC instead of LITERA+NRF
data_2002<-base::merge(data_2002,
                       data_2002 %>% select(id02,SIRUTA,MAPA,CLAD,LOC,GOSP,NOP,ET,EDUC,SCA,SCU,AA,LL,ZZ,OCUP,LIM,REL,ROMA,LOCN,LOCN_URB,years),
                       by.x=c("SIRUTA","MAPA","CLAD","LOC","GOSP","SPLOC"),
                       by.y=c("SIRUTA","MAPA","CLAD","LOC","GOSP","NOP"),
                       all.x=T,suffixes=c("","_SPOUSE"))
data_2002<-base::merge(data_2002,
                       data_2002 %>% select(id02,SIRUTA,MAPA,CLAD,LOC,GOSP,NOP,ET,EDUC,SCA,SCU,AA,LL,ZZ,OCUP,LIM,REL,ROMA,LOCN,LOCN_URB,years),
                       by.x=c("SIRUTA","MAPA","CLAD","LOC","GOSP","MOMLOC"),
                       by.y=c("SIRUTA","MAPA","CLAD","LOC","GOSP","NOP"),
                       all.x=T,suffixes=c("","_MOM"))
data_2002<-base::merge(data_2002,
                       data_2002 %>% select(id02,SIRUTA,MAPA,CLAD,LOC,GOSP,NOP,ET,EDUC,SCA,SCU,AA,LL,ZZ,OCUP,LIM,REL,ROMA,LOCN,LOCN_URB,years),
                       by.x=c("SIRUTA","MAPA","CLAD","LOC","GOSP","POPLOC"),
                       by.y=c("SIRUTA","MAPA","CLAD","LOC","GOSP","NOP"),
                       all.x=T,suffixes=c("","_POP"))
data_2002<-data_2002 %>%
  group_by(SIRUTA) %>%
  mutate(pop_SIRUTA_2002=n()) %>%
  group_by(SIRSUP) %>%
  mutate(pop_SIRSUP_2002=n())


# Output: cleaned 2002 census file
setwd(wd_data_02)
fwrite(data_2002,"data_2002_clean.csv")
rm(list=ls(pattern="data_2002"))
gc()


# --- 1992 Census Cleaning ---
#1992----
setwd(wd_data_92)
filename<-'data_1992_full.csv'
data_1992_full<-read_sample(filename)
data_1992_full<-read_data(filename,data_1992_full)

filename<-'data_1992_common.csv'
data_1992_common<-read_sample(filename)
data_1992_common<-read_data(filename,data_1992_common)




data_1992<-rbindlist(list(data_1992_full %>% mutate(source='all'),data_1992_common %>% mutate(source='common')),fill=T)

##Roma----
# ET==12 is the Roma code in the 1992 census (same integer as 2002)
data_1992<-data_1992 %>% mutate(ROMA=ifelse(ET==12,T,F))

# --- Residential Mobility and Matching Cells (1992) ---
##Category----
# In 1992, AA_SLR is NA (not 0) when no move is recorded; condition mirrors 2002 logic but checks is.na
data_1992<-data_1992 %>%
  mutate(category=case_when(
    LOCN==90  & is.na(AA_SLR) ~ "stayer",
    LOCN!=90 | !is.na(AA_SLR) ~ "mover")) %>%
  mutate(cell_id=paste(SIRSUP,AA,LL,ZZ,SEX,sep="-"))

data_1992<-data_1992 %>%
  mutate(cell_id_genderless=paste(SIRSUP,AA,LL,ZZ,sep="-"))

# --- Household ID (1992) ---
##HHID----
data_1992<-data_1992 %>%
  mutate(HHID=paste(MAPA,CLAD,LOC,GOSP,sep="-"))%>%
  mutate(id92=row_number())


# --- Education Harmonisation (1992) ---
##Education----

# The 1992 SCA codebook has different ranges from 2002/2011; notably includes "Treapta I" (SCA==90)
# and collapses university into a single "Higher" category (no short/long split available in 1992)
# years_1992 (derived from this EDUC) serves as the IV instrument in the main IV specification
data_1992<-data_1992 %>%
  mutate(EDUC=case_when(SCA==0 & AA>1981 ~ "Sub 10",
                        SCA>=1 & SCA<=37 ~"Higher",
                        SCA>=50 & SCA<=61 ~ "Postsec",
                        SCA>=62 & SCA<=65 | SCA==73 ~ "General HS",
                        (SCA>=66 & SCA<=72) | (SCA>=74 & SCA<=80) ~ "Specialized HS",
                        (SCA>=81 & SCA<=89) ~ "Vocational",
                        # "Treapta I": first cycle of post-Gym secondary, discontinued after 1990
                        SCA==90 ~ "Treapta I",
                        SCA>=91 & SCA<=93 ~ "Gym",
                        SCA==94 |SCA==95 ~ "Primary",
                        (SCA>=96 & SCA<=98) ~ "No formal",
                        SCA==99 | (SCA==0 & AA<=1981) ~ "Undeclared")) %>%
  mutate(EDUC=factor(EDUC,levels=c("Sub 10","Undeclared","No formal","Primary","Gym",
                                   "Specialized HS","General HS","Treapta I","Vocational",
                                   "Postsec","Higher")))

data_1992<-data_1992 %>%
  mutate(years=
           case_when(EDUC=="None" ~ 0,
                     EDUC=="Sub 10" ~ 2,
                     EDUC=="Higher" ~ 16,
                     EDUC=="General HS" ~ 12,
                     EDUC=="Specialized HS" ~ 12,
                     EDUC=="Vocational" ~ 13,
                     EDUC=="Treapta I" ~ 10,
                     EDUC=="Gym" ~ 8,
                     EDUC=="Primary" ~ 4,
                     EDUC=="Postsec" ~ 13,
                     EDUC=="Undeclared" ~ NA_real_,
                     EDUC=="No formal" ~ 0,
           ))



# --- Parental and Spouse Characteristics (1992) ---
##add parental and spouse info----
# SIRSUP is included explicitly in the 1992 join (unlike 2002/2011) because the 1992 raw file
# does not always carry SIRSUP on every row after the rbindlist; ensures the join key is available
data_1992<-base::merge(data_1992,
                       data_1992 %>% select(id92,SIRUTA,SIRSUP,MAPA,CLAD,LOC,GOSP,NOP,ET,EDUC,SCA,AA,LL,ZZ,OCUP,LIM,REL,ROMA,LOCN,LOCN_URB,years),
                       by.x=c("SIRUTA","MAPA","CLAD","LOC","GOSP","SPLOC"),
                       by.y=c("SIRUTA","MAPA","CLAD","LOC","GOSP","NOP"),
                       all.x=T,suffixes=c("","_SPOUSE"))
data_1992<-base::merge(data_1992,
                       data_1992 %>% select(id92,SIRUTA,SIRSUP,MAPA,CLAD,LOC,GOSP,NOP,ET,EDUC,SCA,AA,LL,ZZ,OCUP,LIM,REL,ROMA,LOCN,LOCN_URB,years),
                       by.x=c("SIRUTA","MAPA","CLAD","LOC","GOSP","MOMLOC"),
                       by.y=c("SIRUTA","MAPA","CLAD","LOC","GOSP","NOP"),
                       all.x=T,suffixes=c("","_MOM"))
data_1992<-base::merge(data_1992,
                       data_1992 %>% select(id92,SIRUTA,SIRSUP,MAPA,CLAD,LOC,GOSP,NOP,ET,EDUC,SCA,AA,LL,ZZ,OCUP,LIM,REL,ROMA,LOCN,LOCN_URB,years),
                       by.x=c("SIRUTA","MAPA","CLAD","LOC","GOSP","POPLOC"),
                       by.y=c("SIRUTA","MAPA","CLAD","LOC","GOSP","NOP"),
                       all.x=T,suffixes=c("","_POP"))

data_1992<-data_1992 %>%
  group_by(SIRUTA) %>%
  mutate(pop_SIRUTA_1992=n()) %>%
  group_by(SIRSUP) %>%
  mutate(pop_SIRSUP_1992=n())

# Output: cleaned 1992 census file; this file supplies years_1992 (the IV instrument) in downstream scripts
setwd(wd_data_92)
fwrite(data_1992,"data_1992_clean.csv")
rm(list=ls(pattern="data_1992"))
gc()

