# =====================================================================
# Harmonize and clean the census microdata for all three waves (2011/2002/1992)
# Produces:  data_2011_clean.csv, data_2002_clean.csv, data_1992_clean.csv
#            (intermediate; each written to its wave's data directory)
# Inputs:    data_<year>_full.csv / data_<year>_common.csv (and 2011 migrants),
#            plus school nomenclature lookups (SCOALA92/02, nomenclator_scoli)
# Summary:   For each wave: stack the full/common/(migrant) samples, build a
#            Roma indicator (ET ethnicity code), a mover/stayer category, the
#            birthdate-by-locality cell IDs used for linkage, a household ID,
#            an education ladder (EDUC) and its years-of-schooling mapping, then
#            self-merge to attach spouse/mother/father attributes and locality
#            population counts. Writes one cleaned CSV per wave. No paper exhibit.
# =====================================================================

#clean data
#2011 ----
setwd(wd_data_11)
filename<-'data_2011_full.csv'
data_2011_full<-read_sample(filename)
data_2011_full<-read_data(filename,data_2011_full)



filename<-'data_2011_common.csv'
data_2011_common<-read_sample(filename)
data_2011_common<-read_data(filename,data_2011_common)

filename<-'data_2011_migrants.csv'
data_2011_migrants<-read_sample(filename)
data_2011_migrants<-read_data(filename,data_2011_migrants)

data_2011<-rbindlist(list(data_2011_full %>% mutate(source='all'),data_2011_common %>% mutate(source='common'),data_2011_migrants %>% mutate(source='mig')),fill=T)

###nomenclator
setwd(wd_data_02_other)
school_2002<-read_dta('SCOALA02.DTA')
setwd(wd_data_92_other)
school_1992<-read.dbf('SCOALA92.DBF')
setwd(wd_data_11_other)
school_2011<-read.csv('nomenclator_scoli.CSV')


##Roma----
data_2011<-data_2011 %>% 
  mutate(ROMA=ifelse(ET>=1200 & ET<1300,T,F)) 

##category----
# Classify each person by migration status and the wave in which they would be
# observed at their pre-move locality, using last-residence year (AA_SLR).
data_2011<-data_2011 %>%
  mutate(category=case_when(
    source=='mig' & AA_SLR>=2002 ~ "migrant post-2002",
    source=='mig' & AA_SLR>=1992 & AA_SLR<2002  ~ "migrant 1992-2002",
    source=='mig' & AA_SLR<1992 ~ "migrant pre-1992",
    ALTARES==2 ~ "stayer",
    AA_SLR>=2002 ~ "mover post-2002",
    AA_SLR>=1992 & AA_SLR<2002  ~ "mover 1992-2002",
    AA_SLR<1992 ~ "mover pre-1992")) %>%
  # Linkage key to the 2002 wave: birthdate + sex, located at the residence
  # locality (RESA) for those who moved after 2002, else current superior
  # locality (SIRSUP).
  mutate(cell_id_2002=case_when(
    category=="mover post-2002" ~ paste(RESA,AA,LL,ZZ,SEX,sep="-"),
    T ~ paste(SIRSUP,AA,LL,ZZ,SEX,sep="-"))) %>%
  mutate(cell_id_1992=case_when(
    category=="mover post-2002" | category=="mover 1992-2002" ~ paste(RESA,AA,LL,ZZ,SEX,sep="-"),
    T ~ paste(SIRSUP,AA,LL,ZZ,SEX,sep="-")))
#add "no gender" cells
data_2011<-data_2011 %>%
  mutate(cell_id_genderless_2002=case_when(
    category=="mover post-2002" ~ paste(RESA,AA,LL,ZZ,sep="-"),
    T ~ paste(SIRSUP,AA,LL,ZZ,sep="-"))) %>%
  mutate(cell_id_genderless_1992=case_when(
    category=="mover post-2002" | category=="mover 1992-2002" ~ paste(RESA,AA,LL,ZZ,sep="-"),
    T ~ paste(SIRSUP,AA,LL,ZZ,sep="-")))

#HHID
data_2011<-data_2011 %>%
  mutate(HHID=case_when(source != 'mig' ~ paste(MAPA,LITERA,NRF,GOSP,sep="-"),
                        source=="mig" ~ paste(MAPA,LITERA,NRF,GOSP,sep="-"))) %>%
  mutate(id11=row_number())

##Educ----
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

##add parental and spouse info----
# Self-join within household: match each person to their spouse (SPLOC),
# mother (MOMLOC) and father (POPLOC) line number to attach _SPOUSE/_MOM/_POP
# attributes (ethnicity, education, occupation, etc.).
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
data_2011<-data_2011 %>%
  group_by(SIRUTA) %>%
  mutate(pop_SIRUTA_2011=n()) %>%
  group_by(SIRSUP) %>%
  mutate(pop_SIRSUP_2011=n())


setwd(wd_data_11)
fwrite(data_2011,"data_2011_clean.csv")
rm(list=ls(pattern="data_2011"))
gc()


#2002----
setwd(wd_data_02)
filename<-'data_2002_full.csv'
data_2002_full<-read_sample(filename)
data_2002_full<-read_data(filename,data_2002_full)

filename<-'data_2002_common.csv'
data_2002_common<-read_sample(filename)
data_2002_common<-read_data(filename,data_2002_common)

data_2002<-rbindlist(list(data_2002_full %>% mutate(source='all'),data_2002_common %>% mutate(source='common')),fill=T)

##Roma----
data_2002<-data_2002 %>% mutate(ROMA=ifelse(ET==12,T,F))

##category----
data_2002<-data_2002 %>%
  mutate(category=case_when(
    LOCN==90  & AA_SLR==0 ~ "stayer",
    LOCN!=90 | AA_SLR!=0 ~ "mover")) %>%
  mutate(cell_id= paste(SIRSUP,AA,LL,ZZ,SEX,sep="-")) 

data_2002<-data_2002 %>%
  mutate(cell_id_genderless= paste(SIRSUP,AA,LL,ZZ,sep="-"))

##HHID----
data_2002<-data_2002 %>%
  mutate(HHID=paste(MAPA,CLAD,LOC,GOSP,sep="-")) %>%
  mutate(id02=row_number())


##Educ----
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
                     EDUC=="Treapta I" ~ 10,
                     EDUC=="Gym" ~ 8,
                     EDUC=="Primary" ~ 4,
                     EDUC=="Postsec" ~ 13,
                     EDUC=="Undeclared" ~ NA_real_,
                     EDUC=="No formal" ~ 0,
           ))

##add parental and spouse info----
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


setwd(wd_data_02)
fwrite(data_2002,"data_2002_clean.csv")
rm(list=ls(pattern="data_2002"))
gc()


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
data_1992<-data_1992 %>% mutate(ROMA=ifelse(ET==12,T,F)) 

##Category----
data_1992<-data_1992 %>%
  mutate(category=case_when(
    LOCN==90  & is.na(AA_SLR) ~ "stayer", 
    LOCN!=90 | !is.na(AA_SLR) ~ "mover")) %>%
  mutate(cell_id=paste(SIRSUP,AA,LL,ZZ,SEX,sep="-")) 

data_1992<-data_1992 %>%
  mutate(cell_id_genderless=paste(SIRSUP,AA,LL,ZZ,sep="-"))

##HHID----
data_1992<-data_1992 %>%
  mutate(HHID=paste(MAPA,CLAD,LOC,GOSP,sep="-"))%>%
  mutate(id92=row_number())


##Education----


data_1992<-data_1992 %>%
  mutate(EDUC=case_when(SCA==0 & AA>1981 ~ "Sub 10",
                        SCA>=1 & SCA<=37 ~"Higher",
                        SCA>=50 & SCA<=61 ~ "Postsec",
                        SCA>=62 & SCA<=65 | SCA==73 ~ "General HS",
                        (SCA>=66 & SCA<=72) | (SCA>=74 & SCA<=80) ~ "Specialized HS",
                        (SCA>=81 & SCA<=89) ~ "Vocational",
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



##add parental and spouse info----
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

setwd(wd_data_92)
fwrite(data_1992,"data_1992_clean.csv")
rm(list=ls(pattern="data_1992"))
gc()

