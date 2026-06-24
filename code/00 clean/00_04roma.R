# =====================================================================
# Flag the set of "ever-Roma" linkage cells across all three census waves
# Produces:  roma_any.csv (genderless cell IDs ever associated with Roma),
#            data_2011_roma.csv, data_2002_roma.csv, data_1992_roma.csv
#            (intermediate; written to wd_data_linked and each wave's dir)
# Inputs:    data_2011_clean_births.csv, data_2002_clean.csv, data_1992_clean.csv
# Summary:   For each wave, a (genderless) birthdate-by-locality cell is marked
#            Roma if anyone in it self-reports Roma (ROMA), has a Roma mother/
#            father/spouse (ET in the 1200-1299 / ==12 ethnicity codes), or has
#            Roma on the birth certificate (nat==12). Cells flagged in any wave
#            are pooled into roma_any, and each wave's full data is then
#            subset to those cells. No paper exhibit.
# =====================================================================

# ---- Load the three cleaned waves (only the columns needed for flagging) ----
setwd(wd_data_11)
filename<-'data_2011_clean_births.csv'
data_2011<-read_sample(filename) %>%
  select(cell_id_genderless_2002,cell_id_genderless_1992,ROMA,ET_MOM,ET_POP,ET_SPOUSE,nat,source,id11_MOM_BC,copil_id)
data_2011<-read_data(filename,data_2011)

setwd(wd_data_02)
filename<-'data_2002_clean.csv'
data_2002<-read_sample(filename) %>%
  select(cell_id_genderless,ROMA,ET_MOM,ET_POP,ET_SPOUSE,source)
data_2002<-read_data(filename,data_2002)

setwd(wd_data_92)
filename<-'data_1992_clean.csv'
data_1992<-read_sample(filename) %>%
  select(cell_id_genderless,ROMA,ET_MOM,ET_POP,ET_SPOUSE,source)
data_1992<-read_data(filename,data_1992)

#Get Roma Cells
#2011 - 2002 ----
roma_2011_2002<-data_2011 %>%
  filter(source != "common") %>%
  select(cell_id_genderless_2002,ROMA,ET_MOM,ET_POP,ET_SPOUSE,nat) %>%
  group_by(cell_id_genderless_2002) %>%
  filter(any(ROMA==T) | ET_MOM %in% 1200:1299 | ET_POP%in% 1200:1299 | ET_SPOUSE %in% 1200:1299 | nat==12)  %>%
  ungroup %>%
  select(cell_id_genderless_2002) %>%
  rename(cell_id_genderless=cell_id_genderless_2002)
gc()

roma_2011_2002_common<-data_2011 %>%
  filter(source == "common") %>%
  select(cell_id_genderless_2002,ROMA) %>%
  filter(ROMA==T) %>%
  select(cell_id_genderless_2002)
gc()

#2011 - 1992 ----
roma_2011_1992<-data_2011 %>%
  filter(source != "common") %>%
  select(cell_id_genderless_1992,ROMA,ET_MOM,ET_POP,ET_SPOUSE,nat) %>%
  group_by(cell_id_genderless_1992) %>%
  filter(any(ROMA==T) | ET_MOM %in% 1200:1299 | ET_POP%in% 1200:1299 | ET_SPOUSE %in% 1200:1299 | nat==12)  %>%
  ungroup %>%
  select(cell_id_genderless_1992) %>%
  rename(cell_id_genderless=cell_id_genderless_1992)
gc()

roma_2011_1992_common<-data_2011 %>%
  filter(source == "common") %>%
  select(cell_id_genderless_1992,ROMA) %>%
  filter(ROMA==T) %>%
  select(cell_id_genderless_1992)
gc()

#2002
roma_2002<-data_2002 %>%
  filter(source != "common") %>%
  select(cell_id_genderless,ROMA,ET_MOM,ET_POP,ET_SPOUSE) %>%
  group_by(cell_id_genderless) %>%
  filter(any(ROMA==T) | ET_MOM==12 | ET_POP==12 | ET_SPOUSE==12 ) %>%
  ungroup %>%
  select(cell_id_genderless)
gc()

roma_2002_common<-data_2002 %>%
  filter(source == "common") %>%
  select(cell_id_genderless,ROMA) %>%
  filter(ROMA==T) %>%
  select(cell_id_genderless)
gc()

#1992
roma_1992<-data_1992 %>%
  filter(source != "common") %>%
  select(cell_id_genderless,ROMA,ET_MOM,ET_POP,ET_SPOUSE) %>%
  group_by(cell_id_genderless) %>%
  filter(any(ROMA==T) | ET_MOM==12 | ET_POP==12 | ET_SPOUSE==12 ) %>%
  ungroup %>%
  select(cell_id_genderless)
gc()

roma_1992_common<-data_1992 %>%
  filter(source == "common") %>%
  select(cell_id_genderless,ROMA) %>%
  filter(ROMA==T) %>%
  select(cell_id_genderless)
gc()

# ---- Pool ever-Roma cells across waves and write ----
#any
roma_any<-bind_rows(roma_2011_2002,roma_2011_1992,roma_2002,roma_1992,
                    roma_1992_common,roma_2002_common,roma_2011_1992_common,roma_2011_2002_common)
roma_any<-roma_any %>% select(cell_id_genderless)

#write
setwd(wd_data_linked)
fwrite(roma_any,"roma_any.csv")


# ---- Subset each wave to the ever-Roma cells and write ----
data_2011_roma<-data_2011 %>%
  filter(cell_id_genderless_2002 %in% roma_any$cell_id_genderless|
           cell_id_genderless_1992 %in% roma_any$cell_id_genderless  ) 

data_2002_roma<-data_2002 %>% 
  filter(cell_id_genderless %in% roma_any$cell_id_genderless) 

data_1992_roma<-data_1992 %>% 
  filter(cell_id_genderless %in% roma_any$cell_id_genderless) 


setwd(wd_data_11)
fwrite(data_2011_roma,"data_2011_roma.csv")
setwd(wd_data_02)
fwrite(data_2002_roma,"data_2002_roma.csv")
setwd(wd_data_92)
fwrite(data_1992_roma,"data_1992_roma.csv")