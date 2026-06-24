# =====================================================================
# Figures A07 and A08 — Representativeness of the linked samples
# Produces:  output/Figure A07.pdf (Roma only)
#            output/Figure A08.pdf (full sample)
# Inputs:    data_2011_clean.csv, data_1992_2011_unique.csv,
#            data_2002_2011_unique.csv, data_1992_2002_2011_unique.csv
# Summary:   Compares the distribution of locality population, education,
#            sex and age across the full 2011 census and each linked
#            sample, to show the linked panels are broadly representative.
#            Figure A08 uses everyone; Figure A07 restricts to reported
#            Roma. The commented block at the bottom records the match
#            rates quoted elsewhere in the text.
# =====================================================================

# ---- (Disabled) optional 1992/2002 standalone cross-sections ----
# setwd(wd_data_92)
# filename<-'data_1992_clean.csv'
# data_92<-read_sample(filename) %>%
#   select(ROMA,years,ET,AA,pop_SIRSUP_1992,SEX)
# data_92<-read_data(filename,data_92) 
# data_92<-data_92 %>%
#   mutate(Census="1992")
# 
# setwd(wd_data_02)
# filename<-'data_2002_clean.csv'
# data_02<-read_sample(filename) %>%
#   select(ROMA,years,ET,AA,pop_SIRSUP_2002,SEX)
# data_02<-read_data(filename,data_02) 
# data_02<-data_02 %>%
#   mutate(Census="2002")

setwd(wd_data_11)
filename<-'data_2011_clean.csv'
data_11<-read_sample(filename) %>%
  select(ROMA,years,ET,AA,pop_SIRSUP_2011,SEX)
data_11<-read_data(filename,data_11) 
data_11<-data_11 %>%
  mutate(Census="2011")

setwd(wd_data_linked)
filename<-'data_1992_2011_unique.csv'
data_92_11<-read_sample(filename) %>%
  select(ROMA_2011,years_2011,AA_2011,pop_SIRSUP_2011,SEX_2011,ROMA_1992,pop_SIRSUP_1992) 
data_92_11<-read_data(filename,data_92_11) 
data_92_11<-data_92_11 %>%
  mutate(Census="1992 - 2011") %>%
  rename(ROMA=ROMA_2011,years=years_2011,AA=AA_2011,ROMA_baseline=ROMA_1992)

setwd(wd_data_linked)
filename<-'data_2002_2011_unique.csv'
data_02_11<-read_sample(filename) %>%
  select(ROMA_2011,years_2011,AA_2011,pop_SIRSUP_2011,SEX_2011,ROMA_2002,pop_SIRSUP_2002)
data_02_11<-read_data(filename,data_02_11) 
data_02_11<-data_02_11 %>%
  mutate(Census="2002 - 2011") %>%
  rename(ROMA=ROMA_2011,years=years_2011,AA=AA_2011,ROMA_baseline=ROMA_2002)

setwd(wd_data_linked)
filename<-'data_1992_2002_2011_unique.csv'
data_02_11<-read_sample(filename) %>%
  select(ROMA_2011,years_2011,AA_2011,pop_SIRSUP_2011,SEX_2011,ROMA_2002,pop_SIRSUP_2002)
data_02_11<-read_data(filename,data_02_11) 
data_02_11<-data_02_11 %>%
  mutate(Census="2002 - 2011") %>%
  rename(ROMA=ROMA_2011,years=years_2011,AA=AA_2011,ROMA_baseline=ROMA_2002)

data_02_11<-data_02_11 %>%
  rename(SEX=SEX_2011)
data_92_11<-data_92_11 %>%
  rename(SEX=SEX_2011)

data<-bind_rows(data_11,data_02_11,data_92_11)
data<-data %>%
  mutate(Education=case_when(years==0 ~ "None",
                             years==4 ~ "Primary School",
                             years %in% 8:10  ~ "Middle School",
                             years %in% 12:13 ~ "High School or Vocational",
                             years %in% 14:16 ~ "Post- Secondary")) %>%
  mutate(pop=cut(pop_SIRSUP_2011, 
                 breaks = c(0,1000,5000,10000,50000,200000,Inf),
                 labels = c("<1k","1-5k","5-10k", "10-50k", "50-200k",
                            ">200k"))) %>%
  mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary"))) 

stats_pop<-data %>%
  group_by(pop,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Locality Population (2011)")

stats_educ<-data %>%
  filter(!is.na(Education)) %>%
  group_by(Education,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Education (2011)")

stats_gender<-data %>%
  mutate(SEX=ifelse(SEX==1,"Male","Female")) %>%
  filter(!is.na(SEX)) %>%
  group_by(SEX,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="SEX") %>%
  mutate(SEX=factor(SEX,levels=c("Male","Female"))) 

stats_age<-data %>%
  mutate(AGE_baseline=2011-AA-1) %>%
  mutate(AGE=case_when(AGE_baseline %in% 10:20 ~ "10-20",
                       AGE_baseline %in% 21:30 ~ "21-30",
                       AGE_baseline %in% 31:40 ~ "31-40",
                       AGE_baseline %in% 41:110 ~ "41+"
                       )) %>%
  filter(!is.na(AGE)) %>%
  group_by(AGE,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Age (2011)") %>%
  mutate(AGE=factor(AGE,levels=c("10-20","21-30","31-40","41+"))) 


data_graph <- bind_rows(
  stats_educ %>% mutate(x = factor(Education, levels = levels(Education))) %>% select(-Education),
  stats_pop %>% mutate(x = factor(pop, levels = levels(pop))) %>% select(-pop),
  stats_age %>% mutate(x = factor(AGE, levels = levels(AGE))) %>% select(-AGE),
  stats_gender %>% mutate(x = factor(SEX, levels = levels(SEX))) %>% select(-SEX)
)

g<-ggplot(data=data_graph,aes(
  x=x,
  y=proportion ,
  fill=Census))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~stat,scales="free_x")+
  #scale_x_continuous(breaks=c(1992,2002,2011))+
  #theme(axis.title.y=element_text("Town Population (1992)"))+
  theme(strip.text.x = element_text(size=14),
    axis.text.x = element_text(angle = 0, vjust = 0.5,size=14),
    axis.text.y = element_text(size=14),  
    axis.title.x = element_text(size=14),
    axis.title.y = element_text(size=16),
    legend.title=element_text(size=14),
    legend.text=element_text(size=14),
    legend.justification = c("center"),
    legend.position = c("top"))+
  # theme(legend.key.size = unit(0.8, 'cm'),
  #       legend.title.align=0.5,
  #       legend.text = element_text(size=12),
  #       legend.title=element_text(size=12),
  #       plot.title = element_text(size = 15),
  #       legend.justification = c("center"),
  #       legend.position = c("top"),
  #       plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
  #       axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
  #       strip.text.x = element_text(size = 12))+
  guides(fill=guide_legend(title="Census"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  xlab(element_blank())+
  ylab("Proportion")
g

setwd(wd_output)
pdf("Figure A08.pdf",width=10,height=6)
g
dev.off()

#roma----
stats_pop_roma<-data %>%
  filter(ROMA==T) %>%
  group_by(pop,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Locality Population")
stats_educ_roma<-data %>%
  filter(ROMA==T) %>%
  filter(!is.na(Education)) %>%
  group_by(Education,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Education")
stats_gender_roma<-data %>%
  filter(ROMA==T) %>%
  mutate(SEX=ifelse(SEX==1,"Male","Female")) %>%
  filter(!is.na(SEX)) %>%
  group_by(SEX,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="SEX") %>%
  mutate(SEX=factor(SEX,levels=c("Male","Female"))) 
stats_age_roma<-data %>%
  filter(ROMA==T) %>%
  mutate(AGE_baseline=2011-AA-1) %>%
  mutate(AGE=case_when(AGE_baseline %in% 10:20 ~ "10-20",
                       AGE_baseline %in% 21:30 ~ "21-30",
                       AGE_baseline %in% 31:40 ~ "31-40",
                       AGE_baseline %in% 41:110 ~ "41+"
  )) %>%
  filter(!is.na(AGE)) %>%
  group_by(AGE,Census) %>%
  summarise(proportion = n()) %>%
  group_by(Census) %>%
  mutate(proportion=proportion/sum(proportion)) %>%
  mutate(stat="Age (2011)") %>%
  mutate(AGE=factor(AGE,levels=c("10-20","21-30","31-40","41+"))) 




data_graph <- bind_rows(
  stats_educ_roma %>% mutate(x = factor(Education, levels = levels(Education))) %>% select(-Education),
  stats_pop_roma %>% mutate(x = factor(pop, levels = levels(pop))) %>% select(-pop),
  stats_age_roma %>% mutate(x = factor(AGE, levels = levels(AGE))) %>% select(-AGE),
  stats_gender_roma %>% mutate(x = factor(SEX, levels = levels(SEX))) %>% select(-SEX)
)


g<-ggplot(data=data_graph,aes(
  x=x,
  y=proportion ,
  fill=Census))+
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~stat,scales="free_x")+
  #scale_x_continuous(breaks=c(1992,2002,2011))+
  #theme(axis.title.y=element_text("Town Population (1992)"))+
  theme(strip.text.x = element_text(size=14),
        axis.text.x = element_text(angle = 0, vjust = 0.5,size=14),
        axis.text.y = element_text(size=14),  
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=16),
        legend.title=element_text(size=14),
        legend.text=element_text(size=14),
        legend.justification = c("center"),
        legend.position = c("top"))+
  # theme(legend.key.size = unit(0.8, 'cm'),
  #       legend.title.align=0.5,
  #       legend.text = element_text(size=12),
  #       legend.title=element_text(size=12),
  #       plot.title = element_text(size = 15),
  #       legend.justification = c("center"),
  #       legend.position = c("top"),
  #       plot.subtitle = element_text(size=20, hjust = 0.5, vjust=-4),
  #       axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
  #       strip.text.x = element_text(size = 12))+
  guides(fill=guide_legend(title="Census"))+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  xlab(element_blank())+
  ylab("Proportion")
g

setwd(wd_output)
pdf("Figure A07.pdf",width=10,height=6)
g
dev.off()

# see summary_stats_for_matching
# ##match rate Roma (2011):
# #2011 individuals linked: 32.9%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991))/
#   nrow(data %>% filter(Census=="2011") %>% filter(AA<=1991)) 
# 
# #2011 Roma linked: 36.0\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991) %>% filter(ROMA==T))/
#   nrow(data %>% filter(Census=="2011") %>% filter(AA<=1991) %>% filter(ROMA==T)) 
# 
# #2011 ind. linked small towns: 53.3\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% 
#       filter(pop_SIRSUP_2011<=10000 & AA<=1991))/
#   nrow(data %>% filter(Census=="2011") %>% 
#         filter(pop_SIRSUP_2011<=10000 & AA<=1991)) 
# 
# #2011 Roma linked:  45.7\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% 
#       filter(pop_SIRSUP_2011<=10000 & AA<=1991) %>% filter(ROMA==T))/
#   nrow(data %>% filter(Census=="2011") %>% 
#         filter(pop_SIRSUP_2011<=10000 & AA<=1991) %>% filter(ROMA==T)) 


# ##match rate Roma (1992):
# #24.0\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991)) 
# #27.5\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991) %>% filter(ROMA_baseline==T))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991) %>% filter(ROMA==T))
# #40.3%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991 & pop_SIRSUP_1992<=10000))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991 & pop_SIRSUP_1992<=10000) ) 
# #38.2\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991 & pop_SIRSUP_1992<=10000) %>% filter(ROMA_baseline==T))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991 & pop_SIRSUP_1992<=10000) %>% filter(ROMA==T))
# 
# 
# #not too old:
# #49.7%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991 & AA>=1951 & pop_SIRSUP_1992<=10000))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991 & AA>=1951 & pop_SIRSUP_1992<=10000) ) 
# #41.7\%
# nrow(data %>% filter(Census=="1992 - 2011") %>% filter(AA<=1991 & AA>=1951 & pop_SIRSUP_1992<=10000) %>% filter(ROMA_baseline==T))/
#   nrow(data_92 %>% filter(Census=="1992") %>% filter(AA<=1991 & AA>=1951 & pop_SIRSUP_1992<=10000) %>% filter(ROMA==T))
