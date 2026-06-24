# =====================================================================
# Figure A09 — Structural model fit (estimated vs. data moments)
# Produces:  output/Figure A09.pdf
# Inputs:    results_parallel.rds (saved output of the structural
#            estimation, containing data and model-implied moments)
# Summary:   Reshapes the estimation output into matched pairs of data
#            and model-predicted moments, labels each of the eight
#            three-census identity sequences (e.g. NNN, RRR), and plots
#            estimated against data moments with a 45-degree line so
#            points on the line indicate a good fit. Split into a
#            "Never Roma" (NNN) panel and an "Other Moments" panel.
# =====================================================================

# ---- Load structural estimation results ----
setwd(wd_data_structural)
results_all<-readRDS("results_parallel.rds")
# ---- Map the numeric education id to readable schooling categories ----
results_all<-results_all %>%
    mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School",
                             id %in% 14:16 ~ "Postsec")) %>%
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
  # mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary"))) 
     # mutate(Education=case_when(id==0 ~ "None",
     #                          id==4 ~ "GS",
     #                          id %in% 8:10  ~ "MS",
     #                          id %in% 12:13 ~ "HS",
     #                          id %in% 14:16 ~ "Postsec")) %>%
   # mutate(Education=factor(Education,levels=c("None","GS","MS","HS","Postsec"))) %>%
   mutate(educ=as.numeric(as.character(id)))

# ---- Reshape to long form: pair each model moment (m*) with its data moment (dist_m*) ----
# Columns starting with "dist_m" hold model-implied moments, columns starting with
# "m" hold the corresponding data moments. Pivot them long, decode the moment index
# 1-8 into the three-census identity sequence (R=reported Roma, N=not), and pivot
# back wide so each row has both the Data and Estimated value for one moment.
data_fit<-results_all %>%
  group_by(Education) %>%
  mutate(across(starts_with("dist_"), 
                .fns = ~ .x + get(sub("dist_m", "m", cur_column())))) %>%
   pivot_longer(
    cols = starts_with("dist") | starts_with("m"),
    names_to = c("Population", "metric"),
    names_pattern = "(dist_m|m)(.*)"
  )  %>%
  mutate(metric=case_when(metric==1 ~ "NNN",
                          metric==2 ~ "NNR",
                          metric==3 ~ "NRN",
                          metric==4 ~ "NRR",
                          metric==5 ~ "RNN",
                          metric==6 ~ "RNR",
                          metric==7 ~ "RRN",
                          metric==8 ~ "RRR"
                          )) %>%
  # filter(metric!="NNN")%>%
  mutate(Panel=factor(ifelse(metric=="NNN","Never Roma","Other Moments"),
                      levels=c("Other Moments","Never Roma"))) %>%
  mutate(Population=ifelse(Population=="m","Data","Estimated")) %>%
  pivot_wider(names_from="Population",values_from="value") %>%
  rename(Population=metric)

# data_fit$Education <- str_wrap(data_fit$Education, width = 10)

# ---- Plot estimated vs. data moments with a 45-degree reference line ----
g<-ggplot(data=data_fit,aes(x=Estimated,y=Data,group=Population,color=Population))+
  facet_wrap(~ Panel,scales="free") +
  geom_point()+
  geom_abline(slope = 1, intercept = 0, color = "black") +
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),  
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))+
  xlab("Data Moments")+
  ylab("Estimated Moments")+
  # scale_x_continuous(breaks=0:10)+
  #scale_y_continuous(breaks=seq(from = 0, to = 1, by = 0.05),labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
   # scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  labs(color=str_wrap("Moment",width=10))

# ---- Write the figure to PDF ----
setwd(wd_output)
pdf("Figure A09.pdf",width=10,height=5)
g
dev.off() 
g
