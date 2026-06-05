# Produces Figure A.9: Structural Model Fit

# --- Load Structural Model Results ---

# Load bootstrap results from the structural mixture model (100 iterations over
# education groups, four distributional assumptions for cost heterogeneity).
# results_parallel.rds is written by the structural estimation script.
setwd(wd_data_structural)
results_all<-readRDS("results_parallel.rds")

# --- Assign Education Labels and Recode ---

# Map numeric education group ids (years of schooling) to ordered factor labels.
# id values correspond to years_1992 / years_2011 bins used in estimation.
results_all<-results_all %>%
    mutate(Education=case_when(id==0 ~ "None",
                             id==4 ~ "Primary",
                             id %in% 8:10  ~ "Middle School",
                             id %in% 12:13 ~ "High School",
                             id %in% 14:16 ~ "Postsec")) %>%
  # Education: ordered factor used on the x-axis of the fit plot
  mutate(Education=factor(Education,levels=c("None","Primary","Middle School","High School","Postsec"))) %>%
  # mutate(Education=factor(Education,levels=c("None","Primary School","Middle School","High School or Vocational","Post- Secondary")))
     # mutate(Education=case_when(id==0 ~ "None",
     #                          id==4 ~ "GS",
     #                          id %in% 8:10  ~ "MS",
     #                          id %in% 12:13 ~ "HS",
     #                          id %in% 14:16 ~ "Postsec")) %>%
   # mutate(Education=factor(Education,levels=c("None","GS","MS","HS","Postsec"))) %>%
  # educ: numeric version of id, kept for potential further calculations
   mutate(educ=as.numeric(as.character(id)))

# --- Reshape to Long Format and Compute Model-Fit Moments ---

# The eight moments (m1--m8, dist_m1--dist_m8) are the observed and model-predicted
# joint probabilities of three consecutive Roma identification spells across the
# 1992-2002-2011 panel. Each combination is encoded as a three-letter string
# (R = Roma-identified, N = non-Roma-identified) for the three census waves.
data_fit<-results_all %>%
  group_by(Education) %>%
  # For each dist_mX column, add the corresponding raw moment mX so that
  # dist_mX holds the model-predicted value on the same scale as the data moment.
  mutate(across(starts_with("dist_"),
                .fns = ~ .x + get(sub("dist_m", "m", cur_column())))) %>%
  # Pivot both the data moments (m1-m8) and estimated moments (dist_m1-dist_m8)
  # into long format, separating the source prefix from the numeric moment index.
   pivot_longer(
    cols = starts_with("dist") | starts_with("m"),
    names_to = c("Population", "metric"),
    names_pattern = "(dist_m|m)(.*)"
  )  %>%
  # Decode numeric moment index to the three-wave R/N identification string.
  # NNN = never Roma across all three waves; RRR = Roma in all three waves, etc.
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
  # Separate the NNN moment into its own facet panel because its scale dwarfs
  # the other moments (always-non-Roma is the large majority of the sample).
  mutate(Panel=factor(ifelse(metric=="NNN","Never Roma","Other Moments"),
                      levels=c("Other Moments","Never Roma"))) %>%
  # Recode the source prefix to human-readable labels for the color legend.
  mutate(Population=ifelse(Population=="m","Data","Estimated")) %>%
  # Pivot wide so Data and Estimated are separate columns for the scatter plot.
  pivot_wider(names_from="Population",values_from="value") %>%
  # Rename metric to Population for use as the point color/group aesthetic.
  rename(Population=metric)

# data_fit$Education <- str_wrap(data_fit$Education, width = 10)

# --- Build Model-Fit Scatter Plot ---

# Each point is one (Education group, moment) combination.
# Proximity to the 45-degree line indicates good structural model fit.
g<-ggplot(data=data_fit,aes(x=Estimated,y=Data,group=Population,color=Population))+
  # Free scales allow the NNN panel (large probabilities) to differ from the
  # other-moments panel (small probabilities near zero).
  facet_wrap(~ Panel,scales="free") +
  geom_point()+
  # 45-degree reference line: perfect model fit lies on this line.
  geom_abline(slope = 1, intercept = 0, color = "black") +
  theme(strip.text.x = element_text(size=18),
        axis.text.x=element_text(angle = 0, vjust=0.5,size=14),
        axis.text.y = element_text(size=14),
        axis.title.x = element_text(size=18),
        axis.title.y = element_text(size=18),
        legend.title=element_text(size=18),
        legend.text=element_text(size=18))+
  # Axes are swapped relative to convention: x = model, y = data, so deviations
  # from the diagonal are visually interpreted as model over/under-prediction.
  xlab("Data Moments")+
  ylab("Estimated Moments")+
  # scale_x_continuous(breaks=0:10)+
  #scale_y_continuous(breaks=seq(from = 0, to = 1, by = 0.05),labels=function(x) format(x, big.mark = ",", scientific = FALSE))+
   # scale_x_discrete(labels = function(x) str_wrap(x, width = 8))+
  labs(color=str_wrap("Moment",width=10))

# --- Save Output ---

# Output: Figure A09.pdf -- structural model fit scatter (two panels)
setwd(wd_output)
pdf("Figure A09.pdf",width=10,height=5)
g
dev.off()
g
