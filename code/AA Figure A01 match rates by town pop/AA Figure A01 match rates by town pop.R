# Produces Figure A.1: Singleton Cells and Match Rates by Town Size
# --- Load Linked and Cross-Sectional Census Data ---
# Linked datasets contain only records that were successfully matched across censuses;
# cross-sectional files contain all individuals enumerated in the respective year.
setwd(wd_data_linked)
# data_2002_2011: matched pairs of individuals observed in both 2002 and 2011 censuses
data_2002_2011<-fread('data_2002_2011_unique.csv')
# data_1992_2011: matched pairs of individuals observed in both 1992 and 2011 censuses
data_1992_2011<-fread('data_1992_2011_unique.csv')
# data_1992_2002_2011<-fread('data_1992_2002_2011_unique.csv')
setwd(wd_data_11)
# data_2011: full cleaned 2011 census (used here only for population counts via data_2002/1992)
data_2011<-fread('data_2011_clean.csv')
setwd(wd_data_02)
data_2002<-fread('data_2002_clean.csv')
setwd(wd_data_92)
data_1992<-fread('data_1992_clean.csv')

# data_2011<-data_2011 %>%
#   select(ROMA,MEDIU,AA,LL,ZZ,pop_SIRSUP_2011,SIRSUP,cell_id_1992,cell_id_2002,SEX)
# Retain only the matching key and locality population from the 2002 cross-section;
# pop_SIRSUP_2002 is the total population of the LAU2 locality in the 2002 census
data_2002<-data_2002 %>%
  select(cell_id,SIRSUP,pop=pop_SIRSUP_2002)
# pop_SIRSUP_1992: analogous locality population from the 1992 census
data_1992<-data_1992 %>%
  select(cell_id,SIRSUP,pop=pop_SIRSUP_1992)
# Reduce the linked file to the matching key only; the 'matched' flag will mark
# which 2002 cell_ids appear in the linked dataset (i.e., were successfully matched to 2011)
data_2002_2011<-data_2002_2011 %>%
  select(cell_id_2002) %>%
  mutate(matched=1)
# Same for the 1992-2011 linkage: flag which 1992 cell_ids have a 2011 match
data_1992_2011<-data_1992_2011 %>%
  select(cell_id_1992) %>%
  mutate(matched=1)
# data_1992_2002_2011<-data_1992_2002_2011 %>%
#   select(AA_2011,pop_SIRSUP_2011,SIRSUP_2011,SIRSUP_2002,SIRSUP_1992)



# data_2011_matchable_02<-data_2011 %>%
#   filter(AA<=2001) %>%
#   group_by(SIRSUP) %>%
#   mutate()
#   group_by(SIRSUP,pop_SIRSUP_2011) %>%
#   summarise(n=n())




# --- Compute Cell-Level Match and Singleton Statistics: 2002-2011 Linkage ---
# Left-join brings the 'matched' flag onto every 2002 individual record;
# unmatched rows receive NA for 'matched', which is handled below.
DT <- as.data.table(data_2002 %>% left_join(data_2002_2011,by=c("cell_id"="cell_id_2002")))
# Two-step aggregation:
# Step 1 (inner []): collapse to the cell level (SIRSUP x pop x cell_id),
#   computing the number of individuals sharing a cell (cell_n) and the count matched.
#   cell_n == 1L identifies singleton cells -- the only cells usable for probabilistic linkage
#   since a singleton cell_id maps to exactly one person in each census.
# Step 2 (outer []): collapse to the locality level, computing:
#   n_cells       -- number of distinct matching cells in the locality
#   n_people      -- total individuals in the locality
#   n_singletons  -- individuals in cells of size 1 (matchable individuals)
#   matched       -- match rate = matched singletons / total singletons
#   share_singleton -- fraction of all individuals who fall in singleton cells
singleton_share_02_11 <- DT[,
  .(cell_n = .N,
    matched = ifelse(!is.na(sum(matched)),sum(matched),0)),
  by = .(SIRSUP,pop,cell_id)
][
  ,
  .(
    n_cells       = .N,
    n_people      = sum(cell_n),
    n_singletons  = sum(cell_n == 1L),
    matched       = sum(matched==T)/sum(cell_n == 1L),
    share_singleton = sum(cell_n == 1L) / sum(cell_n)
  ),
  by = .(SIRSUP,pop)
]

# --- Compute Cell-Level Match and Singleton Statistics: 1992-2011 Linkage ---
# Identical aggregation logic applied to the 1992 baseline; yields locality-level
# singleton shares and match rates for the longer (19-year) linkage window.
DT <- as.data.table(data_1992 %>% left_join(data_1992_2011,by=c("cell_id"="cell_id_1992")))
singleton_share_92_11 <- DT[,
                            .(cell_n = .N,
                              matched = ifelse(!is.na(sum(matched)),sum(matched),0)),
                            by = .(SIRSUP,pop,cell_id)
][
  ,
  .(
    n_cells       = .N,
    n_people      = sum(cell_n),
    n_singletons  = sum(cell_n == 1L),
    matched       = sum(matched==T)/sum(cell_n == 1L),
    share_singleton = sum(cell_n == 1L) / sum(cell_n)
  ),
  by = .(SIRSUP,pop)
]

# --- Stack Both Linkages for Plotting ---
# cohort labels the baseline census year so panels can be distinguished by color
plot_data <- bind_rows(
  singleton_share_92_11 %>% mutate(cohort = "1992"),
  singleton_share_02_11 %>% mutate(cohort = "2002")
)

# short, readable population bins
# Bins follow a roughly log-linear scale to capture variation across small villages,
# mid-size towns, and large cities; right=FALSE means each bin is [lower, upper).
plot_data <- plot_data %>%
  mutate(pop_bin = cut(
    pop,
    breaks = c(0, 1000, 5000, 10000, 50000, 200000, Inf),
    labels = c(
      "<1k", "1-5k", "5-10k", "10-50k",
      "50-200k", "200k+"
    ),
    right = FALSE
  ))

# bin-level aggregation
# Weighted means use n_people as weights so that larger localities contribute
# proportionally more to each bin statistic (population-weighted averages).
plot_binned <- plot_data %>%
  group_by(pop_bin, cohort) %>%
  summarise(
    prop_singletons  = weighted.mean(share_singleton, w = n_people, na.rm = TRUE),
    match_singletons = weighted.mean(matched, w = n_people, na.rm = TRUE),
    .groups = "drop"
  )

# --- Panel 1: Singleton Share by Town Size ---
# Visualises how the share of matchable (singleton-cell) individuals varies across
# the locality size distribution; larger towns have denser cell populations so
# fewer singletons, making matching harder there.
p1 <- ggplot(
  plot_binned,
  aes(x = pop_bin, y = prop_singletons, color = cohort, group = cohort)
) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_line(position = position_dodge(width = 0.4)) +
  theme(
    strip.text.x = element_text(size = 18),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text  = element_text(size = 9),
    legend.position = c(.78, .82),
    legend.title.align = 0.5
  ) +
  xlab("Locality population (bins)") +
  ylab("Singletons as Proportion of Total Births") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  labs(color = "Baseline Census")

# --- Panel 2: Match Rate Among Singletons by Town Size ---
# Among the matchable (singleton) individuals, shows what fraction was actually
# linked to a record in the target census; the legend is suppressed because
# cohort coloring is already explained in p1.
p2 <- ggplot(
  plot_binned,
  aes(x = pop_bin, y = match_singletons, color = cohort, group = cohort)
) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_line(position = position_dodge(width = 0.4)) +
  theme(
    strip.text.x = element_text(size = 18),
    axis.text.x  = element_text(size = 14),
    axis.text.y  = element_text(size = 14),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.title = element_text(size = 9, hjust = 0.5),
    legend.text  = element_text(size = 9),
    legend.position = "none"
  ) +
  xlab("Locality population (bins)") +
  ylab("Match rate (singletons)") +
  scale_y_continuous(labels = percent_format(accuracy = 1))

# Combine the two panels side-by-side using patchwork
g_final <- p1 + p2

# --- Output: Figure A01.pdf ---
setwd(wd_output)
pdf("Figure A01.pdf",width=12,height=6)
g_final
dev.off()


