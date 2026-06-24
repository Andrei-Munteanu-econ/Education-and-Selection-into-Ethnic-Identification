# =====================================================================
# Figure A01 — Singleton cells and match rates by town population
# Produces:  output/Figure A01.pdf
# Inputs:    data_2002_2011_unique.csv, data_1992_2011_unique.csv (linked
#            census samples), data_2011_clean.csv, data_2002_clean.csv,
#            data_1992_clean.csv (per-census cleaned microdata)
# Summary:   For each locality (SIRSUP), computes the share of births that
#            fall in singleton cells and the match rate among those
#            singletons, separately for the 1992->2011 and 2002->2011
#            linkages. Aggregates these to locality-population bins and
#            plots both quantities against population bin (two panels).
# =====================================================================

# ---- Load linked samples and per-census cleaned microdata ----
setwd(wd_data_linked)
data_2002_2011<-fread('data_2002_2011_unique.csv')
data_1992_2011<-fread('data_1992_2011_unique.csv')
setwd(wd_data_02)
data_2002<-fread('data_2002_clean.csv')
setwd(wd_data_92)
data_1992<-fread('data_1992_clean.csv')

# ---- Keep only the columns needed: cell id, locality code, locality population ----
data_2002<-data_2002 %>%
  select(cell_id,SIRSUP,pop=pop_SIRSUP_2002)
data_1992<-data_1992 %>%
  select(cell_id,SIRSUP,pop=pop_SIRSUP_1992)
data_2002_2011<-data_2002_2011 %>%
  select(cell_id_2002) %>%
  mutate(matched=1)
data_1992_2011<-data_1992_2011 %>%
  select(cell_id_1992) %>%
  mutate(matched=1)



# ---- 2002->2011: collapse to locality level, computing singleton share and match rate ----
# Join the matched flag onto every 2002 cell, count people and singleton cells per
# locality, and compute the share of births in singleton cells plus the match rate
# among singletons.




DT <- as.data.table(data_2002 %>% left_join(data_2002_2011,by=c("cell_id"="cell_id_2002")))
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

# ---- 1992->2011: same locality-level singleton share and match rate ----
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

# ---- Stack the two linkages, label by baseline census ----
plot_data <- bind_rows(
  singleton_share_92_11 %>% mutate(cohort = "1992"),
  singleton_share_02_11 %>% mutate(cohort = "2002")
)

# ---- Bin localities by population into short, readable size brackets ----
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

# ---- Aggregate to (population bin, cohort): people-weighted mean singleton share and match rate ----
plot_binned <- plot_data %>%
  group_by(pop_bin, cohort) %>%
  summarise(
    prop_singletons  = weighted.mean(share_singleton, w = n_people, na.rm = TRUE),
    match_singletons = weighted.mean(matched, w = n_people, na.rm = TRUE),
    .groups = "drop"
  )

# ---- Panel 1: singletons as a share of total births, by population bin ----
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

# ---- Panel 2: match rate among singleton cells, by population bin ----
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

# ---- Combine the two panels side by side and write to PDF ----
g_final <- p1 + p2

setwd(wd_output)
pdf("Figure A01.pdf",width=12,height=6)
g_final
dev.off()  


