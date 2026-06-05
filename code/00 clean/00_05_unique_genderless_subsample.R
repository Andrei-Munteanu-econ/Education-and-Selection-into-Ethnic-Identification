# Builds 2002-2011 uniquely-matched sample for robustness checks; outputs data_2002_2011_unique_genderless.csv
#data ----
# --- Load 2002 genderless unique-match sample ---
# This census year serves as the baseline for the 2002-2011 panel in robustness checks.
# "Genderless" means cell_id_genderless (SIRSUP + AA + ZZ, without sex) was used for
# matching, so the uniqueness condition is looser -- only one person per locality/birth-year/
# ethnicity cell is kept. This avoids bias from local gender-ratio imbalances.
setwd(wd_data_02)
# data_02_genderless: 2002 records that are unique under the genderless matching cell
data_02_genderless<-read_sample('data_2002_unique_genderless.csv')
# Retain only the columns needed for cross-year merging and the main regressors.
# cell_id* columns include both the gendered and genderless cell identifiers.
# pop* columns carry population-weight variables used in weighted regressions.
data_02_genderless<-data_02_genderless %>%
  select(SIRUTA,SIRSUP,starts_with("cell_id"),EDUC,ROMA,SEX,starts_with("pop"),years)
# read_data() adds the full analytic variables (e.g., years_1992, category) to the
# column-filtered sample by re-joining against the complete CSV.
data_02_genderless<-read_data('data_2002_unique_genderless.csv',data_02_genderless)


# --- Load 1992 genderless unique-match sample ---
# The 1992 records are the instrument source: years_1992 (education at the earlier census)
# will be used as the IV for years_2011 in the 1992-2011 panel.
setwd(wd_data_92)
# data_92_genderless: 1992 records unique under the genderless cell
data_92_genderless<-read_sample('data_1992_unique_genderless.csv')
data_92_genderless<-data_92_genderless %>%
  select(SIRUTA,SIRSUP,starts_with("cell_id"),EDUC,ROMA,SEX,starts_with("pop"),years)
data_92_genderless<-read_data('data_1992_unique_genderless.csv',data_92_genderless)


# --- Load 2011 records uniquely matched to 2002 counterparts (genderless) ---
# These are 2011 observations whose genderless cell_id matched exactly one 2002 record.
# Used as the 2011 endpoint in the 2002-2011 robustness panel.
setwd(wd_data_11)
# data_11_02_genderless: 2011 records matched uniquely to 2002, under genderless cell
data_11_02_genderless<-read_sample('data_2011_unique_02_genderless.csv')
data_11_02_genderless<-data_11_02_genderless %>%
  select(SIRUTA,SIRSUP,starts_with("cell_id"),EDUC,ROMA,SEX,starts_with("pop"),years)
data_11_02_genderless<-read_data('data_2011_unique_02_genderless.csv',data_11_02_genderless)

# --- Load 2011 records uniquely matched to 1992 counterparts (genderless) ---
# These are 2011 observations whose genderless cell_id matched exactly one 1992 record.
# Used as the 2011 endpoint in the main 1992-2011 IV panel.
setwd(wd_data_11)
# data_11_92_genderless: 2011 records matched uniquely to 1992, under genderless cell
data_11_92_genderless<-read_sample('data_2011_unique_92_genderless.csv')
data_11_92_genderless<-data_11_92_genderless %>%
  select(SIRUTA,SIRSUP,starts_with("cell_id"),EDUC,ROMA,SEX,starts_with("pop"),years)
data_11_92_genderless<-read_data('data_2011_unique_92_genderless.csv',data_11_92_genderless)


# --- Write subsampled files ---
# Each file is a restricted version of the genderless unique-match data, kept only for
# individuals who also appear in the complementary census year (i.e., the intersection
# of uniqueness conditions from both panel endpoints). The "_subsample" suffix signals
# this mutual-uniqueness restriction, which is stricter than single-year uniqueness.
setwd(wd_data_11)
# Output: 2011 records matched uniquely to 1992, saved as the subsample for the 2002 robustness panel
fwrite(data_11_92_genderless,'data_2011_unique_02_genderless_subsample.csv')
# Output: 2011 records matched uniquely to 2002, saved as the subsample for the 1992 main panel
fwrite(data_11_02_genderless,'data_2011_unique_92_genderless_subsample.csv')
setwd(wd_data_92)
# Output: 1992 genderless subsample restricted to individuals uniquely present across panel years
fwrite(data_92_genderless,'data_1992_unique_genderless_subsample.csv')
setwd(wd_data_02)
# Output: 2002 genderless subsample restricted to individuals uniquely present across panel years
fwrite(data_02_genderless,'data_2002_unique_genderless_subsample.csv')

