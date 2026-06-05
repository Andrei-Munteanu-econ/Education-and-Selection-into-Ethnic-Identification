###############################################################################
# Script:  aa_04_iv_spouse_het_3cat_92.R
# Purpose: How does spouse Roma ethnicity affect ethnic passing?
#          3-category baseline-only spouse split (1992-2011 sample).
#
#          Spouse category (mutually exclusive):
#            (1) Roma Spouse (Baseline)     — endline spouse present AND
#                baseline-spouse linkage exists AND baseline spouse is
#                loose-Roma at baseline (parent Roma OR self Roma in 1992)
#            (2) Non-Roma Spouse (Baseline) — endline spouse present AND
#                baseline-spouse linkage exists AND baseline spouse is NOT
#                loose-Roma at baseline
#            (3) No Spouse (endline)        — no 2011 spouse identifier
#          Dropped: endline spouse present but no baseline spouse linkage
#          (married post-baseline; baseline status unobservable).
#
#   TABLE 1 — Own passing by spouse ethnicity (6 cols)
#     Cols 1-3: Women — Roma Sp. (Base) | Non-Roma Sp. (Base) | No Sp.
#     Cols 4-6: Men   — Roma Sp. (Base) | Non-Roma Sp. (Base) | No Sp.
#     DV: ROMA_2011 (own 2011 self-declaration)
#     Sample: 1992-2011 linked panel; no Roma pre-filter (cat split carries it)
#
#   TABLE 2 — Child's passing by mother's spouse ethnicity (6 cols)
#     Cols 1-3: VSN sample, DV = ROMA_bc (child birth-cert ethnicity)
#     Cols 4-6: VSN sample, DV = ROMA    (child 2011 self-declaration)
#     Same sample on both halves -> N's match column-by-column across cats.
#     Sample: no Roma pre-filter (cat split carries it)
#
# Input:   - data/linked/data_1992_2011_roma_unique.csv
#          - data/census_2011/data_2011_clean_births.csv
#          - data/census_1992/data_1992_clean.csv
# Output:  - output/04_iv_spouse_het_own_new_92_3cat.tex     (Table 1)
#          - output/04_iv_child_spouse_het_new_92_3cat.tex   (Table 2)
# Deps:    fixest, modelsummary, data.table, dplyr
###############################################################################


# =============================================================================
# 1. DATA LOADING
# =============================================================================

# ---- 1a. Load 2011 census + births data ------------------------------------
setwd(wd_data_11)
filename <- 'data_2011_clean_births.csv'
# Select only the variables needed for this analysis; keep the data footprint small
data_2011 <- read_sample(filename) %>%
  select(id11, id11_MOM, id11_MOM_BC, nat, LIM, AA, years,
         SIRUTA, SIRSUP, ROMA, ROMA_MOM, HHID,
         scoala_m, years_MOM, years_POP, SEX, category, source,
         ET_MOM, ET_POP, ET_SPOUSE)
# read_data applies any project-specific post-processing (type casting, filters);
# ROMA_bc = birth-certificate Roma declaration (nat == 12); ROMA_lim = Romani-language Roma flag
data_2011 <- read_data(filename, data_2011) %>%
  mutate(ROMA_bc  = nat == 12,
         ROMA_lim = LIM == 1201)

# Restrict to children born 2002-2011 for the intergenerational sample
data_2011_kids <- data_2011 %>%
  filter(AA %in% 2002:2011)

# ---- 1b. Load linked 1992-2011 Roma panel -----------------------------------
setwd(wd_data_linked)
# This is the 1:1 uniquely-matched Roma panel used in all main IV regressions
data_1992_2011_r <- fread('data_1992_2011_roma_unique.csv')
# Construct language-based Roma flags for both baseline and endline
# LIM_1992 == 12 means Romani declared as mother tongue in 1992; LIM_2011 == 1201 is equivalent 2011 code
data_1992_2011_r <- data_1992_2011_r %>%
  mutate(ROMA_1992_LIM = LIM_1992 == 12,
         ROMA_2011_LIM = LIM_2011 == 1201)

# Variables to pull from the linked panel when merging onto the mother-child sample
mom_vars <- c("id11", "years_1992", "years_2011",
              "ROMA_1992", "ROMA_2011",
              "ROMA_1992_LIM", "ROMA_2011_LIM",
              "AA_2011", "SEX_2011",
              "years_MOM_2011", "years_POP_2011",
              "HHID_1992", "HHID_2011",
              "years_MOM_1992", "years_POP_1992",
              "SIRSUP_2011", "SIRSUP_1992",
              "AA_1992",
              "ET_SPOUSE_2011", "id92_SPOUSE", "id11_SPOUSE",
              "category_1992", "source_1992", "category_2011", "source_2011")

# ---- 1c. Mother-child linked sample (VSN) -----------------------------------
# Children -> mothers via birth certificate ID
# id11_MOM_BC links a child's birth-certificate record to the mother's 2011 individual ID;
# joining onto the Roma linked panel gives the mother's education history and ethnicity flags
data_mom_vsn <- data_2011_kids %>%
  inner_join(
    data_1992_2011_r %>% select(all_of(mom_vars)),
    by     = c("id11_MOM_BC" = "id11"),
    suffix = c("_CHILD", "_MOM")
  )

# ---- 1d. Build loose Roma reference sets per census -------------------------
# "Loose Roma" = self-identifies OR has at least one Roma parent; used to classify
# a spouse's Roma background even when the spouse does not self-identify as Roma
setwd(wd_data_92)
filename <- 'data_1992_clean.csv'
data_1992 <- read_sample(filename) %>%
  select(ROMA, ET_MOM, ET_POP, ET_SPOUSE, source, id92)
data_1992 <- read_data(filename, data_1992)
# ET_MOM / ET_POP == 12 means the person's recorded mother/father is Roma (ZZ == 12)
data_1992_roma_loose <- data_1992 %>%
  filter(ET_MOM == 12 | ET_POP == 12 | ROMA == T)

setwd(wd_data_11)
filename <- 'data_2011_clean_births.csv'
data_2011_for_spouse <- read_sample(filename) %>%
  select(ROMA, ET_MOM, ET_POP, ET_SPOUSE, source, id11)
data_2011_for_spouse <- read_data(filename, data_2011_for_spouse)
# ET_MOM/ET_POP codes 1200-1299 are the 2011 Roma ethnicity range (different coding scheme)
data_2011_roma_loose <- data_2011_for_spouse %>%
  filter(ET_MOM %in% 1200:1299 | ET_POP %in% 1200:1299 | ROMA == T)

# ---- 1d.bis. Full 1992-2011 linked panel for spouse-at-baseline lookup -----
# Unlike data_1992_2011_r (Roma-only), this panel covers ALL uniquely-matched
# individuals across 1992-2011, so we can look up any 2011 spouse's 1992 status
# regardless of whether they cohabited with ego at baseline. id92_SPOUSE in the
# cleaned data is a within-household link, which would miss spouses who lived
# in a different household at baseline.
setwd(wd_data_linked)
data_1992_2011_full <- fread('data_1992_2011_unique.csv',
                             select = c('id11', 'id92', 'ROMA_1992', 'ET_MOM_1992', 'ET_POP_1992'))

# id11 values for individuals who were loose-Roma at 1992 baseline
id11_loose_roma_baseline <- data_1992_2011_full %>%
  filter(ET_MOM_1992 == 12 | ET_POP_1992 == 12 | ROMA_1992 == TRUE) %>%
  pull(id11)

# id11 values that can be looked up at 1992 baseline (i.e. linked across years)
id11_linked_baseline <- data_1992_2011_full$id11

# ---- 1e. 3-category baseline-only spouse classification ---------------------
# 1 = Roma Sp. (Baseline)     : endline spouse + endline spouse linked to baseline + loose-Roma at baseline
# 2 = Non-Roma Sp. (Baseline) : endline spouse + endline spouse linked to baseline + NOT loose-Roma at baseline
# 3 = No Sp. (endline)        : no 2011 spouse identifier of any kind
# NA = dropped (endline spouse but cannot be looked up at baseline)
# Using baseline spouse ethnicity (rather than endline) avoids reverse causality:
# a spouse may also change ethnic identification over time, so endline spouse
# ethnicity is itself potentially endogenous to the same passing process.
add_spouse_cat <- function(df) {
  df %>%
    mutate(
      # sp_absent: no 2011 spouse in the household and no within-household spouse code
      sp_absent        = is.na(id11_SPOUSE) & is.na(ET_SPOUSE_2011),
      # sp_baseline_obs: the 2011 spouse can be traced back to a 1992 record
      sp_baseline_obs  = id11_SPOUSE %in% id11_linked_baseline,
      # sp_roma_baseline: the 2011 spouse was loose-Roma at 1992 baseline
      sp_roma_baseline = id11_SPOUSE %in% id11_loose_roma_baseline,
      SPOUSE_CAT = case_when(
        sp_absent                                          ~ 3L,
        !sp_absent & sp_baseline_obs &  sp_roma_baseline   ~ 1L,
        !sp_absent & sp_baseline_obs & !sp_roma_baseline   ~ 2L,
        # Endline spouse present but no baseline record: baseline status unobservable -> drop
        TRUE                                               ~ NA_integer_
      )
    ) %>%
    filter(!is.na(SPOUSE_CAT))
}

data_mom_vsn <- add_spouse_cat(data_mom_vsn)

# Confirm no unclassified observations remain after filtering
stopifnot(!any(is.na(data_mom_vsn$SPOUSE_CAT)))

# ---- 1f. Build own-passing regression data (1992 cohort adults) -------------
setwd(wd_data_linked)
data_92 <- read_sample('data_1992_2011_roma_unique.csv')
# pop_1992: town-size tercile at 1992, used as a robustness control in related specifications
data_92 <- read_data('data_1992_2011_roma_unique.csv', data_92) %>%
  mutate(pop_1992 = cut(pop_SIRSUP_1992,
                        breaks = c(0, 5000, 50000, Inf),
                        labels = c("<5000", "5k-50k", "50k+")))

# Apply 3-cat spouse classification (looks up endline spouse in full linked panel)
data_92 <- data_92 %>%
  mutate(
    sp_absent        = is.na(id11_SPOUSE) & is.na(ET_SPOUSE_2011),
    sp_baseline_obs  = id11_SPOUSE %in% id11_linked_baseline,
    sp_roma_baseline = id11_SPOUSE %in% id11_loose_roma_baseline,
    SPOUSE_CAT = case_when(
      sp_absent                                          ~ 3L,
      !sp_absent & sp_baseline_obs &  sp_roma_baseline   ~ 1L,
      !sp_absent & sp_baseline_obs & !sp_roma_baseline   ~ 2L,
      TRUE                                               ~ NA_integer_
    ),
    # migrant: indicator for cross-locality movers between 1992 and 2011
    migrant = SIRSUP_2011 != SIRSUP_1992
  ) %>%
  filter(!is.na(SPOUSE_CAT)) %>%
  # Rename suffix so FE formula can refer to baseline locality/cohort as SIRSUP_baseline
  rename_with(.fn = ~gsub("_1992", "_baseline", .))

stopifnot(!any(is.na(data_92$SPOUSE_CAT)))

# census tag allows pooled regressions across cohorts in other scripts; kept for consistency
data_reg <- data_92 %>%
  mutate(census = "92")

# ---- Diagnostic: per-cat sample sizes before fitting regressions ------------
cat("\n[diagnostic 92_3cat] data_reg N by SPOUSE_CAT x SEX_2011:\n")
print(data_reg %>% count(SPOUSE_CAT, SEX_2011))
cat("\n[diagnostic 92_3cat] data_mom_vsn N by SPOUSE_CAT:\n")
print(data_mom_vsn %>% count(SPOUSE_CAT))


# =============================================================================
# 2. TABLE 1 — OWN PASSING BY SPOUSE ETHNICITY (3 cats x 2 sexes = 6 cols)
# =============================================================================

# IV specification: years_2011 (endogenous) instrumented by years_baseline (1992 education).
# FE: locality × birth-cohort cell (SIRSUP_baseline^AA_baseline) absorbs local cohort trends.
# Clustering at the locality level accounts for within-town correlation in passing rates.
# Splitting by SPOUSE_CAT tests whether the effect of education on passing differs
# by the ethnic composition of the marriage market.
fit_own <- function(sex_val, cat_val) {
  feols(
    ROMA_2011 ~ 1 | SIRSUP_baseline^AA_baseline + census | years_2011 ~ years_baseline,
    data    = data_reg %>%
      filter(SEX_2011 == sex_val & SPOUSE_CAT == cat_val),
    cluster = ~SIRSUP_baseline
  )
}

# ---- Women (SEX_2011 == 2) --------------------------------------------------
own_women_1 <- fit_own(2, 1); summary(own_women_1)  # Roma Sp. (Baseline)
own_women_2 <- fit_own(2, 2); summary(own_women_2)  # Non-Roma Sp. (Baseline)
own_women_3 <- fit_own(2, 3); summary(own_women_3)  # No Sp.

# ---- Men (SEX_2011 == 1) ----------------------------------------------------
own_men_1 <- fit_own(1, 1); summary(own_men_1)
own_men_2 <- fit_own(1, 2); summary(own_men_2)
own_men_3 <- fit_own(1, 3); summary(own_men_3)


# =============================================================================
# 3. TABLE 2 — CHILD'S PASSING BY MOTHER'S SPOUSE ETHNICITY (3 cats x 2 = 6)
# =============================================================================

# VSN linkage, DV = ROMA_bc (child birth-cert ethnicity)
# FE: mother's 1992 locality × mother's birth cohort cell; instrument = mother's 1992 years of schooling.
# This tests whether a mother's education (via passing channel) shifts the ethnic label
# recorded on the child's birth certificate — a very early, administrative measure of
# intergenerational ethnic transmission.
fit_child_vsn_bc <- function(cat_val) {
  feols(
    ROMA_bc ~ 1 | SIRSUP_1992^AA_1992 | years_2011 ~ years_1992,
    data    = data_mom_vsn %>% filter(SPOUSE_CAT == cat_val),
    cluster = ~SIRSUP_1992
  )
}

# VSN linkage, DV = ROMA (child 2011 self-decl)
# Same sample as above; only the DV changes -> N's match column-by-column
# Comparing ROMA_bc vs. ROMA for the same child reveals whether birth-cert and
# self-reported ethnicity diverge differently by mother's education/spouse type.
fit_child_vsn_roma <- function(cat_val) {
  feols(
    ROMA ~ 1 | SIRSUP_1992^AA_1992 | years_2011 ~ years_1992,
    data    = data_mom_vsn %>% filter(SPOUSE_CAT == cat_val),
    cluster = ~SIRSUP_1992
  )
}

child_vsn_bc_1 <- fit_child_vsn_bc(1); summary(child_vsn_bc_1)
child_vsn_bc_2 <- fit_child_vsn_bc(2); summary(child_vsn_bc_2)
child_vsn_bc_3 <- fit_child_vsn_bc(3); summary(child_vsn_bc_3)

child_vsn_roma_1 <- fit_child_vsn_roma(1); summary(child_vsn_roma_1)
child_vsn_roma_2 <- fit_child_vsn_roma(2); summary(child_vsn_roma_2)
child_vsn_roma_3 <- fit_child_vsn_roma(3); summary(child_vsn_roma_3)


# =============================================================================
# 4. TABLE OUTPUT
# =============================================================================

# =============================================================================
# 4. TABLE OUTPUT  —  ONE table, TWO panels.
#    Each panel is built exactly like the tables in the 2nd script
#    (same gof_map, add_rows F-stat row, stars, coef_rename, output="latex"),
#    then the two tabular bodies are stitched into a single table.
# =============================================================================

# ---- Formatting setup (identical to 2nd script) ----------------------------
# Rename IV-fitted coefficient to match the display label used across all tables
variables <- c(
  'years_2011'     = 'Schooling Yrs',
  'fit_years_2011' = 'Schooling Yrs'
)

# f_big: comma-formatted number printer used for large counts (N rows) and F-statistics
f_big <- function(x) format(x, big.mark = ",", scientific = FALSE, nsmall = 1, digits = 1)
# Suppress modelsummary's default scientific-notation wrapping in LaTeX output
options(modelsummary_format_numeric_latex = "plain")
# glance_custom.fixest: inject dependent-variable mean as an extra GOF row;
# reconstructed as mean(fitted + residuals) since feols stores demeaned outcome
glance_custom.fixest <- function(x, ...) {
  dv <- sprintf("%.2f", base::mean(x$fitted.values + x$residuals, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# gof_map controls which fit statistics appear in the table and in what order
gof_map <- list(
  list("raw" = "nobs",       "clean" = "N",       "fmt" = f_big),
  list("raw" = "r.squared",  "clean" = "R$^2$",   "fmt" = "%.2f"),
  list("raw" = "Mean of DV", "clean" = "DV Mean", "fmt" = "%.2f")
)

# ---- Panel A: Own passing (F-stat add_rows, exactly like 2nd script) --------
# First-stage F-statistics assess instrument strength for each subsample;
# values well above 10 confirm that 1992 schooling is a strong instrument within each spouse category.
f_own <- data.frame(
  n    = "F-stat",
  col1 = fitstat(own_women_1, "ivf")$ivf1$stat,
  col2 = fitstat(own_women_2, "ivf")$ivf1$stat,
  col3 = fitstat(own_women_3, "ivf")$ivf1$stat,
  col4 = fitstat(own_men_1,   "ivf")$ivf1$stat,
  col5 = fitstat(own_men_2,   "ivf")$ivf1$stat,
  col6 = fitstat(own_men_3,   "ivf")$ivf1$stat
)
f_own <- f_big(f_own)

# Build LaTeX tabular body for Panel A (own passing); output="latex_tabular" omits
# the surrounding \begin{table} wrapper so panels can be stitched manually below
tabA <- modelsummary(
  list(
    "Roma Sp. (Baseline)"     = own_women_1,
    "Non-Roma Sp. (Baseline)" = own_women_2,
    "No Sp."                  = own_women_3,
    "Roma Sp. (Baseline)"     = own_men_1,
    "Non-Roma Sp. (Baseline)" = own_men_2,
    "No Sp."                  = own_men_3
  ),
  estimate    = "{estimate}{stars}",
  statistic   = "std.error",
  stars       = c('$^{*}$' = 0.1, '$^{**}$' = 0.05, '$^{***}$' = 0.01),
  gof_map     = gof_map,
  add_rows    = f_own,
  output      = "latex_tabular",
  coef_rename = variables,
  escape      = FALSE
)
tabA <- as.character(print(tabA))

# ---- Panel B: Child's passing (F-stat add_rows, exactly like 2nd script) ----
f_child <- data.frame(
  n    = "F-stat",
  col1 = fitstat(child_vsn_bc_1,   "ivf")$ivf1$stat,
  col2 = fitstat(child_vsn_bc_2,   "ivf")$ivf1$stat,
  col3 = fitstat(child_vsn_bc_3,   "ivf")$ivf1$stat,
  col4 = fitstat(child_vsn_roma_1, "ivf")$ivf1$stat,
  col5 = fitstat(child_vsn_roma_2, "ivf")$ivf1$stat,
  col6 = fitstat(child_vsn_roma_3, "ivf")$ivf1$stat
)
f_child <- f_big(f_child)

# Build LaTeX tabular body for Panel B (child's passing)
tabB <- modelsummary(
  list(
    "Roma Sp. (Baseline)"     = child_vsn_bc_1,
    "Non-Roma Sp. (Baseline)" = child_vsn_bc_2,
    "No Sp."                  = child_vsn_bc_3,
    "Roma Sp. (Baseline)"     = child_vsn_roma_1,
    "Non-Roma Sp. (Baseline)" = child_vsn_roma_2,
    "No Sp."                  = child_vsn_roma_3
  ),
  estimate    = "{estimate}{stars}",
  statistic   = "std.error",
  stars       = c('$^{*}$' = 0.1, '$^{**}$' = 0.05, '$^{***}$' = 0.01),
  gof_map     = gof_map,
  add_rows    = f_child,
  output      = "latex_tabular",
  coef_rename = variables,
  escape      = FALSE
)
tabB <- as.character(print(tabB))

# ---- Stitch the two panels into ONE table -----------------------------------
# Keep Panel A's full table scaffold (\begin{table}..\begin{tabular}{...}\toprule
# ... \bottomrule \end{tabular}\end{table}); inject a panel-title row above each
# panel's header, and splice Panel B's inner body (header + coefs + gof) in
# before A's closing \bottomrule.
A <- strsplit(tabA, "\n")[[1]]
B <- strsplit(tabB, "\n")[[1]]

# rows between \begin{tabular}{...} and \end{tabular}
begA <- grep("\\\\begin\\{tabular\\}", A)
endA <- grep("\\\\end\\{tabular\\}",   A)
begB <- grep("\\\\begin\\{tabular\\}", B)
endB <- grep("\\\\end\\{tabular\\}",   B)

bodyA <- A[(begA + 1):(endA - 1)]   # header + coef rows + gof rows
bodyB <- B[(begB + 1):(endB - 1)]

# split each body into: header(1) | coef block | gof block.
# header = row 1; GOF starts at the "N &" row; coefs are in between.
split_body <- function(body) {
  n_idx <- grep("^N &", body)[1]
  list(
    header = body[1],
    coefs  = body[2:(n_idx - 1)],
    gof    = body[n_idx:length(body)]
  )
}
pa <- split_body(bodyA)
pb <- split_body(bodyB)

# panel_title: produces a full-width \multicolumn spanning all 6 data columns
panel_title <- function(txt)
  sprintf("& \\multicolumn{6}{c}{%s} \\\\", txt)

# header rows matching the target (group labels + 2-line spouse titles + col numbers)
header_own <- c(
  "& \\multicolumn{3}{c}{Women} & \\multicolumn{3}{c}{Men} \\\\",
  "& Roma   &  Non-Roma & No      & Roma   &  Non-Roma & No  \\\\",
  "& Spouse & Spouse & Spouse   & Spouse & Spouse & Spouse \\\\",
  "& (1) & (2) & (3) & (4) & (5) & (6) \\\\"
)
header_child <- c(
  "& \\multicolumn{3}{c}{Birth Certificate (VSN)} & \\multicolumn{3}{c}{Census 2011} \\\\",
  "& Roma   &  Non-Roma & No      & Roma   &  Non-Roma & No  \\\\",
  "& Spouse & Spouse & Spouse   & Spouse & Spouse & Spouse \\\\",
  "& (1) & (2) & (3) & (4) & (5) & (6) \\\\"
)

# Assemble the final LaTeX tabular environment: custom column spec adds 2em space
# between the women and men blocks (or birth-cert and census blocks) for readability
stitched <- c(
  "\\begin{tabular}{lccc@{\\hspace{2em}}ccc}",
  "\\toprule",
  panel_title("Panel 1: Own Ethnic Identification"),
  "& \\multicolumn{6}{c}{\\textit{Dependent Variable: Self Reported Roma}} \\\\",
  header_own,
  "\\midrule",
  pa$coefs,
  "\\midrule",
  pa$gof,
  "\\addlinespace \\midrule",
  panel_title("Panel 2: Inter-generational Ethnic Identification"),
  "& \\multicolumn{6}{c}{\\textit{Dependent Variable: Child Reported Roma}} \\\\",
  header_child,
  "\\midrule",
  pb$coefs,
  "\\midrule",
  pb$gof,
  "\\bottomrule",
  "\\end{tabular}"
)

# Output: Table 03.tex — two-panel LaTeX table (own passing + child passing by spouse ethnicity)
setwd(wd_output)
writeLines(stitched, "Table 03.tex")
# add_column_numbers("Table 03.tex")
