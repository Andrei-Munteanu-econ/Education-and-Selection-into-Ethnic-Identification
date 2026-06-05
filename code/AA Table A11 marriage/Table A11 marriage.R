###############################################################################
# Script:  aa_04_iv_spouse_het_3cat_02.R
# Purpose: How does spouse Roma ethnicity affect ethnic passing?
#          3-category baseline-only spouse split (2002-2011 sample).
#
#          Spouse category (mutually exclusive):
#            (1) Roma Spouse (Baseline)     — endline spouse present AND
#                baseline-spouse linkage exists AND baseline spouse is
#                loose-Roma at baseline (parent Roma OR self Roma in 2002)
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
#     Sample: 2002-2011 linked panel; no Roma pre-filter (cat split carries it)
#
#   TABLE 2 — Child's passing by mother's spouse ethnicity (6 cols)
#     Cols 1-3: VSN sample, DV = ROMA_bc (child birth-cert ethnicity)
#     Cols 4-6: VSN sample, DV = ROMA    (child 2011 self-declaration)
#     Same sample on both halves -> N's match column-by-column across cats.
#     Sample: no Roma pre-filter (cat split carries it)
#
# Input:   - data/linked/data_2002_2011_roma_unique.csv
#          - data/census_2011/data_2011_clean_births.csv
#          - data/census_2002/data_2002_clean.csv
# Output:  - output/04_iv_spouse_het_own_new_02_3cat.tex     (Table 1)
#          - output/04_iv_child_spouse_het_new_02_3cat.tex   (Table 2)
# Deps:    fixest, modelsummary, data.table, dplyr
###############################################################################


# =============================================================================
# 1. DATA LOADING
# =============================================================================

# ---- 1a. Load 2011 census + births data ------------------------------------
setwd(wd_data_11)
filename <- 'data_2011_clean_births.csv'
# Initial column selection keeps variables needed for ego attributes, locality
# linkage (SIRSUP), household ID, and parent/spouse ethnicity codes.
data_2011 <- read_sample(filename) %>%
  select(id11, id11_MOM, id11_MOM_BC, nat, LIM, AA, years,
         SIRUTA, SIRSUP, ROMA, ROMA_MOM, HHID,
         scoala_m, years_MOM, years_POP, SEX, category, source,
         ET_MOM, ET_POP, ET_SPOUSE)
# Full data load, then derive alternative Roma indicators:
# ROMA_bc: child declared Roma on birth certificate (nat == 12)
# ROMA_lim: mother-tongue based Roma indicator (LIM == 1201 = Romani language)
data_2011 <- read_data(filename, data_2011) %>%
  mutate(ROMA_bc  = nat == 12,
         ROMA_lim = LIM == 1201)

# Restrict to children born 2002-2011 (i.e., born after the 2002 census baseline)
# to form the intergenerational passing sample.
data_2011_kids <- data_2011 %>%
  filter(AA %in% 2002:2011)

# ---- 1b. Load linked 2002-2011 Roma panel -----------------------------------
setwd(wd_data_linked)
# Roma-only longitudinal panel: one row per uniquely matched Roma individual
# across the 2002 and 2011 censuses.
data_2002_2011_r <- fread('data_2002_2011_roma_unique.csv')
# Harmonise Roma indicators: LIM_2002 == 12 codes Romani mother tongue in 2002
# notation; LIM_2011 == 1201 is the equivalent code in 2011 notation.
data_2002_2011_r <- data_2002_2011_r %>%
  mutate(ROMA_2002_LIM = LIM_2002 == 12,
         ROMA_2011_LIM = LIM_2011 == 1201)

# Variables to carry forward when this panel acts as the mother roster for
# the intergenerational sample (VSN linkage). Includes education variables for
# both census years (used in IV) and household / locality identifiers.
mom_vars <- c("id11", "years_2002", "years_2011",
              "ROMA_2002", "ROMA_2011",
              "ROMA_2002_LIM", "ROMA_2011_LIM",
              "AA_2011", "SEX_2011",
              "years_MOM_2011", "years_POP_2011",
              "HHID_2002", "HHID_2011",
              "years_MOM_2002", "years_POP_2002",
              "SIRSUP_2011", "SIRSUP_2002",
              "AA_2002",
              "ET_SPOUSE_2011", "id02_SPOUSE", "id11_SPOUSE",
              "category_2002", "source_2002", "category_2011", "source_2011")

# ---- 1c. Mother-child linked sample (VSN) -----------------------------------
# Children -> mothers via birth certificate ID
# VSN (Vital Statistics Notification) linkage: joins each child record to its
# mother's record in the 2002-2011 Roma panel using the birth-certificate
# mother identifier (id11_MOM_BC). This is the intergenerational analysis sample.
data_mom_vsn <- data_2011_kids %>%
  inner_join(
    data_2002_2011_r %>% select(all_of(mom_vars)),
    by     = c("id11_MOM_BC" = "id11"),
    suffix = c("_CHILD", "_MOM")
  )

# ---- 1d. Build loose Roma reference sets per census -------------------------
# "Loose Roma" = self-identified Roma OR has at least one Roma parent.
# This broader definition avoids under-identifying Roma due to selective
# self-identification and is used to classify spouses at baseline.
setwd(wd_data_02)
filename <- 'data_2002_clean.csv'
data_2002 <- read_sample(filename) %>%
  select(ROMA, ET_MOM, ET_POP, ET_SPOUSE, source, id02)
data_2002 <- read_data(filename, data_2002)
# 2002 loose-Roma set: keep if self-identifies OR has a Roma parent.
data_2002_roma_loose <- data_2002 %>%
  filter(ET_MOM == 12 | ET_POP == 12 | ROMA == T)

setwd(wd_data_11)
filename <- 'data_2011_clean_births.csv'
data_2011_for_spouse <- read_sample(filename) %>%
  select(ROMA, ET_MOM, ET_POP, ET_SPOUSE, source, id11)
data_2011_for_spouse <- read_data(filename, data_2011_for_spouse)
# 2011 Roma ethnicity codes fall in the range 1200-1299 (ET_MOM/ET_POP) due to
# the revised 2011 code scheme; ROMA == T captures self-identification.
data_2011_roma_loose <- data_2011_for_spouse %>%
  filter(ET_MOM %in% 1200:1299 | ET_POP %in% 1200:1299 | ROMA == T)

# ---- 1d.bis. Full 2002-2011 linked panel for spouse-at-baseline lookup -----
# Unlike data_2002_2011_r (Roma-only), this panel covers ALL uniquely-matched
# individuals across 2002-2011, so we can look up any 2011 spouse's 2002 status
# regardless of whether they cohabited with ego at baseline. id02_SPOUSE in the
# cleaned data is a within-household link, which would miss spouses who lived
# in a different household at baseline.
setwd(wd_data_linked)
data_2002_2011_full <- fread('data_2002_2011_unique.csv',
                             select = c('id11', 'id02', 'ROMA_2002', 'ET_MOM_2002', 'ET_POP_2002'))

# id11 values for individuals who were loose-Roma at 2002 baseline
id11_loose_roma_baseline <- data_2002_2011_full %>%
  filter(ET_MOM_2002 == 12 | ET_POP_2002 == 12 | ROMA_2002 == TRUE) %>%
  pull(id11)

# id11 values that can be looked up at 2002 baseline (i.e. linked across years)
id11_linked_baseline <- data_2002_2011_full$id11

# ---- 1e. 3-category baseline-only spouse classification ---------------------
# 1 = Roma Sp. (Baseline)     : endline spouse + endline spouse linked to baseline + loose-Roma at baseline
# 2 = Non-Roma Sp. (Baseline) : endline spouse + endline spouse linked to baseline + NOT loose-Roma at baseline
# 3 = No Sp. (endline)        : no 2011 spouse identifier of any kind
# NA = dropped (endline spouse but cannot be looked up at baseline)
# Classifying spouse ethnicity at BASELINE (not endline) avoids reverse causality:
# if passing changes marital choices, endline spouse status would be endogenous.
# Individuals with a 2011 spouse who cannot be traced to 2002 are dropped because
# their pre-treatment marital status is unobservable (potentially post-baseline unions).
add_spouse_cat <- function(df) {
  df %>%
    mutate(
      # sp_absent: TRUE if neither a linked 2011 spouse ID nor an endline ET_SPOUSE code is present
      sp_absent        = is.na(id11_SPOUSE) & is.na(ET_SPOUSE_2011),
      # sp_baseline_obs: TRUE if the 2011 spouse can be matched back to a 2002 record
      sp_baseline_obs  = id11_SPOUSE %in% id11_linked_baseline,
      # sp_roma_baseline: TRUE if that baseline record meets the loose-Roma criterion
      sp_roma_baseline = id11_SPOUSE %in% id11_loose_roma_baseline,
      SPOUSE_CAT = case_when(
        sp_absent                                          ~ 3L,
        !sp_absent & sp_baseline_obs &  sp_roma_baseline   ~ 1L,
        !sp_absent & sp_baseline_obs & !sp_roma_baseline   ~ 2L,
        # Endline spouse present but baseline record unavailable -> drop (NA)
        TRUE                                               ~ NA_integer_
      )
    ) %>%
    filter(!is.na(SPOUSE_CAT))
}

data_mom_vsn <- add_spouse_cat(data_mom_vsn)

# Confirm no residual NAs slipped through the filter inside add_spouse_cat
stopifnot(!any(is.na(data_mom_vsn$SPOUSE_CAT)))

# ---- 1f. Build own-passing regression data (2002 cohort adults) -------------
setwd(wd_data_linked)
data_02 <- read_sample('data_2002_2011_roma_unique.csv')
# pop_2002: town size at baseline, used as a control / heterogeneity cut;
# cut into three bins matching the rest of the paper's heterogeneity tables.
data_02 <- read_data('data_2002_2011_roma_unique.csv', data_02) %>%
  mutate(pop_2002 = cut(pop_SIRSUP_2002,
                        breaks = c(0, 5000, 50000, Inf),
                        labels = c("<5000", "5k-50k", "50k+")))

# Apply 3-cat spouse classification (looks up endline spouse in full linked panel)
# migrant: flag for residential mobility between censuses (endline locality differs
# from baseline locality); included as a potential control / diagnostic.
data_02 <- data_02 %>%
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
    migrant = SIRSUP_2011 != SIRSUP_2002
  ) %>%
  filter(!is.na(SPOUSE_CAT)) %>%
  # Rename all "_2002" suffixes to "_baseline" for clarity in formula notation
  rename_with(.fn = ~gsub("_2002", "_baseline", .))

stopifnot(!any(is.na(data_02$SPOUSE_CAT)))

# census tag carried into data_reg; allows stacking multiple cohorts in later extensions
data_reg <- data_02 %>%
  mutate(census = "02")

# ---- Diagnostic: per-cat sample sizes before fitting regressions ------------
cat("\n[diagnostic 02_3cat] data_reg N by SPOUSE_CAT x SEX_2011:\n")
print(data_reg %>% count(SPOUSE_CAT, SEX_2011))
cat("\n[diagnostic 02_3cat] data_mom_vsn N by SPOUSE_CAT:\n")
print(data_mom_vsn %>% count(SPOUSE_CAT))


# =============================================================================
# 2. TABLE 1 — OWN PASSING BY SPOUSE ETHNICITY (3 cats x 2 sexes = 6 cols)
# =============================================================================

# IV specification: years_2011 (endogenous 2011 schooling) instrumented by
# years_baseline (1992/2002 schooling). Fixed effects: locality x birth-year
# cell (SIRSUP_baseline^AA_baseline) absorb all town-cohort level shocks.
# Standard errors clustered at the locality level (SIRSUP_baseline).
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
# IV: mother's years_2011 instrumented by years_2002 (mother's 2002 schooling).
# Fixed effects: mother's baseline locality x birth-year cell.
# This isolates the effect of maternal education on the child's declared ethnicity.
fit_child_vsn_bc <- function(cat_val) {
  feols(
    ROMA_bc ~ 1 | SIRSUP_2002^AA_2002 | years_2011 ~ years_2002,
    data    = data_mom_vsn %>% filter(SPOUSE_CAT == cat_val),
    cluster = ~SIRSUP_2002
  )
}

# VSN linkage, DV = ROMA (child 2011 self-decl)
# Same sample as above; only the DV changes -> N's match column-by-column
# Contrasting ROMA_bc (birth cert, recorded at birth) vs ROMA (2011 census,
# self-reported in adolescence) reveals whether transmission fades over time.
fit_child_vsn_roma <- function(cat_val) {
  feols(
    ROMA ~ 1 | SIRSUP_2002^AA_2002 | years_2011 ~ years_2002,
    data    = data_mom_vsn %>% filter(SPOUSE_CAT == cat_val),
    cluster = ~SIRSUP_2002
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
# Map internal coefficient names to display labels for the LaTeX table.
variables <- c(
  'years_2011'     = 'Schooling Yrs',
  'fit_years_2011' = 'Schooling Yrs'
)

# f_big: number formatter that adds thousands separators and suppresses scientific notation
f_big <- function(x) format(x, big.mark = ",", scientific = FALSE, nsmall = 1, digits = 1)
options(modelsummary_format_numeric_latex = "plain")
# glance_custom.fixest: injects the dependent-variable mean as an extra GOF row;
# computed as mean(fitted + residuals) to recover the raw DV regardless of
# fixed-effect demeaning applied internally by fixest.
glance_custom.fixest <- function(x, ...) {
  dv <- sprintf("%.2f", base::mean(x$fitted.values + x$residuals, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# gof_map controls which goodness-of-fit statistics appear in the table and
# how they are formatted; R^2 here is the within-R^2 from fixest.
gof_map <- list(
  list("raw" = "nobs",       "clean" = "N",       "fmt" = f_big),
  list("raw" = "r.squared",  "clean" = "R$^2$",   "fmt" = "%.2f"),
  list("raw" = "Mean of DV", "clean" = "DV Mean", "fmt" = "%.2f")
)

# ---- Panel A: Own passing (F-stat add_rows, exactly like 2nd script) --------
# First-stage F-statistics (Kleibergen-Paap equivalent from fixest) for the
# schooling IV; reported as an additional row below GOF statistics.
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

# Build Panel A LaTeX tabular body (own ethnic identification).
# output = "latex_tabular" returns a tabular fragment without \begin{table} wrapper,
# allowing manual stitching of two panels into one float below.
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
# First-stage F-statistics for the child regressions (mother's schooling IV).
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

# Build Panel B LaTeX tabular body (intergenerational ethnic identification).
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

# Helper to emit a full-width panel title spanning all 6 data columns
panel_title <- function(txt)
  sprintf("& \\multicolumn{6}{c}{%s} \\\\", txt)

# header rows matching the target (group labels + 2-line spouse titles + col numbers)
header_own <- c(
  "& \\multicolumn{3}{c}{Women} & \\multicolumn{3}{c}{Men} \\\\",
  "& Roma   &  Non-Roma & No      & Roma   &  Non-Roma & No  \\\\",
  "& Spouse & Spouse & Spouse   & Spouse & Spouse & Spouse \\\\",
  "& (1) & (2) & (3) & (4) & (5) & (6) \\\\"
)
# Panel 2 columns differ by DV: cols 1-3 = birth-certificate Roma; cols 4-6 = 2011 census Roma
header_child <- c(
  "& \\multicolumn{3}{c}{Birth Certificate (VSN)} & \\multicolumn{3}{c}{Census 2011} \\\\",
  "& Roma   &  Non-Roma & No      & Roma   &  Non-Roma & No  \\\\",
  "& Spouse & Spouse & Spouse   & Spouse & Spouse & Spouse \\\\",
  "& (1) & (2) & (3) & (4) & (5) & (6) \\\\"
)

# Assemble the final LaTeX tabular environment by interleaving both panels
# with shared column format, panel titles, midrules, and a single \bottomrule.
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

# Output: Table A11.tex — two-panel LaTeX tabular saved to the output directory
setwd(wd_output)
writeLines(stitched, "Table A11.tex")
