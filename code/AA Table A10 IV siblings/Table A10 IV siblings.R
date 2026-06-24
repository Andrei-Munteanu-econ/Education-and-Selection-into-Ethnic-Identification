###############################################################################
# Script:  aa_07_sibling_table.R
# Purpose: Sibling-comparison table requested by referee. 4 columns:
#
#   Col 1 "OLS, '92-'11"      -- 1992-2011 linked Roma panel, HHID_1992 FE.
#                                Sample restricted to siblings: people who had
#                                a parent linked in the 1992 household and
#                                whose 1992 HH contains >=2 such panel members.
#   Col 2 "IV, '92-'11"       -- Same sibling sample. years_2011 instrumented
#                                by years_1992. SIRSUP_1992 x AA_1992 + HHID_1992 FE.
#   Col 3 "OLS, VSN"          -- VSN within-family OLS on non-first children of
#                                Roma first child (same sample as Table A.5 col 1).
#   Col 4 "IV, VSN"           -- Same VSN sibling sample, restricted to mothers
#                                bridged to the 1992-2011 panel (via copil_id
#                                -> matches_census_final.csv -> id11_MOM -> id11).
#                                Roma-1992 mothers only. years_2011 instrumented
#                                by years_1992. Cluster mama_id. Child-birth-year FE.
#
# Input:   - data/processed/linked/data_1992_2011_roma_unique.csv
#          - data/raw/Birth/census2011_mergedwith_nv2003_2005_2011_cnp.dta
#          - data/raw/2011/matches_census_final.csv
# Output:  - output/Table A10.tex
# Deps:    fixest, modelsummary, data.table, dplyr, haven
###############################################################################


# ---- 1. Load 1992-2011 Roma panel -------------------------------------------
setwd(wd_data_linked)
data_1992_2011_r <- fread('data_1992_2011_roma_unique.csv')


# ---- 2. Build siblings-only sample for cols 1-2 -----------------------------
# "Siblings" = people who had a parent linked in the 1992 household
# (years_MOM_1992 or years_POP_1992 is non-NA in the panel, which implies the
# census merge identified a coresident mother or father at baseline), AND
# whose 1992 HHID contains >=2 such panel members.
data_siblings_11_92 <- data_1992_2011_r %>%
  mutate(had_parent_1992 = !is.na(years_MOM_1992) | !is.na(years_POP_1992)) %>%
  filter(had_parent_1992, !is.na(HHID_1992)) %>%
  group_by(HHID_1992) %>%
  filter(n() >= 2) %>%
  ungroup()


# ---- 3. MODELS 1-2: OLS and IV with HHID_1992 FE on sibling sample ----------
# Mirrors aa_02_iv_robustness_within_household.R, but on the tighter sample.

model_ols_siblings <- feols(
  data    = data_siblings_11_92 %>% filter(!is.na(years_1992)),
  fml     = ROMA_2011 ~ years_2011 | SIRSUP_1992^AA_1992 + HHID_1992,
  cluster = ~SIRSUP_1992
)
summary(model_ols_siblings)

model_iv_siblings <- feols(
  data    = data_siblings_11_92,
  fml     = ROMA_2011 ~ 1 | SIRSUP_1992^AA_1992 + HHID_1992 | years_2011 ~ years_1992,
  cluster = ~SIRSUP_1992
)
summary(model_iv_siblings)


# ---- 4. Load raw VSN birth records ------------------------------------------
# Keep copil_id (for bridging to the 1992-2011 panel in col 4).
setwd(wd_data_births)
data_births <- read_dta('census2011_mergedwith_nv2003_2005_2011_cnp.dta')

data_births <- data_births %>%
  mutate(AA = as.numeric(substr(datan, 1, 4)))

data_births <- data_births %>%
  filter(!is.na(judCensus), judCensus != 0) %>%
  filter(AA %in% c(2003, 2005:2011)) %>%
  select(mama_id, copil_id, nat, sca, AA)


# ---- 5. Build VSN within-family (sibling) sample ----------------------------
data_vsn_siblings <- data_births %>%
  filter(!is.na(mama_id)) %>%
  group_by(mama_id) %>%
  filter(n() >= 2) %>%
  arrange(AA) %>%
  mutate(
    birth_rank    = row_number(),
    ROMA_bc_first = first(nat == 12)
  ) %>%
  ungroup()

data_vsn_nonfirst <- data_vsn_siblings %>%
  filter(ROMA_bc_first == TRUE,
         birth_rank > 1) %>%
  mutate(
    EDUC_vsn = case_when(
      sca >= 1  & sca <= 40  ~ "Higher Long",
      sca >= 41 & sca <= 52  ~ "Higher Short",
      sca >= 53 & sca <= 65  ~ "Postsec",
      sca >= 66 & sca <= 67  ~ "General HS",
      sca >= 68 & sca <= 83  ~ "Specialized HS",
      sca >= 84 & sca <= 92  ~ "Vocational",
      sca == 93              ~ "Gym",
      sca %in% 94:95         ~ "Primary",
      sca >= 96 & sca <= 98  ~ "No formal",
      sca == 0 | sca == 99   ~ NA_character_
    ),
    years_vsn = case_when(
      EDUC_vsn == "Higher Long"    ~ 16,
      EDUC_vsn == "Higher Short"   ~ 14,
      EDUC_vsn == "Postsec"        ~ 13,
      EDUC_vsn == "General HS"     ~ 12,
      EDUC_vsn == "Specialized HS" ~ 12,
      EDUC_vsn == "Vocational"     ~ 13,
      EDUC_vsn == "Gym"            ~ 8,
      EDUC_vsn == "Primary"        ~ 4,
      EDUC_vsn == "No formal"      ~ 0
    )
  )


# ---- 6. MODEL 3: VSN within-family OLS (same spec as Table A.5 col 1) -------
model_ols_vsn <- feols(
  data    = data_vsn_nonfirst,
  fml     = I(nat == 12) ~ years_vsn | AA,
  cluster = ~mama_id
)
summary(model_ols_vsn)


# ---- 7. Bridge VSN mothers to the 1992-2011 panel ---------------------------
# VSN -> matches_census_final (copil_id -> id11_MOM, the mother's census id)
# -> 1992-2011 panel (id11 == id11_MOM).

setwd(wd_data_11)
filename <- 'matches_census_final.csv'
matches <- read_sample(filename) %>% select(copil_id, id11_MOM)
matches <- read_data(filename, matches)

# For each mama_id, take the first non-NA id11_MOM found among her children
# (should be consistent across siblings in the vast majority of cases).
vsn_mom_bridge <- data_vsn_nonfirst %>%
  left_join(matches, by = "copil_id") %>%
  filter(!is.na(id11_MOM)) %>%
  group_by(mama_id) %>%
  arrange(id11_MOM) %>%
  slice(1) %>%
  ungroup() %>%
  select(mama_id, id11_MOM_BC_vsn = id11_MOM)

# Sanity: how many VSN non-first siblings retain the mother bridge?
data_vsn_nonfirst_linked <- data_vsn_nonfirst %>%
  inner_join(vsn_mom_bridge, by = "mama_id") %>%
  inner_join(
    data_1992_2011_r %>%
      filter(ROMA_1992 == T) %>%
      select(id11, years_1992, years_2011, ROMA_1992, SIRSUP_1992, AA_1992),
    by = c("id11_MOM_BC_vsn" = "id11")
  )

cat("VSN non-first obs (col 3):  ", nrow(data_vsn_nonfirst), "\n")
cat("VSN non-first linked (col 4):", nrow(data_vsn_nonfirst_linked), "\n")
cat("Unique mothers in col 4:    ",
    length(unique(data_vsn_nonfirst_linked$mama_id)), "\n")


# ---- 8. MODEL 4: VSN IV -----------------------------------------------------
model_iv_vsn <- feols(
  data    = data_vsn_nonfirst_linked,
  fml     = I(nat == 12) ~ 1 | AA | years_2011 ~ years_1992,
  cluster = ~mama_id
)
summary(model_iv_vsn)


# ---- 9. Export LaTeX table --------------------------------------------------
# ---- 9. Export LaTeX table --------------------------------------------------
variables <- c(
  'years_2011'     = 'Schooling Yrs',
  'fit_years_2011' = 'Schooling Yrs',
  'years_vsn'      = 'Schooling Yrs'
)

f_big <- function(x) format(x, big.mark = ",", scientific = FALSE, nsmall = 1, digits = 1)
options(modelsummary_format_numeric_latex = "plain")
glance_custom.fixest <- function(x, ...) {
  dv <- sprintf("%.2f", base::mean(x$fitted.values + x$residuals, na.rm = TRUE))
  data.table::data.table(`Mean of DV` = dv)
}

# F-stats row: OLS columns blank; IV columns show first-stage F.
f <- data.frame(
  n    = "F-stat",
  col1 = "",
  col2 = fitstat(model_iv_siblings, "ivf")$ivf1$stat,
  col3 = "",
  col4 = fitstat(model_iv_vsn,      "ivf")$ivf1$stat
)
f <- f_big(f)

# Run modelsummary -> latex_tabular, return body (coefs + GOF), drop header row,
# inject the Sample row and a \midrule before the GOF block (the "N &" row).
ms_body <- function(models, fstat, coef_rename, align) {
  tab <- modelsummary(models,
                      estimate  = "{estimate}{stars}",
                      statistic = "std.error",
                      stars = c('$^{*}$'=0.1,'$^{**}$'=0.05,'$^{***}$'=0.01),
                      gof_map = list(list("raw"="nobs",      "clean"="N",      "fmt"=f_big),
                                     list("raw"="Mean of DV","clean"="DV Mean","fmt"="%.2f")),
                      metrics = "R2",
                      add_rows = fstat,
                      output  = "latex_tabular",
                      coef_rename = coef_rename,
                      align = align,
                      escape = FALSE)
  if (!is.character(tab)) tab <- paste(as.character(tab), collapse = "\n")
  lines <- strsplit(tab, "\n")[[1]]
  
  beg  <- grep("\\\\begin\\{tabular\\}", lines)[1]
  end  <- grep("\\\\end\\{tabular\\}",   lines)[1]
  body <- lines[(beg + 1):(end - 1)]      # header + coefs + gof
  body <- body[-1]                        # drop modelsummary header row
  
  n_row <- grep("^N &", body)[1]          # inject Sample + \midrule before GOF
  if (!is.na(n_row)) {
    sample_row <- "Sample & '92-'11 & '92-'11 & VSN & '92-VSN-'11 \\\\"
    body <- append(body, c("\\midrule", sample_row, "\\midrule"), after = n_row - 1)
  }
  
  body
}

body <- ms_body(list(
  "m1" = model_ols_siblings,
  "m2" = model_iv_siblings,
  "m3" = model_ols_vsn,
  "m4" = model_iv_vsn
), f, variables, "lcccc")

out <- c(
  "\\begin{tabular}[t]{lcccc}",
  "\\toprule",
  "& \\multicolumn{4}{c}{\\textit{Dependent Variable: }}\\\\",
  "& \\multicolumn{4}{c}{\\textit{Reported Roma Ethnicity (2011)} }\\\\",
  "& OLS '92-'11 & IV '92-'11 & OLS VSN & IV VSN \\\\",
  "\\midrule",
  body,
  "\\bottomrule",
  "\\end{tabular}"
)

setwd(wd_output)
writeLines(out, "Table A10.tex")