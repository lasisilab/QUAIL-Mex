# 08. Forest Plot Tables
# Creates publication-ready tables summarising the forest plot results from
# 06.1 (PSS models) and 06.2 (Emotion models) and saves them into one Word doc.
# Run from project root: Rscript analysis/08.forest_plot_tables.R

suppressPackageStartupMessages({
  library(tidyverse)
  library(broom)
  library(lmtest)
  library(flextable)
  library(officer)
})

# ── Helpers ──────────────────────────────────────────────────────────────────

MISSING_CODES <- c(999, 888, 777, 666, 555, 444, 333)

clean_num <- function(x) {
  x_num <- suppressWarnings(as.numeric(as.character(x)))
  ifelse(x_num %in% MISSING_CODES, NA_real_, x_num)
}

p_fmt <- function(p) {
  case_when(
    is.na(p)   ~ "—",
    p < 0.001  ~ "<0.001",
    TRUE       ~ sprintf("%.3f", p)
  )
}

# Colour palette (shared across tables)
COL_HEADER  <- "#1A3A5C"
COL_SUBHEAD <- "#2E6DA4"
COL_RANK    <- "#D6EAF8"
COL_SIG     <- "#D5F5E3"
COL_NS      <- "#F9F9F9"
COL_GROUP   <- "#EAF2FB"   # pale blue for group header rows

# ── Load data ─────────────────────────────────────────────────────────────────

df_raw <- read_csv("./data/03.Merged_Q1-6_Q16_Q19_Q26_Q28.csv",
                   show_col_types = FALSE)

hwise_items <- c("HW_WORRY", "HW_INTERR", "HW_CLOTHES", "HW_PLANS",
                 "HW_FOOD",  "HW_HANDS",  "HW_BODY",    "HW_DRINK",
                 "HW_ANGRY", "HW_SLEEP",  "HW_NONE",    "HW_SHAME")

hwise_readable <- c(
  HW_WORRY   = "Worry about water",
  HW_INTERR  = "Interrupted supply",
  HW_CLOTHES = "Couldn't wash clothes",
  HW_PLANS   = "Plans disrupted",
  HW_FOOD    = "Food preparation affected",
  HW_HANDS   = "Couldn't wash hands",
  HW_BODY    = "Couldn't bathe",
  HW_DRINK   = "Not enough to drink",
  HW_ANGRY   = "Felt angry",
  HW_SLEEP   = "Sleep disrupted",
  HW_NONE    = "No water at all",
  HW_SHAME   = "Felt shame"
)

term_readable <- c(
  HRS_PER_WEEK                         = "Hours of supply / week",
  CONTINTIntermittent                  = "Intermittent vs. continuous supply",
  SEASONDry                            = "Dry vs. rainy season",
  NB_COMPSame                          = "Neighborhood: same as CDMX",
  NB_COMPWorse                         = "Neighborhood: worse than CDMX",
  TIME_WATERYes                        = "Excessive time managing water",
  SUFF_RATING                          = "Sufficiency rating (1–4)",
  `HWISE_CatLow (4-11)`                = "HWISE: Low (4–11)",
  `HWISE_CatModerate/Severe (≥12)`     = "HWISE: Moderate/Severe (≥12)",
  Water_worry                          = "Water worry subscale",
  HWI_5sub_hiHigh                      = "5-item subscale: High (≥3)",
  Water_5sub                           = "5-item subscale (continuous)",
  W_PressureNo.Low                     = "Water pressure: No/Low",
  `W_PressureNo/Low`                   = "Water pressure: No/Low",
  W_FrequencyDaily                     = "Supply frequency: Daily",
  `W_Frequency4–6 days/week`           = "Supply frequency: 4–6 days/wk",
  `W_Frequency1–3 days/week`           = "Supply frequency: 1–3 days/wk",
  W_FrequencyIntermittent              = "Supply frequency: Intermittent",
  `W_WS_LOC_FWater Insecure`           = "Neighborhood: Water Insecure"
)

group_panel <- c(
  "m_a6: Hours supply"            = "Water Supply",
  "m_a7: Intermittent"            = "Water Supply",
  "m_a8: Season"                  = "Water Supply",
  "m_a9: Neighborhood comparison" = "Neighborhood",
  "m_a10: Time burden"            = "Water Supply",
  "m_a11: Sufficiency"            = "Water Supply",
  "m_a12: HWISE categorical"      = "HWISE Composite",
  "m_a13: Water worry"            = "HWISE Composite",
  "m_a14: 5-item categorical"     = "HWISE Composite",
  "m_a15: 5-item continuous"      = "HWISE Composite",
  "m_a16: Water pressure"         = "Water Supply",
  "m_a17: Supply frequency"       = "Water Supply",
  "m_a18: Neighborhood"           = "Neighborhood",
  "m_b6: Hours supply"            = "Water Supply",
  "m_b7: Intermittent"            = "Water Supply",
  "m_b8: Season"                  = "Water Supply",
  "m_b9: Neighborhood comparison" = "Neighborhood",
  "m_b10: Time burden"            = "Water Supply",
  "m_b11: Sufficiency"            = "Water Supply",
  "m_b12: HWISE categorical"      = "HWISE Composite",
  "m_b13: Water worry"            = "HWISE Composite",
  "m_b14: 5-item categorical"     = "HWISE Composite",
  "m_b15: 5-item continuous"      = "HWISE Composite",
  "m_b16: Water pressure"         = "Water Supply",
  "m_b17: Supply frequency"       = "Water Supply",
  "m_b18: Neighborhood"           = "Neighborhood"
)

# ── Prepare variables (shared across both outcomes) ──────────────────────────

df <- df_raw %>%
  mutate(
    PSS_TOTAL    = clean_num(PSS_TOTAL),
    D_AGE        = clean_num(D_AGE),
    SES_SC_Total = clean_num(SES_SC_Total),
    D_HH_SIZE    = clean_num(D_HH_SIZE),
    across(all_of(hwise_items), clean_num),
    HRS_PER_WEEK = clean_num(MX5_WFREQ_HRS_PER_WEEK),
    CONT_INT = {
      raw     <- as.character(MX3_CONT_INT)
      raw_num <- suppressWarnings(as.numeric(raw))
      factor(case_when(
        raw_num %in% MISSING_CODES                       ~ NA_character_,
        raw_num == 0                                     ~ "Continuous",
        raw_num == 1                                     ~ "Intermittent",
        str_detect(str_to_lower(raw), "contin")          ~ "Continuous",
        str_detect(str_to_lower(raw), "inter")           ~ "Intermittent",
        is.na(raw) | str_trim(raw) == "" | raw == "NA"  ~ NA_character_,
        TRUE                                             ~ raw
      ), levels = c("Continuous", "Intermittent"))
    },
    SEASON = factor(SEASON, levels = c(0, 1), labels = c("Rainy", "Dry")),
    NB_COMP = factor(
      case_when(
        clean_num(MX28_WQ_COMP_CAT) == 0 ~ "Better",
        clean_num(MX28_WQ_COMP_CAT) == 1 ~ "Same",
        clean_num(MX28_WQ_COMP_CAT) == 2 ~ "Worse",
        TRUE                             ~ NA_character_
      ), levels = c("Better", "Same", "Worse")
    ),
    TIME_WATER = factor(
      case_when(
        clean_num(MX19_TIME_CAT) == 0 ~ "No",
        clean_num(MX19_TIME_CAT) == 1 ~ "Yes",
        TRUE                          ~ NA_character_
      ), levels = c("No", "Yes")
    ),
    SUFF_RATING = clean_num(MX16_SCEN_99),
    Chronic_dis = factor(
      case_when(
        clean_num(HLTH_CDIS_CAT) == 0 ~ "No",
        clean_num(HLTH_CDIS_CAT) == 1 ~ "Yes",
        TRUE                          ~ NA_character_
      ), levels = c("No", "Yes")
    ),
    HWISE_Cat = factor(
      case_when(
        is.na(clean_num(HW_TOTAL)) ~ NA_character_,
        clean_num(HW_TOTAL) <= 3   ~ "Marginal (0-3)",
        clean_num(HW_TOTAL) <= 11  ~ "Low (4-11)",
        TRUE                       ~ "Moderate/Severe (\u226512)"
      ), levels = c("Marginal (0-3)", "Low (4-11)", "Moderate/Severe (\u226512)")
    ),
    Water_worry = clean_num(HW_WORRY_SUB),
    Water_5sub  = clean_num(HW_MEX_5SUB),
    HWI_5sub_hi = factor(
      case_when(
        is.na(clean_num(HW_MEX_5SUB)) ~ NA_character_,
        clean_num(HW_MEX_5SUB) >= 3   ~ "High",
        TRUE                          ~ "Low/none"
      ), levels = c("Low/none", "High")
    ),
    W_Pressure = factor(
      case_when(
        str_detect(str_to_lower(as.character(MX6_WPRESS)),
                   "s[i\u00ed]|yes|buena|bien")  ~ "Yes",
        str_detect(str_to_lower(as.character(MX6_WPRESS)),
                   "^no|baja|poca")               ~ "No/Low",
        TRUE                                      ~ NA_character_
      ), levels = c("Yes", "No/Low")
    ),
    W_Frequency = {
      freq_raw <- if ("MX4_W_FREQ" %in% names(df_raw)) {
        dplyr::coalesce(as.character(MX4_WFREQ), as.character(MX4_W_FREQ))
      } else {
        as.character(MX4_WFREQ)
      }
      raw <- str_to_lower(as.character(freq_raw))
      factor(
        case_when(
          str_detect(raw, "^contin")           ~ "Continuous",
          str_detect(raw, "^diari|every day")  ~ "Daily",
          str_detect(raw, "4 a 6|4-6")         ~ "4\u20136 days/week",
          str_detect(raw, "1 a 3|1-3")         ~ "1\u20133 days/week",
          str_detect(raw, "^intermi")          ~ "Intermittent",
          TRUE                                 ~ NA_character_
        ),
        levels = c("Continuous", "Daily", "4\u20136 days/week",
                   "1\u20133 days/week", "Intermittent")
      )
    },
    W_WS_LOC_F = if ("W_WS_LOC" %in% names(df_raw)) {
      factor(W_WS_LOC, levels = c("WS", "WI"),
             labels = c("Water Secure", "Water Insecure"))
    } else {
      factor(NA_character_, levels = c("Water Secure", "Water Insecure"))
    },
    EMO_NEG = {
      raw <- clean_num(MX26_EM_HHW_CAT)
      case_when(raw == 0 ~ 0L, raw == 1 ~ 1L, TRUE ~ NA_integer_)
    }
  )

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION A: PSS MODELS (06.1)
# ═══════════════════════════════════════════════════════════════════════════════

core_pss <- PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis
core_terms_pss <- c("(Intercept)", "D_AGE", "SES_SC_Total", "D_HH_SIZE", "Chronic_disYes")

pss_item_specs <- setNames(
  lapply(hwise_items, function(item) list(
    label = paste0("m_a5 – ", item), group = "m_a5: HWISE items",
    formula = as.formula(paste(
      "PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis +", item))
  )),
  paste0("m_a5_", tolower(hwise_items))
)

pss_specs <- c(pss_item_specs, list(
  m_a6  = list(label = "m_a6: Hours of water supply/week",     group = "m_a6: Hours supply",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + HRS_PER_WEEK),
  m_a7  = list(label = "m_a7: Intermittent supply",            group = "m_a7: Intermittent",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + CONT_INT),
  m_a8  = list(label = "m_a8: Season (dry vs. rainy)",         group = "m_a8: Season",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + SEASON),
  m_a9  = list(label = "m_a9: Neighborhood comparison",        group = "m_a9: Neighborhood comparison",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + NB_COMP),
  m_a10 = list(label = "m_a10: Excessive time handling water", group = "m_a10: Time burden",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + TIME_WATER),
  m_a11 = list(label = "m_a11: Sufficiency rating",            group = "m_a11: Sufficiency",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + SUFF_RATING),
  m_a12 = list(label = "m_a12: HWISE (categorical)",           group = "m_a12: HWISE categorical",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + HWISE_Cat),
  m_a13 = list(label = "m_a13: Water worry subscale",          group = "m_a13: Water worry",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + Water_worry),
  m_a14 = list(label = "m_a14: 5-item subscale (categorical)", group = "m_a14: 5-item categorical",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + HWI_5sub_hi),
  m_a15 = list(label = "m_a15: 5-item subscale (continuous)",  group = "m_a15: 5-item continuous",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + Water_5sub),
  m_a16 = list(label = "m_a16: Water pressure",                group = "m_a16: Water pressure",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + W_Pressure),
  m_a17 = list(label = "m_a17: Supply frequency",              group = "m_a17: Supply frequency",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + W_Frequency),
  m_a18 = list(label = "m_a18: Neighborhood (WS/WI)",          group = "m_a18: Neighborhood",
               formula = PSS_TOTAL ~ D_AGE + SES_SC_Total + D_HH_SIZE + Chronic_dis + W_WS_LOC_F)
))

pss_fits <- lapply(pss_specs, function(s) lm(s$formula, data = df, na.action = na.omit))

pss_raw <- map_dfr(names(pss_fits), function(nm) {
  mod  <- pss_fits[[nm]]
  spec <- pss_specs[[nm]]
  fit  <- glance(mod)
  rows_used <- as.integer(rownames(model.frame(mod)))
  m0  <- lm(core_pss, data = df[rows_used, ])
  lrt <- anova(m0, mod)
  tidy(mod, conf.int = TRUE) %>%
    filter(!term %in% core_terms_pss) %>%
    mutate(
      model_id     = nm,
      label        = spec$label,
      group        = spec$group,
      n            = nobs(mod),
      adj_R2       = round(fit$adj.r.squared, 3),
      delta_adj_R2 = round(fit$adj.r.squared - glance(m0)$adj.r.squared, 3),
      AIC          = round(fit$AIC, 1),
      F_p          = round(lrt$`Pr(>F)`[2], 4)
    )
})

pss_bh <- pss_raw %>%
  distinct(model_id, F_p) %>%
  mutate(F_p_adj = round(p.adjust(F_p, method = "BH"), 4))

pss_summary <- pss_raw %>%
  left_join(pss_bh, by = c("model_id", "F_p")) %>%
  mutate(
    est_ci    = sprintf("%.2f [%.2f, %.2f]", estimate, conf.low, conf.high),
    p_coef    = p_fmt(p.value),
    p_adj_fmt = p_fmt(F_p_adj)
  )

# ═══════════════════════════════════════════════════════════════════════════════
# SECTION B: EMOTION MODELS (06.2)
# ═══════════════════════════════════════════════════════════════════════════════

core_emo <- EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis
core_terms_emo <- c("(Intercept)", "D_AGE", "SES_SC_Total", "D_HH_SIZE",
                    "PSS_TOTAL", "Chronic_disYes")

mcfadden_r2 <- function(mod) round(1 - mod$deviance / mod$null.deviance, 3)

emo_item_specs <- setNames(
  lapply(hwise_items, function(item) list(
    label = paste0("m_b5 – ", item), group = "m_b5: HWISE items",
    formula = as.formula(paste(
      "EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis +", item))
  )),
  paste0("m_b5_", tolower(hwise_items))
)

emo_specs <- c(emo_item_specs, list(
  m_b6  = list(label = "m_b6: Hours of water supply/week",     group = "m_b6: Hours supply",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + HRS_PER_WEEK),
  m_b7  = list(label = "m_b7: Intermittent supply",            group = "m_b7: Intermittent",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + CONT_INT),
  m_b8  = list(label = "m_b8: Season (dry vs. rainy)",         group = "m_b8: Season",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + SEASON),
  m_b9  = list(label = "m_b9: Neighborhood comparison",        group = "m_b9: Neighborhood comparison",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + NB_COMP),
  m_b10 = list(label = "m_b10: Excessive time handling water", group = "m_b10: Time burden",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + TIME_WATER),
  m_b11 = list(label = "m_b11: Sufficiency rating",            group = "m_b11: Sufficiency",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + SUFF_RATING),
  m_b12 = list(label = "m_b12: HWISE (categorical)",           group = "m_b12: HWISE categorical",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + HWISE_Cat),
  m_b13 = list(label = "m_b13: Water worry subscale",          group = "m_b13: Water worry",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + Water_worry),
  m_b14 = list(label = "m_b14: 5-item subscale (categorical)", group = "m_b14: 5-item categorical",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + HWI_5sub_hi),
  m_b15 = list(label = "m_b15: 5-item subscale (continuous)",  group = "m_b15: 5-item continuous",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + Water_5sub),
  m_b16 = list(label = "m_b16: Water pressure",                group = "m_b16: Water pressure",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + W_Pressure),
  m_b17 = list(label = "m_b17: Supply frequency",              group = "m_b17: Supply frequency",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + W_Frequency),
  m_b18 = list(label = "m_b18: Neighborhood (WS/WI)",          group = "m_b18: Neighborhood",
               formula = EMO_NEG ~ D_AGE + SES_SC_Total + D_HH_SIZE + PSS_TOTAL + Chronic_dis + W_WS_LOC_F)
))

emo_fits <- lapply(emo_specs, function(s) glm(s$formula, data = df, family = binomial, na.action = na.omit))

emo_raw <- map_dfr(names(emo_fits), function(nm) {
  mod  <- emo_fits[[nm]]
  spec <- emo_specs[[nm]]
  rows_used <- as.integer(rownames(model.frame(mod)))
  m0   <- glm(core_emo, data = df[rows_used, ], family = binomial, na.action = na.omit)
  lrt  <- lrtest(m0, mod)
  tidy(mod, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(!term %in% core_terms_emo) %>%
    mutate(
      model_id  = nm,
      label     = spec$label,
      group     = spec$group,
      n         = nobs(mod),
      mcfadden  = mcfadden_r2(mod),
      delta_r2  = round(mcfadden_r2(mod) - mcfadden_r2(m0), 3),
      AIC       = round(glance(mod)$AIC, 1),
      LRT_p     = round(lrt$`Pr(>Chisq)`[2], 4)
    )
})

emo_bh <- emo_raw %>%
  distinct(model_id, LRT_p) %>%
  mutate(LRT_p_adj = round(p.adjust(LRT_p, method = "BH"), 4))

emo_summary <- emo_raw %>%
  left_join(emo_bh, by = c("model_id", "LRT_p")) %>%
  mutate(
    est_ci    = sprintf("%.2f [%.2f, %.2f]", estimate, conf.low, conf.high),
    p_coef    = p_fmt(p.value),
    p_adj_fmt = p_fmt(LRT_p_adj)
  )

# ═══════════════════════════════════════════════════════════════════════════════
# TABLE BUILDER HELPERS
# ═══════════════════════════════════════════════════════════════════════════════

# is_sig: helper to detect significance from formatted p string
is_sig <- function(p_str) {
  suppressWarnings(as.numeric(gsub("<0.001", "0.0005", p_str))) < 0.05
}

# Style a generic forest-plot flextable
style_forest_ft <- function(ft, sig_col = "p_adj_fmt",
                             est_col = "est_ci", p_coef_col = "p_coef") {

  data <- ft$body$dataset

  sig_rows   <- which(!data[[sig_col]] %in% c("—", NA) & is_sig(data[[sig_col]]))
  nsig_rows  <- setdiff(seq_len(nrow(data)), sig_rows)
  grp_rows   <- which(is.na(data[[est_col]]) | data[[est_col]] == "")
  sig_coef   <- which(!data[[p_coef_col]] %in% c("—", NA) &
                        suppressWarnings(as.numeric(gsub("<", "0", data[[p_coef_col]]))) < 0.05)
  nsig_coef  <- setdiff(seq_len(nrow(data)), sig_coef)

  ft <- ft %>%
    # ── Header ──────────────────────────────────────────────────────────────
    bg(bg = COL_HEADER, part = "header") %>%
    color(color = "white", part = "header") %>%
    bold(part = "header") %>%
    fontsize(size = 10, part = "header") %>%
    # ── Body background ────────────────────────────────────────────────────
    bg(i = sig_rows,  bg = COL_SIG,   part = "body") %>%
    bg(i = nsig_rows, bg = COL_NS,    part = "body") %>%
    bg(i = grp_rows,  bg = COL_GROUP, part = "body") %>%
    bold(i = grp_rows, part = "body") %>%
    # ── p_coef colour/bold ─────────────────────────────────────────────────
    color(i = sig_coef,  j = p_coef_col, color = "#1A5276") %>%
    bold( i = sig_coef,  j = p_coef_col) %>%
    # ── p_adj colour/bold ──────────────────────────────────────────────────
    color(i = sig_rows,  j = sig_col, color = "#1E8449") %>%
    bold( i = sig_rows,  j = sig_col) %>%
    color(i = nsig_rows, j = sig_col, color = "grey55") %>%
    # ── Borders ───────────────────────────────────────────────────────────
    border_outer(border = fp_border(color = COL_HEADER, width = 1.5), part = "all") %>%
    hline(border = fp_border(color = COL_SUBHEAD, width = 0.5), part = "body") %>%
    # ── Alignment ─────────────────────────────────────────────────────────
    align(j = est_col, align = "right", part = "body") %>%
    # ── Font ──────────────────────────────────────────────────────────────
    font(fontname = "Calibri", part = "all") %>%
    fontsize(size = 9.5, part = "body") %>%
    autofit()

  ft
}

# ═══════════════════════════════════════════════════════════════════════════════
# TABLE 1 – PSS Forest Plot: Non-HWISE Predictors
# ═══════════════════════════════════════════════════════════════════════════════

pss_main <- pss_summary %>%
  filter(!grepl("m_a5", group)) %>%
  mutate(
    predictor   = coalesce(term_readable[term], term),
    panel_group = coalesce(group_panel[group], group),
    panel_group = factor(panel_group,
                         levels = c("HWISE Composite", "Water Supply", "Neighborhood"))
  ) %>%
  arrange(panel_group, AIC) %>%
  transmute(
    Panel     = as.character(panel_group),
    Predictor = predictor,
    N         = n,
    est_ci    = est_ci,
    adj_R2    = adj_R2,
    delta_r2  = delta_adj_R2,
    AIC       = AIC,
    p_coef    = p_coef,
    p_adj_fmt = p_adj_fmt
  )

# Insert group header rows
insert_group_headers <- function(df, group_col = "Panel") {
  groups <- unique(df[[group_col]])
  out <- map_dfr(groups, function(g) {
    rows <- df[df[[group_col]] == g, ]
    # Build header row: all columns as NA with correct types, then set labels
    header_row <- rows[1, ]
    for (col in names(header_row)) {
      header_row[[col]] <- NA
    }
    header_row[[group_col]] <- g
    header_row[["Predictor"]] <- g
    bind_rows(header_row, rows)
  })
  out
}

pss_main_tbl <- insert_group_headers(pss_main) %>%
  select(-Panel)

ft_pss_main <- pss_main_tbl %>%
  flextable() %>%
  set_header_labels(
    Predictor = "Predictor",
    N         = "N",
    est_ci    = "\u03b2 [95% CI]",
    adj_R2    = "Adj. R\u00b2",
    delta_r2  = "\u0394 Adj. R\u00b2",
    AIC       = "AIC",
    p_coef    = "p (coef.)",
    p_adj_fmt = "p\u2090\u2091\u2c6a (BH)"
  ) %>%
  align(j = c("N", "adj_R2", "delta_r2", "AIC", "p_coef", "p_adj_fmt"),
        align = "center", part = "all") %>%
  add_header_lines("Table 1. PSS Models — Water Predictors (m_a6–m_a18)") %>%
  bg(bg = COL_SUBHEAD, i = 1, part = "header") %>%
  color(color = "white", i = 1, part = "header") %>%
  bold(i = 1, part = "header") %>%
  add_footer_lines(paste0(
    "\u03b2 = unstandardised regression coefficient; 95% CI = confidence interval. ",
    "Adj. R\u00b2 = adjusted R-squared; \u0394 Adj. R\u00b2 = change over core-covariate-only model. ",
    "p\u2090\u2091\u2c6a (BH) = F-test p-value adjusted by Benjamini-Hochberg across all Aim 2 PSS models. ",
    "Green rows: BH-adjusted p < 0.05. Bold blue: unadjusted p < 0.05. ",
    "Core covariates: age, SES composite, household size, chronic disease."
  )) %>%
  color(color = "grey40", part = "footer") %>%
  fontsize(size = 8, part = "footer") %>%
  italic(part = "footer") %>%
  style_forest_ft()

# ═══════════════════════════════════════════════════════════════════════════════
# TABLE 2 – PSS Forest Plot: HWISE Individual Items (m_a5)
# ═══════════════════════════════════════════════════════════════════════════════

pss_hwise_tbl <- pss_summary %>%
  filter(group == "m_a5: HWISE items") %>%
  mutate(item_label = coalesce(hwise_readable[term], term)) %>%
  arrange(estimate) %>%
  transmute(
    `HWISE Item` = item_label,
    N            = n,
    est_ci       = est_ci,
    adj_R2       = adj_R2,
    delta_r2     = delta_adj_R2,
    AIC          = AIC,
    p_coef       = p_coef,
    p_adj_fmt    = p_adj_fmt
  )

ft_pss_hwise <- pss_hwise_tbl %>%
  flextable() %>%
  set_header_labels(
    `HWISE Item` = "HWISE Item",
    N            = "N",
    est_ci       = "\u03b2 [95% CI]",
    adj_R2       = "Adj. R\u00b2",
    delta_r2     = "\u0394 Adj. R\u00b2",
    AIC          = "AIC",
    p_coef       = "p (coef.)",
    p_adj_fmt    = "p\u2090\u2091\u2c6a (BH)"
  ) %>%
  align(j = c("N", "adj_R2", "delta_r2", "AIC", "p_coef", "p_adj_fmt"),
        align = "center", part = "all") %>%
  add_header_lines("Table 2. PSS Models — HWISE Individual Items (m_a5)") %>%
  bg(bg = COL_SUBHEAD, i = 1, part = "header") %>%
  color(color = "white", i = 1, part = "header") %>%
  bold(i = 1, part = "header") %>%
  add_footer_lines(paste0(
    "Each HWISE item tested in a separate model. ",
    "\u03b2 = unstandardised regression coefficient; 95% CI = confidence interval. ",
    "Rows sorted by ascending \u03b2 estimate. ",
    "p\u2090\u2091\u2c6a (BH) = F-test p-value adjusted by Benjamini-Hochberg across all Aim 2 PSS models. ",
    "Green rows: BH-adjusted p < 0.05. Bold blue: unadjusted p < 0.05."
  )) %>%
  color(color = "grey40", part = "footer") %>%
  fontsize(size = 8, part = "footer") %>%
  italic(part = "footer") %>%
  style_forest_ft()

# ═══════════════════════════════════════════════════════════════════════════════
# TABLE 3 – Emotion Forest Plot: Non-HWISE Predictors
# ═══════════════════════════════════════════════════════════════════════════════

emo_main <- emo_summary %>%
  filter(!grepl("m_b5", group)) %>%
  mutate(
    predictor   = coalesce(term_readable[term], term),
    panel_group = coalesce(group_panel[group], group),
    panel_group = factor(panel_group,
                         levels = c("HWISE Composite", "Water Supply", "Neighborhood"))
  ) %>%
  arrange(panel_group, AIC) %>%
  transmute(
    Panel     = as.character(panel_group),
    Predictor = predictor,
    N         = n,
    est_ci    = est_ci,
    mcfadden  = mcfadden,
    delta_r2  = delta_r2,
    AIC       = AIC,
    p_coef    = p_coef,
    p_adj_fmt = p_adj_fmt
  )

emo_main_tbl <- insert_group_headers(emo_main) %>%
  select(-Panel)

ft_emo_main <- emo_main_tbl %>%
  flextable() %>%
  set_header_labels(
    Predictor = "Predictor",
    N         = "N",
    est_ci    = "OR [95% CI]",
    mcfadden  = "McFadden R\u00b2",
    delta_r2  = "\u0394 McFadden R\u00b2",
    AIC       = "AIC",
    p_coef    = "p (coef.)",
    p_adj_fmt = "p\u2090\u2091\u2c6a (BH)"
  ) %>%
  align(j = c("N", "mcfadden", "delta_r2", "AIC", "p_coef", "p_adj_fmt"),
        align = "center", part = "all") %>%
  add_header_lines("Table 3. Emotion Models — Water Predictors (m_b6–m_b18)") %>%
  bg(bg = COL_SUBHEAD, i = 1, part = "header") %>%
  color(color = "white", i = 1, part = "header") %>%
  bold(i = 1, part = "header") %>%
  add_footer_lines(paste0(
    "OR = odds ratio; 95% CI = confidence interval (logistic regression). ",
    "McFadden R\u00b2 = pseudo-R\u00b2; \u0394 McFadden R\u00b2 = change over core-covariate-only model. ",
    "p\u2090\u2091\u2c6a (BH) = LRT p-value adjusted by Benjamini-Hochberg across all Aim 2 emotion models. ",
    "Green rows: BH-adjusted p < 0.05. Bold blue: unadjusted p < 0.05. ",
    "Core covariates: age, SES composite, household size, PSS total, chronic disease."
  )) %>%
  color(color = "grey40", part = "footer") %>%
  fontsize(size = 8, part = "footer") %>%
  italic(part = "footer") %>%
  style_forest_ft()

# ═══════════════════════════════════════════════════════════════════════════════
# TABLE 4 – Emotion Forest Plot: HWISE Individual Items (m_b5)
# ═══════════════════════════════════════════════════════════════════════════════

emo_hwise_tbl <- emo_summary %>%
  filter(group == "m_b5: HWISE items") %>%
  mutate(item_label = coalesce(hwise_readable[term], term)) %>%
  arrange(estimate) %>%
  transmute(
    `HWISE Item` = item_label,
    N            = n,
    est_ci       = est_ci,
    mcfadden     = mcfadden,
    delta_r2     = delta_r2,
    AIC          = AIC,
    p_coef       = p_coef,
    p_adj_fmt    = p_adj_fmt
  )

ft_emo_hwise <- emo_hwise_tbl %>%
  flextable() %>%
  set_header_labels(
    `HWISE Item` = "HWISE Item",
    N            = "N",
    est_ci       = "OR [95% CI]",
    mcfadden     = "McFadden R\u00b2",
    delta_r2     = "\u0394 McFadden R\u00b2",
    AIC          = "AIC",
    p_coef       = "p (coef.)",
    p_adj_fmt    = "p\u2090\u2091\u2c6a (BH)"
  ) %>%
  align(j = c("N", "mcfadden", "delta_r2", "AIC", "p_coef", "p_adj_fmt"),
        align = "center", part = "all") %>%
  add_header_lines("Table 4. Emotion Models — HWISE Individual Items (m_b5)") %>%
  bg(bg = COL_SUBHEAD, i = 1, part = "header") %>%
  color(color = "white", i = 1, part = "header") %>%
  bold(i = 1, part = "header") %>%
  add_footer_lines(paste0(
    "Each HWISE item tested in a separate model. ",
    "OR = odds ratio; 95% CI = confidence interval (logistic regression). ",
    "Rows sorted by ascending OR estimate. ",
    "p\u2090\u2091\u2c6a (BH) = LRT p-value adjusted by Benjamini-Hochberg across all Aim 2 emotion models. ",
    "Green rows: BH-adjusted p < 0.05. Bold blue: unadjusted p < 0.05."
  )) %>%
  color(color = "grey40", part = "footer") %>%
  fontsize(size = 8, part = "footer") %>%
  italic(part = "footer") %>%
  style_forest_ft()

# ═══════════════════════════════════════════════════════════════════════════════
# SAVE ALL FOUR TABLES INTO ONE WORD DOCUMENT
# ═══════════════════════════════════════════════════════════════════════════════

landscape_sec <- prop_section(
  page_size    = page_size(orient = "landscape"),
  page_margins = page_mar(top = 0.8, bottom = 0.8, left = 0.8, right = 0.8)
)

doc <- read_docx() %>%

  # ── Table 1: PSS main predictors ───────────────────────────────────────────
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ft_pss_main) %>%
  body_add_break(pos = "after") %>%

  # ── Table 2: PSS HWISE items ───────────────────────────────────────────────
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ft_pss_hwise) %>%
  body_add_break(pos = "after") %>%

  # ── Table 3: Emotion main predictors ──────────────────────────────────────
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ft_emo_main) %>%
  body_add_break(pos = "after") %>%

  # ── Table 4: Emotion HWISE items ──────────────────────────────────────────
  body_add_par("", style = "Normal") %>%
  body_add_flextable(ft_emo_hwise)

# Apply landscape orientation to all sections
doc <- doc %>%
  body_set_default_section(landscape_sec)

outpath <- "./output/tables/Tables_ForestPlot_Results.docx"
print(doc, target = outpath)
cat("Saved:", outpath, "\n")
