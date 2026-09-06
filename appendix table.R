stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ " . ",
    TRUE      ~ ""
  )
}
# ---- Table 1A. Full list of countries observation from which used in the study. Countries present in both 2018 and 2020 analytical datasets -----
countries_18 <- wgm_18_merged_fin %>%
  filter(!is.na(cntry)) %>%
  distinct(cntry)

countries_20 <- wgm_20_merged_fin %>%
  filter(!is.na(cntry)) %>%
  distinct(cntry)

country_list_df <- inner_join(
  countries_18,
  countries_20,
  by = "cntry"
) %>%
  arrange(cntry) %>%
  mutate(
    Number = row_number()
  ) %>%
  dplyr::select(Number, Country = cntry)

country_list_df
country_list_table <- country_list_df %>%
  gt() %>%
  tab_header(
    title = md("**Countries included in the analysis**"),
    subtitle = "Countries present in both 2018 and 2020 analytical datasets"
  ) %>%
  cols_label(
    Number = md("**No.**"),
    Country = md("**Country**")
  ) %>%
  cols_align(
    align = "center",
    columns = Number
  ) %>%
  cols_align(
    align = "left",
    columns = Country
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_source_note(
    source_note = "Notes: The table lists countries included in both wave-specific analytical datasets."
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(4)
  )

#country_list_table %>%
#  gtsave("tab_cntry.docx")

country_list_wide_df <- country_list_df %>%
  mutate(
    group = ceiling(Number / ceiling(n() / 3)),
    row = ave(Number, group, FUN = seq_along)
  ) %>%
  dplyr::select(group, row, Country) %>%
  tidyr::pivot_wider(
    names_from = group,
    values_from = Country,
    names_prefix = "Country_"
  ) %>%
  dplyr::select(-row)

country_list_wide_table <- country_list_wide_df %>%
  gt() %>%
  tab_header(
    title = md("**Countries included in the analysis**"),
    subtitle = "Countries present in both 2018 and 2020 analytical datasets"
  ) %>%
  cols_label(
    Country_1 = md("**Country**"),
    Country_2 = md("**Country**"),
    Country_3 = md("**Country**")
  ) %>%
  cols_align(
    align = "left",
    columns = everything()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_source_note(
    source_note = "Notes: The table lists countries included in both wave-specific analytical datasets."
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(4)
  )

#country_list_wide_table %>%
#  gtsave("tab_cntry_2.docx")

# ---- Table 2A. Variables used in the analysis with definitions, sources, and observed ranges or categories ----
# Helper functions 
get_range <- function(data, var, digits = 3) {
  x <- data[[var]]
  x <- x[!is.na(x)]
  
  if (length(x) == 0) return("—")
  
  paste0(
    round(min(x), digits),
    " to ",
    round(max(x), digits)
  )
}

get_n_levels <- function(data, var) {
  x <- data[[var]]
  x <- x[!is.na(x)]
  length(unique(x))
}

# Wave Ns

wave_n <- wgm_pooled_fin %>%
  filter(!is.na(wave)) %>%
  mutate(
    wave_label = case_when(
      wave == 0 ~ "2018",
      wave == 1 ~ "2020",
      TRUE ~ as.character(wave)
    )
  ) %>%
  count(wave_label, name = "n") %>%
  mutate(n_fmt = format(n, big.mark = ","))

wave_n_note <- paste0(
  wave_n$wave_label,
  ": ",
  wave_n$n_fmt,
  collapse = "; "
)

# Variable table

variable_table_df <- tibble::tribble(
  ~Variable, ~Meaning, ~Source, ~Values_or_range,
  
  "sti_3cat",
  "Three-category integrated measure of trust in science and trust in scientists",
  "Self-created from Wellcome Global Monitor",
  "Low trust; Medium trust; High trust",
  
  "cntry",
  "Country of respondent residence",
  "Wellcome Global Monitor",
  paste0(get_n_levels(wgm_pooled_fin, "cntry"), " levels"),
  
  "regions",
  "Region of respondent country",
  "Wellcome Global Monitor / harmonized across waves",
  paste0(get_n_levels(wgm_pooled_fin, "regions"), " levels"),
  
  "wave",
  "Survey wave indicator",
  "Self-created from Wellcome Global Monitor",
  "0 = 2018; 1 = 2020",
  
  "Age_c",
  "Centered age of respondent",
  "Self-created from Wellcome Global Monitor",
  get_range(wgm_pooled_fin, "Age_c"),
  
  "Gender",
  "Gender of respondent",
  "Wellcome Global Monitor",
  "1 = Male; 2 = Female",
  
  "Education",
  "Educational attainment of respondent",
  "Wellcome Global Monitor / harmonized across waves",
  "Primary; Secondary; Tertiary",
  
  "Subjective_Income",
  "Subjective household income situation",
  "Wellcome Global Monitor / harmonized across waves",
  "1 = Living comfortably on present income; 2 = Getting by on present income; 3 = Finding it difficult or very difficult to get by on present income",
  
  "wbi",
  "World Bank income group of respondent country",
  "World Bank / merged to WGM country data",
  "1 = Low income; 2 = Lower-middle income; 3 = Upper-middle income; 4 = High income",
  
  "RoW",
  "Regime type according to Regimes of the World classification",
  "V-Dem Institute",
  "Closed autocracy; Electoral autocracy; Electoral democracy; Liberal democracy",
  
  "raw_lib_dem",
  "Binary indicator of liberal democracy",
  "Self-created from RoW classification",
  "0 = Not liberal democracy; 1 = Liberal democracy",
  
  "raw_auto_dem",
  "Binary democracy/autocracy indicator",
  "Self-created from RoW classification",
  "0 = Autocracy; 1 = Democracy",
  
  "cce",
  "Control of Corruption estimate, pre-centered",
  "World Bank Worldwide Governance Indicators",
  get_range(wgm_pooled_fin, "cce"),
  
  "LDI_c",
  "Centered Liberal Democracy Index",
  "Self-created from V-Dem Institute data",
  get_range(wgm_pooled_fin, "LDI_c"),
  
  "log_gdp_pc_c",
  "Centered logged GDP per capita",
  "World Bank / IMF for Taiwan; self-transformed",
  get_range(wgm_pooled_fin, "log_gdp_pc_c"),
  
  "log_rnd_c",
  "Centered logged national R&D expenditure",
  "World Bank; partially imputed using income-region group medians",
  get_range(wgm_pooled_fin, "log_rnd_c"),
  
  "gini_c",
  "Centered Gini index",
  "World Bank; partially imputed using income-region group medians; National Science and Technology Council for Taiwan",
  get_range(wgm_pooled_fin, "gini_c")
)
variable_table <- variable_table_df %>%
  gt() %>%
  tab_header(
    title = md("**Variables used in the analysis**"),
    subtitle = "Definitions, sources, and observed ranges or categories"
  ) %>%
  cols_label(
    Variable = md("**Variable**"),
    Meaning = md("**Meaning**"),
    Source = md("**Source**"),
    Values_or_range = md("**Values / range**")
  ) %>%
  cols_align(
    align = "left",
    columns = c(Variable, Meaning, Source, Values_or_range)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = Variable)
  ) %>%
  tab_source_note(
    source_note = paste0(
      "Notes: Ranges are calculated from the pooled analytical dataset. ",
      "Observation counts by wave are: ", wave_n_note, ". ",
      "For centered variables, the reported range refers to the centered scale used in the models."
    )
  ) %>%
  tab_options(
    table.font.size = px(12),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(4)
  )

variable_table
#variable_table %>%
#  gtsave("tab_var.docx")
# ---- Table 3A. Full regression table: 2018 education-specific LDI model -----

m18_full_table_df <- as.data.frame(m18[,]) %>%
  rownames_to_column("term") %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    t_value = `t value`,
    p_value = `Pr(>|t|)`
  ) %>%
  mutate(
    stars = stars(p_value),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    std_error_fmt = sprintf("%.3f", std_error),
    t_value_fmt = sprintf("%.3f", t_value),
    p_value_fmt = case_when(
      p_value < .001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  ) %>%
  dplyr::select(
    term,
    estimate_fmt,
    std_error_fmt,
    t_value_fmt,
    p_value_fmt
  )
m18_full_table <- m18_full_table_df %>%
  gt() %>%
  tab_header(
    title = md("**Full regression table: 2018 education-specific LDI model**"),
    subtitle = "Ordered-logit model for the three-category integrated trust variable"
  ) %>%
  cols_label(
    term = md("**Predictor**"),
    estimate_fmt = md("**Estimate**"),
    std_error_fmt = md("**Clustered SE**"),
    t_value_fmt = md("**t value**"),
    p_value_fmt = md("**p-value**")
  ) %>%
  cols_align(
    align = "center",
    columns = c(estimate_fmt, std_error_fmt, t_value_fmt, p_value_fmt)
  ) %>%
  cols_align(
    align = "left",
    columns = term
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = term)
  ) %>%
  tab_source_note(
    source_note = "Notes: Coefficients are ordered-logit estimates on the log-odds scale. Standard errors are clustered by country. . p < .10; * p < .05; ** p < .01; *** p < .001."
  ) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(3)
  )

m18_full_table
#m18_full_table %>%
#  gtsave("m18_full.docx")

# ---- Table 4A: Full regression tables: 2018 threshold robustness models-----

prep_full_coef_long <- function(coef_obj, model_label) {
  
  as.data.frame(coef_obj[,]) %>%
    rownames_to_column("term") %>%
    rename(
      estimate = Estimate,
      std_error = `Std. Error`,
      t_value = `t value`,
      p_value = `Pr(>|t|)`
    ) %>%
    mutate(
      model = model_label,
      stars = stars(p_value),
      estimate_fmt = sprintf("%.3f%s", estimate, stars),
      std_error_fmt = sprintf("%.3f", std_error),
      t_value_fmt = sprintf("%.3f", t_value),
      p_value_fmt = case_when(
        p_value < .001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", p_value)
      )
    ) %>%
    dplyr::select(
      model,
      term,
      estimate_fmt,
      std_error_fmt,
      t_value_fmt,
      p_value_fmt
    )
}

coef_18_threshold_long_df <- bind_rows(
  prep_full_coef_long(coef_18_ldi_05, "LDI threshold 0.5"),
  prep_full_coef_long(coef_18_ldi_06, "LDI threshold 0.6"),
  prep_full_coef_long(coef_18_lib_dem, "Liberal democracy"),
  prep_full_coef_long(coef_18_auto_dem, "Autocracy vs democracy")
)
coef_18_threshold_long_table <- coef_18_threshold_long_df %>%
  gt(
    groupname_col = "model",
    rowname_col = "term"
  ) %>%
  tab_header(
    title = md("**Full regression tables: 2018 threshold robustness models**"),
    subtitle = "Ordered-logit models for the three-category integrated trust variable"
  ) %>%
  cols_label(
    estimate_fmt = md("**Estimate**"),
    std_error_fmt = md("**Clustered SE**"),
    t_value_fmt = md("**t value**"),
    p_value_fmt = md("**p-value**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_align(
    align = "left",
    columns = stub()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#D9D9D9"),
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>%
  tab_source_note(
    source_note = "Notes: Coefficients are ordered-logit estimates on the log-odds scale. Standard errors are clustered by country. The table reports full model coefficients for alternative operationalizations of political regime used in the threshold sensitivity test. . p < .10; * p < .05; ** p < .01; *** p < .001."
  ) %>%
  tab_options(
    table.font.size = px(10),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    data_row.padding = px(3)
  )

coef_18_threshold_long_table

#coef_18_threshold_long_table %>%
#  gtsave("threshold_model.docx")
#----- Table 5A: Full regression tables: 2018 trimming robustness models ----

prep_coef_block_trim <- function(coef_obj, model_label) {
  
  as.data.frame(coef_obj[,]) %>%
    rownames_to_column("term") %>%
    rename(
      estimate = Estimate,
      std_error = `Std. Error`,
      t_value = `t value`,
      p_value = `Pr(>|t|)`
    ) %>%
    mutate(
      stars = stars(p_value),
      estimate_fmt = sprintf("%.3f%s", estimate, stars),
      std_error_fmt = sprintf("%.3f", std_error),
      p_value_fmt = case_when(
        p_value < .001 ~ "<0.001",
        TRUE ~ sprintf("%.3f", p_value)
      )
    ) %>%
    dplyr::select(term, estimate_fmt, std_error_fmt, p_value_fmt) %>%
    rename(
      !!paste0(model_label, "_est") := estimate_fmt,
      !!paste0(model_label, "_se") := std_error_fmt,
      !!paste0(model_label, "_p") := p_value_fmt
    )
}
tab_left_tail <- prep_coef_block_trim(
  coef_obj = m18_left_tail,
  model_label = "left"
)

tab_both_tails <- prep_coef_block_trim(
  coef_obj = m18_both_tails,
  model_label = "both"
)
coef_18_trim_df <- tab_left_tail %>%
  full_join(tab_both_tails, by = "term")

coef_18_trim_table <- coef_18_trim_df %>%
  gt() %>%
  tab_header(
    title = md("**Full regression tables: 2018 trimming robustness models**"),
    subtitle = "Ordered-logit models for the three-category integrated trust variable"
  ) %>%
  tab_spanner(
    label = md("**Left-tail trim**"),
    columns = c(left_est, left_se, left_p)
  ) %>%
  tab_spanner(
    label = md("**Both-tail trim**"),
    columns = c(both_est, both_se, both_p)
  ) %>%
  cols_label(
    term = md("**Predictor**"),
    left_est = md("**Est.**"),
    left_se = md("**SE**"),
    left_p = md("**p**"),
    both_est = md("**Est.**"),
    both_se = md("**SE**"),
    both_p = md("**p**")
  ) %>%
  cols_align(
    align = "center",
    columns = -term
  ) %>%
  cols_align(
    align = "left",
    columns = term
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#D9D9D9"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_spanners()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = term)
  ) %>%
  tab_source_note(
    source_note = "Notes: Coefficients are ordered-logit estimates on the log-odds scale. Standard errors are clustered by country. The left-tail trim excludes the lowest-LDI countries; the both-tail trim excludes countries at both extremes of the LDI distribution. . p < .10; * p < .05; ** p < .01; *** p < .001."
  ) %>%
  tab_options(
    table.font.size = px(10),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(3)
  )
coef_18_trim_table

#coef_18_trim_table%>%
#  gtsave("trim_table.docx")


# ---- Table 6A: Full GAM table: 2018 high-trust model -----

# main parameters

gam18_param_df <- as.data.frame(gam_18_reg$p.table[,]) %>%
  rownames_to_column("term") %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    statistic = `z value`,
    p_value = `Pr(>|z|)`
  ) %>%
  mutate(
    section = "Parametric coefficients",
    stars = stars(p_value),
    estimate_fmt = sprintf("%.3f%s", estimate, stars),
    std_error_fmt = sprintf("%.3f", std_error),
    statistic_fmt = sprintf("%.3f", statistic),
    p_value_fmt = case_when(
      p_value < .001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  ) %>%
  dplyr::select(
    section,
    term,
    estimate_fmt,
    std_error_fmt,
    statistic_fmt,
    p_value_fmt
  )

# Smooth terms

gam18_smooth_df <- as.data.frame(gam_18_reg$s.table[,]) %>%
  rownames_to_column("term") %>%
  rename(
    edf = edf,
    ref_df = Ref.df,
    chi_sq = Chi.sq,
    p_value = `p-value`
  ) %>%
  mutate(
    section = "Smooth terms",
    stars = stars(p_value),
    estimate_fmt = sprintf("%.3f", edf),
    std_error_fmt = sprintf("%.3f", ref_df),
    statistic_fmt = sprintf("%.3f", chi_sq),
    p_value_fmt = case_when(
      p_value < .001 ~ paste0("<0.001", stars),
      TRUE ~ sprintf("%.3f%s", p_value, stars)
    )
  ) %>%
  dplyr::select(
    section,
    term,
    estimate_fmt,
    std_error_fmt,
    statistic_fmt,
    p_value_fmt
  )

# Model fit statistics

gam18_stats_df <- tibble(
  section = "Model statistics",
  term = c(
    "N",
    "Adjusted R²",
    "Deviance explained",
    "fREML",
    "Scale estimate"
  ),
  estimate_fmt = c(
    format(nobs(gam_high_18), big.mark = ","),
    sprintf("%.3f", gam_18_reg$r.sq),
    sprintf("%.1f%%", gam_18_reg$dev.expl * 100),
    sprintf("%.1f", gam_high_18$gcv.ubre),
    sprintf("%.3f", gam_18_reg$scale)
  ),
  std_error_fmt = "",
  statistic_fmt = "",
  p_value_fmt = ""
)

# Combine all blocks

gam18_full_table_df <- bind_rows(
  gam18_param_df,
  gam18_smooth_df,
  gam18_stats_df
)

# gt table
gam18_full_table <- gam18_full_table_df %>%
  gt(
    groupname_col = "section",
    rowname_col = "term"
  ) %>%
  tab_header(
    title = md("**Full GAM table: 2018 high-trust model**"),
    subtitle = "Binomial logit GAM for high trust in the integrated trust variable"
  ) %>%
  cols_label(
    estimate_fmt = md("**Estimate / edf**"),
    std_error_fmt = md("**SE / Ref.df**"),
    statistic_fmt = md("**z / Chi-sq**"),
    p_value_fmt = md("**p-value**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_align(
    align = "left",
    columns = stub()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#D9D9D9"),
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>%
  tab_source_note(
    source_note = "Notes: The model is a binomial logit GAM where the dependent variable is high trust in the three-category integrated trust variable. Parametric coefficients are reported on the log-odds scale. Smooth terms report effective degrees of freedom (edf), reference degrees of freedom, Chi-square statistics, and p-values. The model includes education-specific smooths of LDI_c, individual controls, macro controls, regional fixed effects, and a country random intercept. . p < .10; * p < .05; ** p < .01; *** p < .001."
  ) %>%
  tab_options(
    table.font.size = px(10),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(3)
  )
gam18_full_table
#gam18_full_table %>%
#  gtsave("gam18_full_table.docx")
# ---- Table 7A. Full GAM table: pooled hybrid high-trust model ----

# Save model summary
gam_hybrid_reg <- summary(m_gam_hybrid)

# Parametric coefficients

hybrid_param_df <- as.data.frame(gam_hybrid_reg$p.table[,]) %>%
  rownames_to_column("term") %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    statistic = `z value`,
    p_value = `Pr(>|z|)`
  ) %>%
  mutate(
    section = "Parametric coefficients",
    
    omitted = is.na(statistic) | is.nan(statistic) | is.na(p_value) | is.nan(p_value),
    
    stars = stars(p_value),
    
    estimate_fmt = ifelse(
      omitted,
      "—",
      sprintf("%.3f%s", estimate, stars)
    ),
    std_error_fmt = ifelse(
      omitted,
      "—",
      sprintf("%.3f", std_error)
    ),
    statistic_fmt = ifelse(
      omitted,
      "—",
      sprintf("%.3f", statistic)
    ),
    p_value_fmt = case_when(
      omitted ~ "—",
      p_value < .001 ~ "<0.001",
      TRUE ~ sprintf("%.3f", p_value)
    )
  ) %>%
  dplyr::select(
    section,
    term,
    estimate_fmt,
    std_error_fmt,
    statistic_fmt,
    p_value_fmt
  )


# Smooth terms
hybrid_smooth_df <- as.data.frame(gam_hybrid_reg$s.table[,]) %>%
  rownames_to_column("term") %>%
  rename(
    edf = edf,
    ref_df = Ref.df,
    chi_sq = Chi.sq,
    p_value = `p-value`
  ) %>%
  mutate(
    section = "Smooth terms",
    stars = stars(p_value),
    
    estimate_fmt = sprintf("%.3f", edf),
    std_error_fmt = sprintf("%.3f", ref_df),
    statistic_fmt = sprintf("%.3f", chi_sq),
    p_value_fmt = case_when(
      p_value < .001 ~ paste0("<0.001", stars),
      TRUE ~ sprintf("%.3f%s", p_value, stars)
    )
  ) %>%
  dplyr::select(
    section,
    term,
    estimate_fmt,
    std_error_fmt,
    statistic_fmt,
    p_value_fmt
  )


# Model fit statistics

hybrid_stats_df <- tibble(
  section = "Model statistics",
  term = c(
    "N",
    "Adjusted R²",
    "Deviance explained",
    "fREML",
    "Scale estimate",
    "Rank"
  ),
  estimate_fmt = c(
    format(nobs(m_gam_hybrid), big.mark = ","),
    sprintf("%.3f", gam_hybrid_reg$r.sq),
    sprintf("%.1f%%", gam_hybrid_reg$dev.expl * 100),
    sprintf("%.1f", m_gam_hybrid$gcv.ubre),
    sprintf("%.3f", gam_hybrid_reg$scale),
    paste0(m_gam_hybrid$rank, "/", length(coef(m_gam_hybrid)))
  ),
  std_error_fmt = "",
  statistic_fmt = "",
  p_value_fmt = ""
)

# Combine all blocks
hybrid_full_table_df <- bind_rows(
  hybrid_param_df,
  hybrid_smooth_df,
  hybrid_stats_df
)


# gt table

hybrid_full_table <- hybrid_full_table_df %>%
  gt(
    groupname_col = "section",
    rowname_col = "term"
  ) %>%
  tab_header(
    title = md("**Full GAM table: pooled hybrid high-trust model**"),
    subtitle = "Binomial logit hybrid GAM with region-specific wave shifts"
  ) %>%
  cols_label(
    estimate_fmt = md("**Estimate / edf**"),
    std_error_fmt = md("**SE / Ref.df**"),
    statistic_fmt = md("**z / Chi-sq**"),
    p_value_fmt = md("**p-value**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  cols_align(
    align = "left",
    columns = stub()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#D9D9D9"),
      cell_text(weight = "bold")
    ),
    locations = cells_row_groups()
  ) %>%
  tab_source_note(
    source_note = "Notes: The model is a binomial logit hybrid GAM where the dependent variable is high trust in the three-category integrated trust variable. Parametric coefficients are reported on the log-odds scale. Smooth terms report effective degrees of freedom (edf), reference degrees of freedom, Chi-square statistics, and p-values. The model includes nonlinear LDI_c smooths for Primary and Secondary education by wave, a linear LDI_c specification for Tertiary, individual controls, macro controls, region-specific wave shifts, and a country random intercept. Terms shown as dashes since they are redundant or not separately identified under the hybrid smooth specification. . p < .10; * p < .05; ** p < .01; *** p < .001."
  ) %>%
  tab_options(
    table.font.size = px(9),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(2)
  )

hybrid_full_table
#hybrid_full_table%>%
#  gtsave("hybrid_full_table.docx")
# ---- Figure 8A. Test on the homogeneity of the data distribution across waves, regime spectrum and educational levels ----
ggplot(wgm_pooled_gam_hybrid, aes(x = LDI_c)) +
  geom_histogram(bins = 40) +
  facet_grid(wave ~ Education) +
  labs(
    x = "Centered Liberal Democracy Index (LDI_c)",
    y = "Number of observations",
    title = "Distribution of LDI_c by wave and education"
  ) +
  theme_minimal()

