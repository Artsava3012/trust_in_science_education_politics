stars <- function(p) {
  case_when(
    p < 0.001 ~ "***",
    p < 0.01  ~ "**",
    p < 0.05  ~ "*",
    p < 0.10  ~ " . ",
    TRUE      ~ ""
  )
}

# ---- Table 5.1 Regression table with results of testing H1a– population general effect on trust in science and scientists from democratization ----
pop_eff_df <- as.data.frame(pop_eff[,]) %>%
  rownames_to_column("term") %>%
  rename(
    estimate = Estimate,
    std_error = `Std. Error`,
    p_value = `Pr(>|t|)`
  ) %>%
  mutate(
    stars = stars(p_value),
    
    odds_ratio = exp(estimate),
    conf_low = exp(estimate - 1.96 * std_error),
    conf_high = exp(estimate + 1.96 * std_error),
    
    or_ci_fmt = sprintf(
      "%.3f%s [%.3f, %.3f]",
      odds_ratio, stars, conf_low, conf_high
    ),
    
    p_fmt = sprintf("%.3f", p_value)
  )

#  Model fit statistics
n_obs <- nrow(model.frame(m_18_pop_eff))
aic_val <- AIC(m_18_pop_eff)

nagelkerke_r2 <- performance::r2(m_18_pop_eff)$R2_Nagelkerke

model_stats_df <- tibble(
  term = c("N", "AIC", "Nagelkerke R²"),
  estimate_fmt = c(
    format(n_obs, big.mark = ","),
    sprintf("%.1f", aic_val),
    sprintf("%.3f", nagelkerke_r2)
  ),
  se_fmt = "",
  p_fmt = ""
)
#  Combine coefficients and model statistics
pop_table_df <- pop_eff_df %>%
  dplyr::select(term, or_ci_fmt, p_fmt) %>%
  bind_rows(
    model_stats_df %>%
      transmute(
        term,
        or_ci_fmt = estimate_fmt,
        p_fmt = ""
      )
  )

# gt table
pop_baseline_table <- pop_table_df %>%
  gt() %>%
  tab_header(
    title = md("**Population effect from baseline model: 2018**"),
    subtitle = "Average association between LDI and trust before introducing the LDI × Education interaction, DV = trust in science and scientists (3 categories) "
  ) %>%
  cols_label(
    term = md("**Predictor**"),
    or_ci_fmt = md("**Odds ratio [95% CI]**"),
    p_fmt = md("**p-value**")
  ) %>%
  cols_align(
    align = "center",
    columns = c(or_ci_fmt, p_fmt)
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
    locations = cells_body(
      rows = term %in% c("N", "AIC", "McFadden pseudo-R²")
    )
  ) %>%
  tab_source_note(
    source_note = "Notes: Cells report odds ratios with 95% confidence intervals based on country-clustered standard errors. N reports the unweighted number of observations used in the model. Nagelkerke’s R² is reported as a pseudo-R² measure. . p < .10; * p < .05; ** p < .01; *** p < .001."
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(5)
  )

pop_baseline_table

#----Table 5.2 Results from linear hypothesis test for education-specific LDI effects in 2018 ----

n_obs_18 <- nrow(model.frame(m_18_polr))
aic_18 <- AIC(m_18_polr)

nagelkerke_18 <-  performance::r2(m_18_polr)$R2_Nagelkerke

model_stats_18 <- tibble(
  hypothesis = c("N", "AIC", "Nagelkerke R²"),
  or_ci_fmt = c(
    format(n_obs_18, big.mark = ","),
    sprintf("%.1f", aic_18),
    sprintf("%.3f", nagelkerke_18)
  ),
  p_fmt = ""
)
hyp_or_18_table_df <- hyp_or_18 %>%
  transmute(
    hypothesis,
    odds_ratio = odds_ratio,
    conf.low = exp(conf.low),
    conf.high = exp(conf.high),
    p.value = p.value,
    stars = stars(p.value),
    or_ci_fmt = sprintf(
      "%.3f%s [%.3f, %.3f]",
      odds_ratio, stars, conf.low, conf.high
    ),
    p_fmt = sprintf("%.3f", p.value)
  ) %>%
  dplyr::select(hypothesis, or_ci_fmt, p_fmt) %>%
  bind_rows(model_stats_18)

interaction_18_table <- hyp_or_18_table_df %>%
  gt() %>%
  tab_header(
    title = md("**Education-specific LDI effects, 2018**"),
    subtitle = "Ordered-logit model for the three-category trust in science  and scientists"
  ) %>%
  cols_label(
    hypothesis = md("**Education group**"),
    or_ci_fmt = md("**Odds ratio [95% CI]**"),
    p_fmt = md("**p-value**")
  ) %>%
  cols_align(
    align = "center",
    columns = c(or_ci_fmt, p_fmt)
  ) %>%
  cols_align(
    align = "left",
    columns = hypothesis
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = hypothesis %in% c("N", "AIC", "Nagelkerke R²")
    )
  ) %>%
  tab_source_note(
    source_note = "Notes: Cells report education-specific odds ratios for the cumulative association between LDI_c and the three-category science trust index. Confidence intervals are based on country-clustered standard errors. The model controls for Age_c, Gender, Subjective_Income, log_gdp_pc_c, log_rnd_c × Education, cce × Education, gini_c, and regions. N reports the unweighted number of observations used in the model. . p < .10; * p < .05; ** p < .01; *** p < .001."
  ) %>%
  tab_options(
    table.font.size = px(14),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(5)
  )

interaction_18_table

# ---- Table 5.3 Robustness threshold sensitivity test -----

# Helper function
prep_threshold_block_18 <- function(hyp_df, model, model_label) {
  
  n_obs <- nrow(model.frame(model))
  aic_val <- AIC(model)
  nagelkerke <- performance::r2(model)$R2_Nagelkerke
  
  hyp_block <- hyp_df %>%
    transmute(
      hypothesis,
      odds_ratio = odds_ratio,
      conf.low = exp(conf.low),
      conf.high = exp(conf.high),
      p.value = p.value,
      stars = stars(p.value),
      result = sprintf(
        "%.3f%s [%.3f, %.3f]",
        odds_ratio, stars, conf.low, conf.high
      ),
      p_fmt = sprintf("%.3f", p.value)
    ) %>%
    dplyr::select(hypothesis, result, p_fmt)
  
  stats_block <- tibble(
    hypothesis = c("N", "AIC", "Nagelkerke R²"),
    result = c(
      format(n_obs, big.mark = ","),
      sprintf("%.1f", aic_val),
      sprintf("%.3f", nagelkerke)
    ),
    p_fmt = ""
  )
  
  bind_rows(hyp_block, stats_block) %>%
    rename(
      !!paste0(model_label, "_or_ci") := result,
      !!paste0(model_label, "_p") := p_fmt
    )
}

tab_alt_05 <- prep_threshold_block_18(
  hyp_df = hyp_or_18_alt_05,
  model = m_18_alt_05,
  model_label = "alt_05"
)

tab_alt_06 <- prep_threshold_block_18(
  hyp_df = hyp_or_18_alt_06,
  model = m_18_alt_06,
  model_label = "alt_06"
)

tab_auto_dem <- prep_threshold_block_18(
  hyp_df = hyp_or_18_auto_dem,
  model = m_18_auto_dem,
  model_label = "auto_dem"
)

tab_lib_dem <- prep_threshold_block_18(
  hyp_df = hyp_or_18_lib_dem,
  model = m_18_lib_dem,
  model_label = "lib_dem"
)

threshold_sensitivity_18_df <- tab_alt_05 %>%
  full_join(tab_alt_06, by = "hypothesis") %>%
  full_join(tab_auto_dem, by = "hypothesis") %>%
  full_join(tab_lib_dem, by = "hypothesis") %>%
  mutate(
    hypothesis = factor(
      hypothesis,
      levels = c(
        "Primary", "Secondary", "Tertiary",
        "N", "AIC", "Nagelkerke R²"
      )
    )
  ) %>%
  arrange(hypothesis)

threshold_sensitivity_18_df

threshold_sensitivity_18_table <- threshold_sensitivity_18_df %>%
  gt(rowname_col = "hypothesis") %>%
  tab_header(
    title = md("**Threshold sensitivity checks, 2018**"),
    subtitle = "Ordered-logit models for the three-category science trust index"
  ) %>%
  tab_spanner(
    label = md("**LDI threshold: 0.5**"),
    columns = c(alt_05_or_ci, alt_05_p)
  ) %>%
  tab_spanner(
    label = md("**LDI threshold: 0.6**"),
    columns = c(alt_06_or_ci, alt_06_p)
  ) %>%
  tab_spanner(
    label = md("**Autocracy vs democracy**"),
    columns = c(auto_dem_or_ci, auto_dem_p)
  ) %>%
  tab_spanner(
    label = md("**Non-liberal vs liberal democracy**"),
    columns = c(lib_dem_or_ci, lib_dem_p)
  ) %>%
  cols_label(
    alt_05_or_ci = md("**OR [95% CI]**"),
    alt_05_p = md("**p**"),
    alt_06_or_ci = md("**OR [95% CI]**"),
    alt_06_p = md("**p**"),
    auto_dem_or_ci = md("**OR [95% CI]**"),
    auto_dem_p = md("**p**"),
    lib_dem_or_ci = md("**OR [95% CI]**"),
    lib_dem_p = md("**p**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(
      rows = c("N", "AIC", "Nagelkerke R²")
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#D9D9D9"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_spanners()
  ) %>%
  tab_source_note(
    source_note = "Notes: Cells report education-specific odds ratios with 95% confidence intervals based on country-clustered standard errors. The dependent variable is the three-category science trust index. Models control for Age_c, Gender, Subjective_Income, log_gdp_pc_c, log_rnd_c × Education, cce × Education, gini_c, and regions. N reports the unweighted number of observations used in each model. . p < .10; * p < .05; ** p < .01; *** p < .001."
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    data_row.padding = px(5)
  )

threshold_sensitivity_18_table
# ---- Table 5.4 Trimming sensitivity test results ----

prep_trim_block_18 <- function(hyp_df, model, model_label) {
  
  n_obs <- nrow(model.frame(model))
  aic_val <- AIC(model)
  nagelkerke <- performance::r2(model)$R2_Nagelkerke
  
  hyp_block <- hyp_df %>%
    transmute(
      hypothesis,
      odds_ratio = odds_ratio,
      conf.low = exp(conf.low),
      conf.high = exp(conf.high),
      p.value = p.value,
      stars = stars(p.value),
      result = sprintf(
        "%.3f%s [%.3f, %.3f]",
        odds_ratio, stars, conf.low, conf.high
      ),
      p_fmt = sprintf("%.3f", p.value)
    ) %>%
    dplyr::select(hypothesis, result, p_fmt)
  
  stats_block <- tibble(
    hypothesis = c("N", "AIC", "Nagelkerke R²"),
    result = c(
      format(n_obs, big.mark = ","),
      sprintf("%.1f", aic_val),
      sprintf("%.3f", nagelkerke)
    ),
    p_fmt = ""
  )
  
  bind_rows(hyp_block, stats_block) %>%
    rename(
      !!paste0(model_label, "_or_ci") := result,
      !!paste0(model_label, "_p") := p_fmt
    )
}

tab_left_trim_18 <- prep_trim_block_18(
  hyp_df = hyp_or_left_18,
  model = m_18_left_trim,
  model_label = "left_trim"
)

tab_both_trim_18 <- prep_trim_block_18(
  hyp_df = hyp_or_both_18,
  model = m_18_both_trim,
  model_label = "both_trim"
)
trim_sensitivity_18_df <- tab_left_trim_18 %>%
  full_join(tab_both_trim_18, by = "hypothesis") %>%
  mutate(
    hypothesis = factor(
      hypothesis,
      levels = c(
        "Primary", "Secondary", "Tertiary",
        "N", "AIC", "Nagelkerke R²"
      )
    )
  ) %>%
  arrange(hypothesis)


trim_sensitivity_18_table <- trim_sensitivity_18_df %>%
  gt(rowname_col = "hypothesis") %>%
  tab_header(
    title = md("**Trimming sensitivity checks, 2018**"),
    subtitle = "Ordered-logit models for the merged trust in science and scientists variable"
  ) %>%
  tab_spanner(
    label = md("**Left-tail trim**"),
    columns = c(left_trim_or_ci, left_trim_p)
  ) %>%
  tab_spanner(
    label = md("**Both-tail trim**"),
    columns = c(both_trim_or_ci, both_trim_p)
  ) %>%
  cols_label(
    left_trim_or_ci = md("**OR [95% CI]**"),
    left_trim_p = md("**p**"),
    both_trim_or_ci = md("**OR [95% CI]**"),
    both_trim_p = md("**p**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_stub(
      rows = c("N", "AIC", "Nagelkerke R²")
    )
  ) %>%
  tab_style(
    style = list(
      cell_fill(color = "#D9D9D9"),
      cell_text(weight = "bold")
    ),
    locations = cells_column_spanners()
  ) %>%
  tab_source_note(
    source_note = "Notes: Cells report education-specific odds ratios with 95% confidence intervals based on country-clustered standard errors. The dependent variable is a three-category merged measure of trust in science and trust in scientists. The left-tail trim excludes the lowest-LDI countries; the both-tail trim excludes countries at both extremes of the LDI distribution. Models control for Age_c, Gender, Subjective_Income, log_gdp_pc_c, log_rnd_c × Education, cce × Education, gini_c, and regions. N reports the unweighted number of observations used in each model. . p < .10; * p < .05; ** p < .01; *** p < .001."
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    row_group.font.weight = "bold",
    data_row.padding = px(5)
  )

trim_sensitivity_18_table

# ---- Figure 5.1 Visualization of non-model probability of being in high trust group by country and education ----

emp_country <- wgm_18_merged_fin %>%
  mutate(
    high_trust = as.integer(sti_3cat == "High trust"),
    low_trust  = as.integer(sti_3cat == "Low trust"),
    Education = factor(Education, levels = c("Primary", "Secondary", "Tertiary"))
  ) %>%
  group_by(cntry, wave, Education) %>%
  summarise(
    LDI_c = mean(LDI_c, na.rm = TRUE),
    high_prob = mean(high_trust, na.rm = TRUE),
    low_prob = mean(low_trust, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
# Plot
ggplot(emp_country, aes(x = LDI_c, y = high_prob, color = Education)) +
  scale_color_viridis_d(option = "viridis")+
  geom_point(aes(size = n), alpha = 0.35) +
  geom_smooth(se = TRUE, method = "loess", formula = y ~ x, alpha = 0.15) +
  labs(
    x = "LDI (centered)",
    y = "Country-level observed share with High trust",
    size = "N",
    title = "Non-model probs of high trust by country and education",
    subtitle = "LOESS curve for exploring non-linear pattern"
  ) +
  theme_minimal(base_size = 11)

# ---- Figure 5.2 GAM-estimated relationship between LDI and high trust ----
ggplot(grid_18_pred,
       aes(x = LDI_c, y = pred, color = Education, fill = Education)) +
  scale_colour_viridis_d(option = "viridis")+
  scale_fill_viridis_d(option = "D")+
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high),
              alpha = 0.04, color = NA) +
  geom_line(linewidth = 1.1) +
  labs(
    x = "LDI, centered",
    y = "Predicted probability of high trust",
    title = "GAM-estimated relationship between LDI and high trust in 2018",
    subtitle = "Education-specific smooths with linear controls",
    caption = "Nested structure of obs. was handled by model native Random effects"
  ) +
  theme_minimal(base_size = 11)+
  theme(plot.caption = element_text(hjust = 7))
# ---- Figure 5.3 Significant derivative-based decline and increase in education groups slopes ----

gam18_sum <- summary(gam_high_18)

gam18_adj_r2 <- gam18_sum$r.sq
gam18_dev_exp <- gam18_sum$dev.expl
gam18_n <- nobs(gam_high_18)

gam18_aic <- AIC(gam_high_18)

deriv_18_plot_df <- deriv_18 %>%
  mutate(
    slope_type = case_when(
      .upper_ci < 0 ~ "Decreasing",
      .lower_ci > 0 ~ "Increasing",
      TRUE ~ "Not significant"
    ),
    slope_type = factor(
      slope_type,
      levels = c("Decreasing", "Increasing", "Not significant")
    ),
    Education = factor(
      Education,
      levels = c("Primary", "Secondary", "Tertiary")
    )
  )


grid_18_plot_df <- grid_18_pred %>%
  mutate(
    Education = factor(
      Education,
      levels = c("Primary", "Secondary", "Tertiary")
    )
  )


# Create y-position for derivative bars

bar_y_18 <- min(grid_18_plot_df$CI_low, na.rm = TRUE) - 0.04

deriv_18_bar_df <- deriv_18_plot_df %>%
  mutate(
    y_bar = bar_y_18
  )


# pred prob + derivative segment plot

gam18_combined_plot <- ggplot() +
  
  # Confidence band around predicted probabilities
  geom_ribbon(
    data = grid_18_plot_df,
    aes(
      x = LDI_c,
      ymin = CI_low,
      ymax = CI_high
    ),
    alpha = 0.18,
    fill = "grey70"
  ) +
  
  # Predicted probability curve
  geom_line(
    data = grid_18_plot_df,
    aes(
      x = LDI_c,
      y = pred
    ),
    linewidth = 1.2,
    color = "black"
  ) +
  
  # Derivative significance bar
  geom_tile(
    data = deriv_18_bar_df,
    aes(
      x = LDI_c,
      y = y_bar,
      fill = slope_type
    ),
    height = 0.035
  ) +
  
  facet_wrap(~ Education, nrow = 1) +
  
  scale_fill_manual(
    values = c(
      "Decreasing" = "#B73A3F",
      "Increasing" = "#2B7A25",
      "Not significant" = "#BDBDBD"
    ),
    name = "Derivative"
  ) +
  
  coord_cartesian(
    ylim = c(
      bar_y_18 - 0.04,
      max(grid_18_plot_df$CI_high, na.rm = TRUE) + 0.03
    )
  ) +
  
  labs(
    title = "GAM-estimated relationship between LDI and high trust, 2018",
    subtitle = paste0(
      "Based on sample with excluded progressive autocracies"
    ),
    x = "Centered Liberal Democracy Index (LDI_c)",
    y = "Predicted probability of high trust",
    caption = paste0("adjusted R² = ", sprintf("%.3f", gam18_adj_r2),
                     ", deviance explained = ", sprintf("%.1f%%", gam18_dev_exp * 100),
                     ", N = ", format(gam18_n, big.mark = ",")
    )) +
  
  theme_minimal() +
  theme(
    plot.subtitle = element_text(size = 10),
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    panel.grid.minor = element_line(linewidth = 0.25),
    panel.grid.major = element_line(linewidth = 0.45)
  )

gam18_combined_plot

# ----- Table 5.5 Derivative significant increase/decrease for GAM 2018 ----

# Convert derivative classifications into LDI_c segments
deriv_18_segments <- deriv_18_plot_df %>%
  arrange(Education, LDI_c) %>%
  group_by(Education) %>%
  mutate(
    segment_id = cumsum(
      slope_type != lag(slope_type, default = first(slope_type))
    )
  ) %>%
  group_by(Education, segment_id, slope_type) %>%
  summarise(
    LDI_from = min(LDI_c, na.rm = TRUE),
    LDI_to = max(LDI_c, na.rm = TRUE),
    min_derivative = min(.derivative, na.rm = TRUE),
    max_derivative = max(.derivative, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  dplyr::select(
    Education,
    slope_type,
    LDI_from,
    LDI_to,
    min_derivative,
    max_derivative
  )

# Technical model information

gam18_model_info <- tibble(
  Statistic = c(
    "N",
    "Adjusted R²",
    "Deviance explained",
    "AIC",
    "Model family",
    "Outcome",
    "Focal smooth",
    "Controls",
    "Country structure"
  ),
  Value = c(
    format(gam18_n, big.mark = ","),
    sprintf("%.3f", gam18_adj_r2),
    sprintf("%.1f%%", gam18_dev_exp * 100),
    sprintf("%.1f", gam18_aic),
    "Binomial logit GAM/BAM",
    "High trust in merged trust in science and scientists variable",
    "Education-specific smooths of LDI_c",
    "Age_c, Gender, Subjective_Income, log_gdp_pc_c, log_rnd_c, cce, gini_c, regions",
    "Country random intercept via s(cntry, bs = 're')"
  )
)


# derivative segment table


deriv_18_segments_table_df <- deriv_18_segments %>%
  mutate(
    LDI_range = sprintf("%.3f to %.3f", LDI_from, LDI_to),
    derivative_range = sprintf("%.3f to %.3f", min_derivative, max_derivative)
  ) %>%
  dplyr::select(
    Education,
    slope_type,
    LDI_range,
    derivative_range
  )

deriv_18_segments_table <- deriv_18_segments_table_df %>%
  gt(groupname_col = "Education") %>%
  tab_header(
    title = md("**Derivative segments from the 2018 GAM**"),
    subtitle = "Local direction of the LDI association with high trust"
  ) %>%
  cols_label(
    slope_type = md("**Slope direction**"),
    LDI_range = md("**LDI_c range**"),
    derivative_range = md("**Derivative range**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
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
    source_note = paste0(
      "Model information: N = ", format(gam18_n, big.mark = ","),
      "; adjusted R² = ", sprintf("%.3f", gam18_adj_r2),
      "; deviance explained = ", sprintf("%.1f%%", gam18_dev_exp * 100),
      "; AIC = ", sprintf("%.1f", gam18_aic),
      ". The model estimates education-specific smooths of LDI_c and controls for Age_c, Gender, Subjective_Income, log_gdp_pc_c, log_rnd_c x Education, cce x Education, gini_c, and regions, with a country random intercept."
    )
  ) %>%
  tab_source_note(
    source_note = "Notes: A segment is classified as decreasing when the upper 95% confidence bound of the derivative is below zero, and as increasing when the lower 95% confidence bound is above zero. Segments whose CI crosses zero are not significant."
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(5)
  )

deriv_18_segments_table
# ---- Figure 5.4 GAM-estimated relationship between LDI and high trust, 2020 ----
ggplot(grid_20_pred,
       aes(x = LDI_c, y = pred, color = Education, fill = Education)) +
  scale_colour_viridis_d(option = "viridis")+
  scale_fill_viridis_d(option = "D")+
  geom_ribbon(aes(ymin = CI_low, ymax = CI_high),
              alpha = 0.04, color = NA) +
  geom_line(linewidth = 1.1) +
  labs(
    x = "LDI, centered",
    y = "Predicted probability of high trust",
    title = "GAM-estimated relationship between LDI and high trust in 2020",
    subtitle = "Education-specific smooths with linear controls",
    caption = "Nested structure of obs. was handled by model native Random effects"
  ) +
  theme_minimal(base_size = 11)+
  theme(plot.caption = element_text(hjust = 7))
# ---- Table 5.6 Predicted probability for across waves comparison ----

# Basic settings

model_data <- wgm_pooled_gam_hybrid

B <- 2000
set.seed(2459)

# Defining key points
ldi_points <- tibble(
  LDI_label = c("Low LDI", "Mean LDI", "High LDI"),
  LDI_target = c(-0.44, 0, 0.41)
)

wave_vals <- sort(unique(model_data$wave))

region_weights <- model_data %>%
  filter(!is.na(regions), !is.na(wgt)) %>%
  group_by(regions) %>%
  summarise(region_weight = sum(wgt, na.rm = TRUE), .groups = "drop") %>%
  mutate(region_weight = region_weight / sum(region_weight))


# Prediction grid

newdat_hybrid_points <- tidyr::crossing(
  LDI_label = ldi_points$LDI_label,
  Education = levels(model_data$Education),
  wave = wave_vals,
  regions = levels(model_data$regions)
) %>%
  left_join(ldi_points, by = "LDI_label") %>%
  rename(LDI_c = LDI_target) %>%
  mutate(
    Education = factor(Education, levels = levels(model_data$Education)),
    regions = factor(regions, levels = levels(model_data$regions)),
    
    Age_c = 0,
    Gender = factor(levels(model_data$Gender)[1],
                    levels = levels(model_data$Gender)),
    Subjective_Income = factor(levels(model_data$Subjective_Income)[1],
                               levels = levels(model_data$Subjective_Income)),
    log_gdp_pc_c = 0,
    log_rnd_c = 0,
    cce = 0,
    gini_c = 0,
    
    # Needed even though country random effect is excluded from predictions
    cntry = factor(levels(model_data$cntry)[1],
                   levels = levels(model_data$cntry))
  ) %>%
  left_join(region_weights, by = "regions")



# Add hybrid smooth indicators

newdat_hybrid_points <- newdat_hybrid_points %>%
  mutate(
    wave_num = ifelse(as.character(wave) %in% c("2020", "1"), 1, 0),
    
    P18 = as.numeric(Education == "Primary"   & wave_num == 0),
    P20 = as.numeric(Education == "Primary"   & wave_num == 1),
    S18 = as.numeric(Education == "Secondary" & wave_num == 0),
    S20 = as.numeric(Education == "Secondary" & wave_num == 1),
    
    wave_label = ifelse(wave_num == 0, "2018", "2020")
  )

# Simulate predictions from model variance-covariance matrix


# Exclude country random intercept for population-level predictions
X_hybrid <- predict(
  m_gam_hybrid,
  newdata = newdat_hybrid_points,
  type = "lpmatrix",
  exclude = "s(cntry)"
)

beta_hat <- coef(m_gam_hybrid)
V_hat <- vcov(m_gam_hybrid)

beta_sims <- MASS::mvrnorm(
  n = B,
  mu = beta_hat,
  Sigma = V_hat
)

# Point predictions
eta_point <- as.vector(X_hybrid %*% beta_hat)
pred_point <- plogis(eta_point)

# Simulated predictions: 
eta_sims <- X_hybrid %*% t(beta_sims)
pred_sims <- plogis(eta_sims)

newdat_hybrid_points <- newdat_hybrid_points %>%
  mutate(
    row_id = row_number(),
    pred_point = pred_point
  )

# Average over regions and compute 2020–2018 differences

group_info <- newdat_hybrid_points %>%
  group_by(Education, LDI_label, LDI_c, wave_label) %>%
  summarise(
    rows = list(row_id),
    weights = list(region_weight / sum(region_weight, na.rm = TRUE)),
    pred = sum(pred_point * region_weight, na.rm = TRUE) /
      sum(region_weight, na.rm = TRUE),
    .groups = "drop"
  )

# For each Education x LDI x wave group
group_sims <- group_info %>%
  mutate(
    sim_pred = map2(rows, weights, ~ {
      mat <- pred_sims[.x, , drop = FALSE]
      w <- .y
      as.numeric(crossprod(w, mat))
    }),
    CI_low = map_dbl(sim_pred, ~ quantile(.x, 0.025, na.rm = TRUE)),
    CI_high = map_dbl(sim_pred, ~ quantile(.x, 0.975, na.rm = TRUE))
  )

# Split 2018 and 2020 predictions
pred_2018 <- group_sims %>%
  filter(wave_label == "2018") %>%
  dplyr::select(Education, LDI_label, LDI_c, pred_2018 = pred,
                CI_low_2018 = CI_low, CI_high_2018 = CI_high,
                sim_2018 = sim_pred)

pred_2020 <- group_sims %>%
  filter(wave_label == "2020") %>%
  dplyr::select(Education, LDI_label, LDI_c, pred_2020 = pred,
                CI_low_2020 = CI_low, CI_high_2020 = CI_high,
                sim_2020 = sim_pred)

wave_diff_hybrid_points <- pred_2018 %>%
  full_join(
    pred_2020,
    by = c("Education", "LDI_label", "LDI_c")
  ) %>%
  mutate(
    diff = pred_2020 - pred_2018,
    
    sim_diff = map2(sim_2020, sim_2018, ~ .x - .y),
    diff_low = map_dbl(sim_diff, ~ quantile(.x, 0.025, na.rm = TRUE)),
    diff_high = map_dbl(sim_diff, ~ quantile(.x, 0.975, na.rm = TRUE)),
    p_sim = map_dbl(
      sim_diff,
      ~ 2 * min(
        mean(.x > 0, na.rm = TRUE),
        mean(.x < 0, na.rm = TRUE)
      )
    )
  )

wave_diff_hybrid_points


# Format of the table


wave_diff_hybrid_table_df <- wave_diff_hybrid_points %>%
  mutate(
    Education = factor(
      Education,
      levels = c("Primary", "Secondary", "Tertiary")
    ),
    LDI_label = factor(
      LDI_label,
      levels = c("Low LDI", "Mean LDI", "High LDI")
    ),
    
    stars = stars(p_sim),
    
    pred_2018_fmt = sprintf(
      "%.3f [%.3f, %.3f]",
      pred_2018, CI_low_2018, CI_high_2018
    ),
    pred_2020_fmt = sprintf(
      "%.3f [%.3f, %.3f]",
      pred_2020, CI_low_2020, CI_high_2020
    ),
    diff_fmt = sprintf(
      "%.3f%s [%.3f, %.3f]",
      diff, stars, diff_low, diff_high
    ),
    p_fmt = sprintf("%.3f", p_sim)
  ) %>%
  arrange(Education, LDI_label) %>%
  dplyr::select(
    Education,
    LDI_label,
    pred_2018_fmt,
    pred_2020_fmt,
    diff_fmt,
    p_fmt
  )

wave_diff_hybrid_table_df

# gt table


hybrid_wave_difference_table <- wave_diff_hybrid_table_df %>%
  gt(groupname_col = "Education") %>%
  tab_header(
    title = md("**Predicted high trust by wave**"),
    subtitle = "Hybrid pooled GAM with region-specific wave shifts"
  ) %>%
  cols_label(
    LDI_label = md("**LDI position**"),
    pred_2018_fmt = md("**2018 probability [95% CI]**"),
    pred_2020_fmt = md("**2020 probability [95% CI]**"),
    diff_fmt = md("**Difference, 2020–2018 [95% CI]**"),
    p_fmt = md("**p**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
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
    source_note = "Notes: Cells report predicted probabilities of high trust with simulation-based 95% CI  The dependent variable is high trust in the merged trust in science and scientists variable. The hybrid pooled GAM allows nonlinear LDI_c relationships for Primary and Secondary education groups and a linear LDI_c relationship for Tertiary. The model includes region-specific wave shifts through regions × wave and excludes the country random intercept from population-level predictions. Macro controls are log_gdp_pc_c x wave, log_rnd_c x  Education * wave, cce x Educationxwave, gini_c x wave,regions x wave. Micro controls: Age_c + Gender + Subjective_Income. The difference column reports 2020 minus 2018. . p < .10; * p < .05; ** p < .01; *** p < .001."
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(5)
  )

hybrid_wave_difference_table

# ---- Figure 5.5 Comparison of predicted probability of high trust across waves ----
ggplot(
  gam_pred_hybrid,
  aes(x = LDI_c, y = pred, color = wave, fill = wave)
) +
  #scale_colour_viridis_d(option = "D")+
  #scale_fill_viridis_d(option = "D")+
  geom_ribbon(
    aes(ymin = CI_low, ymax = CI_high),
    alpha = 0.04,
    color = NA
  ) +
  geom_line(linewidth = 1.2) +
  facet_wrap(~ Education) +
  labs(
    title = "Predicted probability of high trust",
    subtitle = "Hybrid pooled GAM: nonlinear Primary & Secondary, linear Tertiary",
    x = "Centered Liberal Democracy Index (LDI_c)",
    y = "Predicted probability of high trust",
    color = "Wave",
    fill = "Wave"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )