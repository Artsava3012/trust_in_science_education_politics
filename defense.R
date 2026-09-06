

# Function: predicted probability change between two LDI points
# Most common category helper
mode_value <- function(x) {
  names(sort(table(x), decreasing = TRUE))[1]
}

make_newdata_18 <- function(data, edu, ldi_value) {
  data.frame(
    high_trust = 0,  # placeholder; not used in prediction
    Education = factor(edu, levels = levels(data$Education)),
    LDI_c = ldi_value,
    Age_c = mean(data$Age_c, na.rm = TRUE),
    Gender = factor(mode_value(data$Gender), levels = levels(data$Gender)),
    Subjective_Income = factor(
      mode_value(data$Subjective_Income),
      levels = levels(data$Subjective_Income)
    ),
    log_gdp_pc_c = mean(data$log_gdp_pc_c, na.rm = TRUE),
    log_rnd_c = mean(data$log_rnd_c, na.rm = TRUE),
    cce = mean(data$cce, na.rm = TRUE),
    gini_c = mean(data$gini_c, na.rm = TRUE),
    regions = factor(mode_value(data$regions), levels = levels(data$regions)),
    cntry = factor(levels(data$cntry)[1], levels = levels(data$cntry))
  )
}

change_primary_18 <- get_gam_change_typical(
  model = gam_high_18,
  data = wgm_18_gam,
  edu = "Primary",
  ldi_start = -0.447,
  ldi_end = -0.173
)
get_gam_change_typical <- function(model, data, edu, ldi_start, ldi_end) {
  
  nd_start <- make_newdata_18(data, edu, ldi_start)
  nd_end <- make_newdata_18(data, edu, ldi_end)
  
  nd <- bind_rows(
    nd_start %>% mutate(point = "start"),
    nd_end %>% mutate(point = "end")
  )
  
  # Link-scale fitted values
  fit_link <- as.numeric(
    predict(
      model,
      newdata = nd,
      type = "link",
      exclude = "s(cntry)"
    )
  )
  
  # Prediction matrix
  Xp <- predict(
    model,
    newdata = nd,
    type = "lpmatrix",
    exclude = "s(cntry)"
  )
  
  # Variance-covariance matrix
  V <- vcov(model)
  
  # Endpoint SEs on link scale
  se_link <- sqrt(rowSums((Xp %*% V) * Xp))
  
  # Probability-scale predictions
  pred_prob <- plogis(fit_link)
  
  # Probability-scale CIs for endpoints
  pred_prob_low <- plogis(fit_link - 1.96 * se_link)
  pred_prob_high <- plogis(fit_link + 1.96 * se_link)
  
  # Difference on link scale
  Xdiff <- Xp[2, ] - Xp[1, ]
  
  diff_link <- fit_link[2] - fit_link[1]
  se_diff_link <- sqrt(as.numeric(Xdiff %*% V %*% Xdiff))
  
  ci_low_link <- diff_link - 1.96 * se_diff_link
  ci_high_link <- diff_link + 1.96 * se_diff_link
  
  data.frame(
    Education = edu,
    LDI_start = ldi_start,
    LDI_end = ldi_end,
    
    pred_start = pred_prob[1],
    pred_start_low = pred_prob_low[1],
    pred_start_high = pred_prob_high[1],
    
    pred_end = pred_prob[2],
    pred_end_low = pred_prob_low[2],
    pred_end_high = pred_prob_high[2],
    
    change_prob = pred_prob[2] - pred_prob[1],
    change_pp = 100 * (pred_prob[2] - pred_prob[1]),
    
    diff_link = diff_link,
    se_diff_link = se_diff_link,
    ci_low_link = ci_low_link,
    ci_high_link = ci_high_link,
    
    significant_link_change = ifelse(
      ci_low_link > 0 | ci_high_link < 0,
      "Yes",
      "No"
    )
  )
}
change_secondary_dec_18 <- get_gam_change_typical(
  model = gam_high_18,
  data = wgm_18_gam,
  edu = "Secondary",
  ldi_start = -0.447,
  ldi_end = -0.224
)

change_secondary_inc_18 <- get_gam_change_typical(
  model = gam_high_18,
  data = wgm_18_gam,
  edu = "Secondary",
  ldi_start = 0.076,
  ldi_end = 0.401
)

gam_change_18_table <- bind_rows(
  change_primary_18 %>% mutate(segment = "Decrease: primary"),
  change_secondary_dec_18 %>% mutate(segment = "Decrease: secondary"),
  change_secondary_inc_18 %>% mutate(segment = "Increase: secondary")
) %>%
  dplyr::select(
    segment,
    Education,
    LDI_start,
    LDI_end,
    pred_start,
    pred_start_low,
    pred_start_high,
    pred_end,
    pred_end_low,
    pred_end_high,
    change_pp,
    diff_link,
    se_diff_link,
    ci_low_link,
    ci_high_link,
    significant_link_change
  )

gam_change_18_table

gam_change_18_table_round <- gam_change_18_table %>%
  mutate(
    across(
      c(
        LDI_start, LDI_end,
        pred_start, pred_start_low, pred_start_high,
        pred_end, pred_end_low, pred_end_high,
        change_pp,
        diff_link, se_diff_link, ci_low_link, ci_high_link
      ),
      ~ round(.x, 3)
    )
  )

gam_change_18_table_round



gam_change_18_gt <- gam_change_18_table_round %>%
  dplyr::select(
    segment,
    Education,
    LDI_start,
    LDI_end,
    pred_start,
    pred_start_low,
    pred_start_high,
    pred_end,
    pred_end_low,
    pred_end_high,
    change_pp,
    diff_link,
    se_diff_link,
    ci_low_link,
    ci_high_link
  ) %>%
  mutate(
    pred_start = 100 * pred_start,
    pred_start_low = 100 * pred_start_low,
    pred_start_high = 100 * pred_start_high,
    pred_end = 100 * pred_end,
    pred_end_low = 100 * pred_end_low,
    pred_end_high = 100 * pred_end_high,
    `Pred. start, % [95% CI]` = sprintf(
      "%.1f [%.1f, %.1f]",
      pred_start, pred_start_low, pred_start_high
    ),
    `Pred. end, % [95% CI]` = sprintf(
      "%.1f [%.1f, %.1f]",
      pred_end, pred_end_low, pred_end_high
    )
  ) %>%
  dplyr::select(
    segment,
    Education,
    LDI_start,
    LDI_end,
    `Pred. start, % [95% CI]`,
    `Pred. end, % [95% CI]`,
    change_pp,
    diff_link,
    se_diff_link,
    ci_low_link,
    ci_high_link
  ) %>%
  rename(
    Segment = segment,
    Education = Education,
    `LDI start` = LDI_start,
    `LDI end` = LDI_end,
    `Change, pp.` = change_pp,
    `Diff. logit` = diff_link,
    `SE diff.` = se_diff_link,
    `CI low, logit` = ci_low_link,
    `CI high, logit` = ci_high_link
  ) %>%
  gt() %>%
  tab_header(
    title = "Predicted probability change across GAM segments, 2018"
  ) %>%
  fmt_number(
    columns = c(`LDI start`, `LDI end`),
    decimals = 3
  ) %>%
  fmt_number(
    columns = c(`Change, pp.`),
    decimals = 1
  ) %>%
  fmt_number(
    columns = c(`Diff. logit`, `SE diff.`, `CI low, logit`, `CI high, logit`),
    decimals = 3
  ) %>%
  cols_label(
    `Pred. start, % [95% CI]` = "Pred. start (%) [95% CI]",
    `Pred. end, % [95% CI]` = "Pred. end (%) [95% CI]",
    `Change, pp.` = "Change (pp.)",
    `Diff. logit` = "Diff. logit",
    `SE diff.` = "SE diff.",
    `CI low, logit` = "CI low",
    `CI high, logit` = "CI high"
  ) %>%
  tab_spanner(
    label = "LDI interval",
    columns = c(`LDI start`, `LDI end`)
  ) %>%
  tab_spanner(
    label = "Predicted probability",
    columns = c(`Pred. start, % [95% CI]`, `Pred. end, % [95% CI]`, `Change, pp.`)
  ) %>%
  tab_spanner(
    label = "Difference on logit scale",
    columns = c(`Diff. logit`, `SE diff.`, `CI low, logit`, `CI high, logit`)
  ) %>%
  tab_source_note(
    source_note = "Predicted probabilities exclude the country random intercept and represent average population-level predictions. Endpoint CI are transformed from the link scale to the probability scale. Difference and confidence interval for the segment-level change are reported on the logit scale."
  ) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(15),
    source_notes.font.size = px(9),
    data_row.padding = px(4)
  )

gam_change_18_gt

