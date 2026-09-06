# ---- Population effect model ----
m_18_pop_eff <- polr(
  sti_3cat ~ LDI_c + Education + Gender + Subjective_Income + Age_c,
  data = wgm_18_merged_fin,
  weights = wgt,
  method = "logistic",
  Hess = TRUE
)

vcov_18_pop_eff <- vcovCL(m_18_pop_eff, cluster = ~ cntry)
coeftest(m_18_pop_eff, vcov_18_pop_eff)
#Save result for the table
pop_eff <- coeftest(m_18_pop_eff, vcov_18_pop_eff)


# ---- Main OLR model ----
m_18_polr <- polr(
  sti_3cat ~ Education * LDI_c +
    Age_c + Gender + Subjective_Income + log_gdp_pc_c + log_rnd_c*Education + cce*Education  + gini_c + regions,
  data = wgm_18_merged_fin,
  weights = wgt,
  method = "logistic",
  Hess = TRUE
)

vcov_18_polr_cl <- vcovCL(m_18_polr, cluster = ~ cntry)
m18 <- coeftest(m_18_polr, vcov_18_polr_cl)

# Pull coefficients and vcov
b <- coef(m_18_polr)
V <- vcov_18_polr_cl

# Establish the names for the variance-covariance matrix calculation
dem      <- "LDI_c"
dem_sec  <- "EducationSecondary:LDI_c"
dem_tert <- "EducationTertiary:LDI_c"

# manual function for linear hypothesis calculation
get_effect <- function(L, b, V) {
  est <- sum(L * b[names(L)])
  se  <- sqrt(as.numeric(t(L) %*% V[names(L), names(L)] %*% L))
  z   <- est / se
  p   <- 2 * pnorm(-abs(z))
  OR  <- exp(est)
  
  data.frame(
    estimate = est,
    std_error = se,
    z_value = z,
    p_value = p,
    odds_ratio = OR,
    row.names = NULL
  )
}

# Define contrasts
L_primary <- c()
L_primary[dem] <- 1

L_secondary <- c()
L_secondary[dem] <- 1
L_secondary[dem_sec] <- 1

L_tertiary <- c()
L_tertiary[dem] <- 1
L_tertiary[dem_tert] <- 1

# Compute cohort-specific democracy effects
res_primary_18   <- get_effect(L_primary, b, V)
res_secondary_18 <- get_effect(L_secondary, b, V)
res_tertiary_18  <- get_effect(L_tertiary, b, V)

# Put together
cumulative_res_18 <- rbind(
  cbind(Education = "Primary",   res_primary_18),
  cbind(Education = "Secondary", res_secondary_18),
  cbind(Education = "Tertiary",  res_tertiary_18)
)

cumulative_res_18

# automatic function for linear hypothesise calculation
hyp_18 <- hypotheses(
  m_18_polr,
  hypothesis = c(
    "Primary"   = "`LDI_c` = 0",
    "Secondary" = "`LDI_c` + `EducationSecondary:LDI_c` = 0",
    "Tertiary"  = "`LDI_c` + `EducationTertiary:LDI_c` = 0"
  ),
  vcov = vcov_18_polr_cl
)

hyp_or_18 <- hyp_18 %>%
  transform(
    odds_ratio = exp(estimate)
  )
hyp_or_18

#Multicol check
m_18_lm <- lm(as.numeric(sti_3cat) ~ Education + LDI_c +
                Age_c + Gender + Subjective_Income + log_gdp_pc_c + log_rnd_c + cce + log_imr_c + gini_c + regions, data = wgm_18_merged_fin)
car::vif(m_18_lm)
cor(wgm_18_merged_fin$log_gdp_pc_c,wgm_18_merged_fin$log_imr_c, method = "pearson")
# high lvl of multicollinearity  GVIF for imr > 11

# ---- Robustness tests ----
# Creating division autocracy vs. democracy (RoW)
wgm_18_merged_fin <- wgm_18_merged_fin %>%
  mutate(
    raw_auto_dem = case_when(
      RoW %in% c("Closed autocracy", "Electoral autocracy") ~ "0",
      RoW %in% c("Electoral democracy", "Liberal democracy") ~ "1"
    ),
    raw_auto_dem = factor(raw_auto_dem, levels = c("0", "1"), labels = c("Autocracy", "Democracy"),  ordered = FALSE),
    raw_auto_dem = relevel(raw_auto_dem, ref = "Autocracy")
  )


# Creating division liberal vs. non-liberal (RoW)
wgm_18_merged_fin <- wgm_18_merged_fin %>%
  mutate(
    raw_lib_dem = case_when(
      RoW %in% c("Closed autocracy", "Electoral autocracy", "Electoral democracy") ~ 0,
      RoW %in% c("Liberal democracy") ~ 1,
    ),
    raw_lib_dem = factor(raw_lib_dem, levels = c(0, 1), labels = c("Non-liberal", "Liberal"),  ordered = FALSE),
    raw_lib_dem = relevel(raw_lib_dem, ref = "Non-liberal")
  )

# Threshold sensitivity test

# LDI 05
m_18_alt_05 <- polr(
  sti_3cat ~ Education * ldi_alt_05 +
    Age_c + Gender + Subjective_Income + log_gdp_pc_c + log_rnd_c*Education + cce*Education  + gini_c + regions,
  data = wgm_18_merged_fin,
  weights = wgt,
  method = "logistic",
  Hess = TRUE
)

vcov_18_alt_05_cl <- vcovCL(m_18_alt_05, cluster = ~ cntry)

coef_18_ldi_05 <- coeftest(m_18_alt_05, vcov_18_alt_05_cl)

hyp_18_alt_05 <- hypotheses(
  m_18_alt_05,
  hypothesis = c(
    "Primary"   = "`ldi_alt_051` = 0",
    "Secondary" = "`ldi_alt_051` + `EducationSecondary:ldi_alt_051` = 0",
    "Tertiary"  = "`ldi_alt_051` + `EducationTertiary:ldi_alt_051` = 0"
  ),
  vcov = vcov_18_alt_05_cl
)

hyp_or_18_alt_05 <- hyp_18_alt_05 %>%
  transform(
    odds_ratio = exp(estimate)
  )
hyp_or_18_alt_05

# LDI 06
m_18_alt_06 <- polr(
  sti_3cat ~ Education * ldi_alt_06 +
    Age_c + Gender + Subjective_Income + log_gdp_pc_c + log_rnd_c*Education + cce*Education  + gini_c + regions,
  data = wgm_18_merged_fin,
  weights = wgt,
  method = "logistic",
  Hess = TRUE
)

vcov_18_alt_06_cl <- vcovCL(m_18_alt_06, cluster = ~ cntry)
coef_18_ldi_06 <- coeftest(m_18_alt_06, vcov_18_alt_06_cl)

hyp_18_alt_06 <- hypotheses(
  m_18_alt_06,
  hypothesis = c(
    "Primary"   = "`ldi_alt_061` = 0",
    "Secondary" = "`ldi_alt_061` + `EducationSecondary:ldi_alt_061` = 0",
    "Tertiary"  = "`ldi_alt_061` + `EducationTertiary:ldi_alt_061` = 0"
  ),
  vcov = vcov_18_alt_06_cl
)

hyp_or_18_alt_06 <- hyp_18_alt_06 %>%
  transform(
    odds_ratio = exp(estimate)
  )
hyp_or_18_alt_06

# Autocracy vs. Democracy
m_18_auto_dem <- polr(
  sti_3cat ~ Education * raw_auto_dem +
    Age_c + Gender + Subjective_Income + log_gdp_pc_c + log_rnd_c*Education + cce*Education  + gini_c + regions,
  data = wgm_18_merged_fin,
  weights = wgt,
  method = "logistic",
  Hess = TRUE
)

vcov_18_auto_dem_cl <- vcovCL(m_18_auto_dem, cluster = ~ cntry)
coef_18_auto_dem <- coeftest(m_18_auto_dem, vcov_18_auto_dem_cl)

hyp_18_auto_dem <- hypotheses(
  m_18_auto_dem,
  hypothesis = c(
    "Primary"   = "`raw_auto_demDemocracy` = 0",
    "Secondary" = "`raw_auto_demDemocracy` + `EducationSecondary:raw_auto_demDemocracy` = 0",
    "Tertiary"  = "`raw_auto_demDemocracy` + `EducationTertiary:raw_auto_demDemocracy` = 0"
  ),
  vcov = vcov_18_auto_dem_cl
)

hyp_or_18_auto_dem <- hyp_18_auto_dem %>%
  transform(
    odds_ratio = exp(estimate)
  )
hyp_or_18_auto_dem

# Liberal vs. non-liberal
m_18_lib_dem <- polr(
  sti_3cat ~ Education * raw_lib_dem +
    Age_c + Gender + Subjective_Income + log_gdp_pc_c + log_rnd_c*Education + cce*Education  + gini_c + regions,
  data = wgm_18_merged_fin,
  weights = wgt,
  method = "logistic",
  Hess = TRUE
)

vcov_18_lib_dem_cl <- vcovCL(m_18_lib_dem, cluster = ~ cntry)
coef_18_lib_dem <- coeftest(m_18_lib_dem, vcov_18_lib_dem_cl)

hyp_18_lib_dem <- hypotheses(
  m_18_lib_dem,
  hypothesis = c(
    "Primary"   = "`raw_lib_demLiberal` = 0",
    "Secondary" = "`raw_lib_demLiberal` + `EducationSecondary:raw_lib_demLiberal` = 0",
    "Tertiary"  = "`raw_lib_demLiberal` + `EducationTertiary:raw_lib_demLiberal` = 0"
  ),
  vcov = vcov_18_lib_dem_cl
)

hyp_or_18_lib_dem <- hyp_18_lib_dem %>%
  transform(
    odds_ratio = exp(estimate)
  )
hyp_or_18_lib_dem

# Trimming test

#Preparing sub-samples

# Both tails trimming preparation
get_two_tail_trim_countries <- function(data, pct = 0.05) {
  country_ldi_mean <- data %>%
    group_by(cntry) %>%
    summarise(
      mean_LDI_c = mean(LDI_c, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(mean_LDI_c)
  
  n_trim <- ceiling(pct * nrow(country_ldi_mean))
  
  bottom <- country_ldi_mean %>%
    slice_head(n = n_trim) %>%
    mutate(trim_tail = "Bottom")
  
  top <- country_ldi_mean %>%
    slice_tail(n = n_trim) %>%
    mutate(trim_tail = "Top")
  
  bind_rows(bottom, top)
}

trim_countries_5_each_tail <- get_two_tail_trim_countries(
  wgm_pooled_fin,
  pct = 0.05
)

wgm_trim_5_18_both <- wgm_18_merged_fin %>%
  filter(!cntry %in% trim_countries_5_each_tail$cntry)

# Left tail trimming preparation
country_ldi_mean <- wgm_pooled_fin %>%
  group_by(cntry) %>%
  summarise(
    mean_LDI_c = mean(LDI_c, na.rm = TRUE),
    LDI_2018   = LDI_c[wave == 0][1],
    LDI_2020   = LDI_c[wave == 1][1],
    .groups = "drop"
  ) %>%
  arrange(mean_LDI_c)

n_countries <- nrow(country_ldi_mean)
n_trim_5 <- ceiling(0.05 * n_countries)

drop_countries_5 <- country_ldi_mean %>%
  slice_head(n = n_trim_5) %>%
  pull(cntry)

wgm_trim_5_18 <- wgm_18_merged_fin %>%
  filter(!cntry %in% drop_countries_5)


#  ----- Trimming Robustness check model -----
# Two-tailord
m_18_both_trim <- polr(
  sti_3cat ~ Education * LDI_c +
    Age_c + Gender + Subjective_Income + log_gdp_pc_c + log_rnd_c*Education + cce*Education  + gini_c + regions,
  data = wgm_trim_5_18_both,
  weights = wgt,
  method = "logistic",
  Hess = TRUE
)
vcov_m_18_cl_both_trimmed <- vcovCL(m_18_both_trim, cluster = ~ cntry)
m18_both_tails <- coeftest(m_18_both_trim, vcov_m_18_cl_both_trimmed)

hyp_both_18 <- hypotheses(
  m_18_both_trim,
  hypothesis = c(
    "Primary"   = "`LDI_c` = 0",
    "Secondary" = "`LDI_c` + `EducationSecondary:LDI_c` = 0",
    "Tertiary"  = "`LDI_c` + `EducationTertiary:LDI_c` = 0"
  ),
  vcov = vcov_m_18_cl_both_trimmed
)


hyp_or_both_18 <- hyp_both_18 %>%
  transform(
    odds_ratio = exp(estimate)
  )

# Left tail trimming

m_18_left_trim <- polr(
  sti_3cat ~ Education * LDI_c +
    Age_c + Gender + Subjective_Income + log_gdp_pc_c + log_rnd_c*Education + cce*Education  + gini_c + regions,
  data = wgm_trim_5_18,
  weights = wgt,
  method = "logistic",
  Hess = TRUE
)
vcov_m_18_cl_left_trimmed <- vcovCL(m_18_left_trim, cluster = ~ cntry)
m18_left_tail <- coeftest(m_18_left_trim, vcov_m_18_cl_left_trimmed)

hyp_left_18 <- hypotheses(
  m_18_left_trim,
  hypothesis = c(
    "Primary"   = "`LDI_c` = 0",
    "Secondary" = "`LDI_c` + `EducationSecondary:LDI_c` = 0",
    "Tertiary"  = "`LDI_c` + `EducationTertiary:LDI_c` = 0"
  ),
  vcov = vcov_m_18_cl_left_trimmed
)


hyp_or_left_18 <- hyp_left_18 %>%
  transform(
    odds_ratio = exp(estimate)
  )
