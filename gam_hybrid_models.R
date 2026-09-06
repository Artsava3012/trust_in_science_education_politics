# ----- GAM -----
wgm_18_gam <- wgm_18_merged_fin%>%
  mutate(
    high_trust = as.integer(sti_3cat == "High trust"),
    low_trust  = as.integer(sti_3cat == "Low trust"),
    Education = factor(Education, levels = c("Primary", "Secondary", "Tertiary"))
  )

wgm_20_gam <- wgm_20_merged_fin %>%
  mutate(
    high_trust = as.integer(sti_3cat == "High trust"),
    low_trust  = as.integer(sti_3cat == "Low trust"),
    Education = factor(Education, levels = c("Primary", "Secondary", "Tertiary"))
  )


gam_high_18 <-bam(
  high_trust ~ 
    Education +
    s(LDI_c, by = Education, k = 5) +
    Age_c + Gender + Subjective_Income +log_gdp_pc_c+ log_rnd_c*Education +
    cce*Education + gini_c +
    regions+
    s(cntry, bs = "re"),
  data = wgm_18_gam,
  weights = wgt,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = FALSE
)
gam_18_reg <- summary(gam_high_18)
gam.check(gam_high_18)

gam_high_20 <- bam(
  high_trust ~ 
    Education +
    s(LDI_c, by = Education, k = 5) +
    Age_c + Gender + Subjective_Income +
    log_gdp_pc_c + log_rnd_c*Education + cce*Education + gini_c +
    s(cntry, bs = "re")+
    regions,
  data = wgm_20_gam,
  weights = wgt,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = FALSE
)
summary(gam_high_20)
gam.check(gam_high_20)

make_gam_grid <- function(data) {
  expand.grid(
    LDI_c = seq(min(data$LDI_c, na.rm = TRUE),
                max(data$LDI_c, na.rm = TRUE),
                length.out = 50),
    Education = levels(data$Education)
  ) %>%
    mutate(
      Age_c = 0,
      Gender = factor(levels(data$Gender)[1], levels = levels(data$Gender)),
      log_gdp_pc_c = 0,
      log_rnd_c = 0,
      cce = 0,
      gini_c = 0,
      regions = factor(levels(data$regions)[1], levels = levels(data$regions)),
      cntry = factor(levels(data$cntry)[1], levels = levels(data$cntry))
    )
}
grid_18 <- make_gam_grid(wgm_18_gam) %>%
  mutate(
    Subjective_Income = factor(
      levels(wgm_18_gam$Subjective_Income)[1],
      levels = levels(wgm_18_gam$Subjective_Income)
    )
  )
grid_20 <- make_gam_grid(wgm_20_gam) %>%
  mutate(
    Subjective_Income = factor(
      levels(wgm_20_gam$Subjective_Income)[1],
      levels = levels(wgm_20_gam$Subjective_Income)
    )
  )
pred_18 <- predict(
  gam_high_18,
  newdata = grid_18,
  type = "link",
  se.fit = TRUE,
  exclude = "s(cntry)"
)

grid_18_pred <- grid_18 %>%
  mutate(
    fit_link = pred_18$fit,
    se_link = pred_18$se.fit,
    pred = plogis(fit_link),
    CI_low = plogis(fit_link - 1.96 * se_link),
    CI_high = plogis(fit_link + 1.96 * se_link),
    wave = "2018"
  )


pred_20 <- predict(
  gam_high_20,
  newdata = grid_20,
  type = "link",
  se.fit = TRUE,
  exclude = "s(cntry)"
)

grid_20_pred <- grid_20 %>%
  mutate(
    fit_link = pred_20$fit,
    se_link = pred_20$se.fit,
    pred = plogis(fit_link),
    CI_low = plogis(fit_link - 1.96 * se_link),
    CI_high = plogis(fit_link + 1.96 * se_link),
    wave = "2020"
  )



# First differences with posterior simulation (link units) to check additionally that tertiary is non-significant (not mentioned in the text)
gam_fd_link <- function(model, grid, wave_label) {
  
  out <- lapply(levels(grid$Education), function(ed) {
    
    nd_low <- grid %>%
      filter(Education == ed) %>%
      slice_min(LDI_c, n = 1)
    
    nd_high <- grid %>%
      filter(Education == ed) %>%
      slice_max(LDI_c, n = 1)
    
    X_low <- predict(
      model,
      newdata = nd_low,
      type = "lpmatrix",
      exclude = "s(cntry)"
    )
    
    X_high <- predict(
      model,
      newdata = nd_high,
      type = "lpmatrix",
      exclude = "s(cntry)"
    )
    
    X_diff <- X_high - X_low
    
    b <- coef(model)
    V <- vcov(model)
    
    est_link <- as.numeric(X_diff %*% b)
    se_link <- sqrt(as.numeric(X_diff %*% V %*% t(X_diff)))
    
    z <- est_link / se_link
    p <- 2 * pnorm(-abs(z))
    
    data.frame(
      wave = wave_label,
      Education = ed,
      estimate_link = est_link,
      std_error_link = se_link,
      z_value = z,
      p_value = p
    )
  })
  
  bind_rows(out)
}
gam_fd_link_18 <- gam_fd_link(gam_high_18, grid_18, "2018")
gam_fd_link_20 <- gam_fd_link(gam_high_20, grid_20, "2020")

gam_fd_link_all <- bind_rows(gam_fd_link_18, gam_fd_link_20)
gam_fd_link_all

# First differences on pred probs 
gam_fd_prob <- function(model, grid, wave_label, B = 1000, seed = 2459) {
  
  set.seed(seed)
  
  b <- coef(model)
  V <- vcov(model, unconditional = TRUE)
  
  beta_sim <- MASS::mvrnorm(
    n = B,
    mu = b,
    Sigma = V
  )
  
  out <- lapply(levels(grid$Education), function(ed) {
    
    nd_low <- grid %>%
      filter(Education == ed) %>%
      slice_min(LDI_c, n = 1)
    
    nd_high <- grid %>%
      filter(Education == ed) %>%
      slice_max(LDI_c, n = 1)
    
    X_low <- predict(
      model,
      newdata = nd_low,
      type = "lpmatrix",
      exclude = "s(cntry)"
    )
    
    X_high <- predict(
      model,
      newdata = nd_high,
      type = "lpmatrix",
      exclude = "s(cntry)"
    )
    
    eta_low <- as.numeric(X_low %*% b)
    eta_high <- as.numeric(X_high %*% b)
    
    p_low <- plogis(eta_low)
    p_high <- plogis(eta_high)
    
    fd_point <- p_high - p_low
    
    eta_low_sim <- as.vector(X_low %*% t(beta_sim))
    eta_high_sim <- as.vector(X_high %*% t(beta_sim))
    
    fd_sim <- plogis(eta_high_sim) - plogis(eta_low_sim)
    
    data.frame(
      wave = wave_label,
      Education = ed,
      estimate = fd_point,
      CI_low = quantile(fd_sim, 0.025, na.rm = TRUE),
      CI_high = quantile(fd_sim, 0.975, na.rm = TRUE),
      p_value = 2 * min(
        mean(fd_sim <= 0, na.rm = TRUE),
        mean(fd_sim >= 0, na.rm = TRUE)
      )
    )
  })
  
  bind_rows(out)
}
gam_fd_18_prob <- gam_fd_prob(
  model = gam_high_18,
  grid = grid_18,
  wave_label = "2018"
)

gam_fd_20_prob <- gam_fd_prob(
  model = gam_high_20,
  grid = grid_20,
  wave_label = "2020"
)

gam_fd_prob_all <- bind_rows(gam_fd_18_prob, gam_fd_20_prob)
gam_fd_prob_all

# Extracting derivatives
deriv_18 <- derivatives(gam_high_18)
deriv_20 <- derivatives(gam_high_20)

print(deriv_18 %>%
  filter(grepl("LDI_c", .smooth)), n = 300)

print(deriv_20 %>%
  filter(grepl("LDI_c", .smooth)), n = 300)


# ------ Hybrid model ---- 
wgm_pooled_gam_hybrid <- wgm_pooled_fin %>%
  mutate(
    wave = factor(wave, levels = c(0, 1), labels = c("2018", "2020")),
    
    P18 = as.numeric(Education == "Primary"   & wave == "2018"),
    P20 = as.numeric(Education == "Primary"   & wave == "2020"),
    S18 = as.numeric(Education == "Secondary" & wave == "2018"),
    S20 = as.numeric(Education == "Secondary" & wave == "2020"),
    high_trust = as.integer(sti_3cat == "High trust"),
    low_trust  = as.integer(sti_3cat == "Low trust"),
    Education = factor(Education, levels = c("Primary", "Secondary", "Tertiary"))
  )

m_gam_hybrid <- bam(
  high_trust ~
    Education * wave +
    LDI_c * Education * wave +
    
    s(LDI_c, by = P18, k = 5) +
    s(LDI_c, by = P20, k = 5) +
    s(LDI_c, by = S18, k = 5) +
    s(LDI_c, by = S20, k = 5) +
    
    Age_c + Gender + Subjective_Income +
    log_gdp_pc_c * wave +
    log_rnd_c * Education * wave +
    cce * Education * wave +
    gini_c * wave +
    regions*wave +
    s(cntry, bs = "re"),
  
  data = wgm_pooled_gam_hybrid,
  weights = wgt,
  family = binomial(link = "logit"),
  method = "fREML",
  discrete = FALSE
)


summary(m_gam_hybrid)
gam.check(m_gam_hybrid)

# Predicted probabilities 
ldi_seq <- seq(
  min(wgm_pooled_gam_hybrid$LDI_c, na.rm = TRUE),
  max(wgm_pooled_gam_hybrid$LDI_c, na.rm = TRUE),
  length.out = 100
)

grid_hybrid <- expand.grid(
  LDI_c = ldi_seq,
  Education = levels(wgm_pooled_gam_hybrid$Education),
  wave = levels(wgm_pooled_gam_hybrid$wave)
) %>%
  mutate(
    P18 = as.numeric(Education == "Primary"   & wave == "2018"),
    P20 = as.numeric(Education == "Primary"   & wave == "2020"),
    S18 = as.numeric(Education == "Secondary" & wave == "2018"),
    S20 = as.numeric(Education == "Secondary" & wave == "2020"),
    
    Age_c = 0,
    Gender = factor(levels(wgm_pooled_gam_hybrid$Gender)[1],
                    levels = levels(wgm_pooled_gam_hybrid$Gender)),
    Subjective_Income = factor(levels(wgm_pooled_gam_hybrid$Subjective_Income)[1],
                               levels = levels(wgm_pooled_gam_hybrid$Subjective_Income)),
    log_gdp_pc_c = 0,
    log_rnd_c = 0,
    cce = 0,
    gini_c = 0,
    regions = factor(levels(wgm_pooled_gam_hybrid$regions)[1],
                     levels = levels(wgm_pooled_gam_hybrid$regions)),
    cntry = factor(levels(wgm_pooled_gam_hybrid$cntry)[1],
                   levels = levels(wgm_pooled_gam_hybrid$cntry))
  )

predict_gam_prob <- function(model, newdata, B = 1000, seed = 2459) {
  
  set.seed(seed)
  
  b <- coef(model)
  V <- vcov(model, unconditional = TRUE)
  
  X <- predict(
    model,
    newdata = newdata,
    type = "lpmatrix",
    exclude = "s(cntry)"
  )
  
  # point estimate on link scale
  eta_hat <- as.vector(X %*% b)
  p_hat <- plogis(eta_hat)
  
  # simulated uncertainty
  beta_sim <- MASS::mvrnorm(
    n = B,
    mu = b,
    Sigma = V
  )
  
  eta_sim <- X %*% t(beta_sim)      # n x B
  p_sim <- plogis(eta_sim)          # n x B
  
  out <- newdata %>%
    mutate(
      pred = p_hat,
      CI_low = apply(p_sim, 1, quantile, probs = 0.025, na.rm = TRUE),
      CI_high = apply(p_sim, 1, quantile, probs = 0.975, na.rm = TRUE)
    )
  
  out
}
gam_pred_hybrid <- predict_gam_prob(
  model = m_gam_hybrid,
  newdata = grid_hybrid,
  B = 1000
)