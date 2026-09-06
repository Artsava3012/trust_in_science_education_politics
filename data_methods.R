# ---- Table 4.1 Final analytical sample by wave ----

prep_sample_summary <- function(df, wave_label) {
  
  df %>%
    filter(!is.na(Education), !is.na(cntry)) %>%
    summarise(
      Wave = wave_label,
      Countries = n_distinct(cntry),
      Observations = n(),
      Primary = mean(Education == "Primary", na.rm = TRUE) * 100,
      Secondary = mean(Education == "Secondary", na.rm = TRUE) * 100,
      Tertiary = mean(Education == "Tertiary", na.rm = TRUE) * 100
    )
}

sample_summary_df <- bind_rows(
  prep_sample_summary(wgm_18_merged_fin, "2018"),
  prep_sample_summary(wgm_20_merged_fin, "2020")
) %>%
  mutate(
    Observations = format(Observations, big.mark = ","),
    Primary = sprintf("%.1f", Primary),
    Secondary = sprintf("%.1f", Secondary),
    Tertiary = sprintf("%.1f", Tertiary)
  )

sample_summary_df

sample_summary_table <- sample_summary_df %>%
  gt() %>%
  tab_header(
    title = md("**Analytical sample by wave**"),
    subtitle = "Country coverage, observations, and educational composition"
  ) %>%
  cols_label(
    Wave = md("**Wave**"),
    Countries = md("**Countries**"),
    Observations = md("**Observations**"),
    Primary = md("**Primary (%)**"),
    Secondary = md("**Secondary (%)**"),
    Tertiary = md("**Tertiary (%)**")
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  ) %>%
  tab_source_note(
    source_note = "Notes: Observations are unweighted counts of respondents. Educational composition is reported as the percentage of respondents in each education group in each survey wave"
  ) %>%
  tab_options(
    table.font.size = px(13),
    heading.title.font.size = px(16),
    heading.subtitle.font.size = px(13),
    column_labels.font.weight = "bold",
    data_row.padding = px(5)
  )

sample_summary_table


# ---- Figure 4.1 Mean trust in science and scientists by country in 2018 with political regime classification ----

country_lDI_trust_18 <- wgm_18_merged_fin %>%
  mutate(
    sti_num = case_when(
      as.character(sti_3cat) %in% c("Low", "low", "1") ~ 1,
      as.character(sti_3cat) %in% c("Medium", "medium", "2") ~ 2,
      as.character(sti_3cat) %in% c("High", "high", "3") ~ 3,
      TRUE ~ as.numeric(sti_3cat)
    )
  )

country_lDI_trust_18_summary <- country_lDI_trust_18 %>%
  group_by(cntry, RoW) %>%
  summarise(
    LDI = mean(LDI, na.rm = TRUE),
    mean_trust = weighted.mean(sti_num, w = wgt, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
  

ldi_country_trust_18_plot <- ggplot(
  country_lDI_trust_18_summary,
  aes(x = LDI, y = mean_trust, color = RoW)
) +
  scale_color_viridis_d(option = "D")+
  scale_fill_viridis_d(option = "D")+
  geom_vline(
    xintercept = c(0.5, 0.6),
    linetype = "dashed",
    color = "grey35",
    linewidth = 0.6
  ) +
  annotate(
    "text",
    x = 0.5,
    y = max(country_lDI_trust_18_summary$mean_trust, na.rm = TRUE) -0.1,
    label = "LDI = 0.5",
    angle = 90,
    vjust = -0.4,
    size = 3
  ) +
  annotate(
    "text",
    x = 0.6,
    y = max(country_lDI_trust_18_summary$mean_trust, na.rm = TRUE) - 0.1,
    label = "LDI = 0.6",
    angle = 90,
    vjust = -0.4,
    size = 3
  ) +
  geom_point(
    aes(size = n),
    alpha = 0.75
  ) +
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2)
  ) +
  scale_size_continuous(
    range = c(2, 6),
    labels = comma
  ) +
  labs(
    title = "Country-level trust and liberal democracy, 2018",
    subtitle = "Mean integrated trust in science and scientists by country",
    x = "Liberal Democracy Index",
    y = "Mean integrated trust category",
    color = "Regime type",
    size = "N"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

ldi_country_trust_18_plot