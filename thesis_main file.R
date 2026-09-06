# Loading necessary libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(car)
library(emmeans)
library(psych)
library(countrycode)
library(readxl)
library(tidyverse)
library(ordinal)
library(ggeffects)
library(fixest)
library(VGAM)
library(MASS)
library(sandwich)
library(lmtest)
library(geepack)
library(multgee)
library(marginaleffects)
library(gt)
library(performance)
library(mgcv)
library(splines)
library(gratia)
library(haven)
library(brant)
library(nnet)
library(scales)

# ---- WGM 2018 ----
# Get right names for df
new_names_of_cols <- c(cntry = "WP5", trst_in_sci = "Q12", trst_in_scientist = "Q11C", wave = "YEAR_CALENDAR", wbi = "WBI")

wgm_18 <- read.xlsx("wgm2018-dataset-crosstabs-all-countries.xlsx", sheet = 2,startRow = 1, colNames = TRUE)

# Selecting vars of interest
wgm_18_sel <- wgm_18 %>%
  dplyr::select(WP5, #Country,
         wgt, # National weight,
         YEAR_CALENDAR, #wave
         Q11C, #trust in scientist in this country
         Q12, #trust in science,
         Age,
         AgeCategories,
         Gender,
         Education,
         Household_Income,
         Subjective_Income,
         Regions_Report,
         WBI# Country income level 
         ) %>%
  rename(all_of(new_names_of_cols))
# Translating country code into country name
country_map <- c( "1" = "United States", "2" = "Egypt", "3" = "Morocco", "4" = "Lebanon", "5" = "Saudi Arabia", "6" = "Jordan", "8" = "Turkey", "9" = "Pakistan", "10" = "Indonesia", "11" = "Bangladesh", "12" = "United Kingdom", "13" = "France", "14" = "Germany","15" = "Netherlands","16" = "Belgium", "17" = "Spain", "18" = "Italy", "19" = "Poland", "20" = "Hungary", "21" = "Czech Republic", "22" = "Romania", "23" = "Sweden", "24" = "Greece", "25" = "Denmark",  "26" = "Iran","28" = "Singapore","29" = "Japan","30" = "China","31" = "India","32" = "Venezuela","33" = "Brazil","34" = "Mexico","35" = "Nigeria","36" = "Kenya","37" = "Tanzania","38" = "Israel","39" = "Palestinian Territories","40" = "Ghana","41" = "Uganda","42" = "Benin","43" = "Madagascar","44" = "Malawi","45" = "South Africa","46" = "Canada","47" = "Australia","48" = "Philippines","49" = "Sri Lanka","50" = "Vietnam","51" = "Thailand","52" = "Cambodia","53" = "Laos","54" = "Myanmar","55" = "New Zealand","57" = "Botswana","60" = "Ethiopia","61" = "Mali", "62" = "Mauritania","63" = "Mozambique","64" = "Niger","65" = "Rwanda","66" = "Senegal","67" = "Zambia", "68" = "South Korea", "69" = "Taiwan", "70" = "Afghanistan", "71" = "Belarus","72" = "Georgia","73" = "Kazakhstan", "74" = "Kyrgyzstan","75" = "Moldova","76" = "Russia","77" = "Ukraine","78" = "Burkina Faso","79" = "Cameroon","80" = "Sierra Leone","81" = "Zimbabwe","82" = "Costa Rica","83" = "Albania","84" = "Algeria","87" = "Argentina","88" = "Armenia","89" = "Austria","90" = "Azerbaijan", "96" = "Bolivia", "97" = "Bosnia Herzegovina", "99" = "Bulgaria", "100" = "Burundi", "103" = "Chad", "104" = "Chile", "105" = "Colombia", "106" = "Comoros", "108" = "Congo Brazzaville", "109" = "Croatia", "111" = "Cyprus", "114" = "Dominican Republic", "115" = "Ecuador", "116" = "El Salvador", "119" = "Estonia", "121" = "Finland","122" = "Gabon","124" = "Guatemala","125" = "Guinea","128" = "Haiti","129" = "Honduras","130" = "Iceland","131" = "Iraq","132" = "Ireland","134" = "Ivory Coast","137" = "Kuwait", "138" = "Latvia", "140" = "Liberia",  "141" = "Libya", "143" = "Lithuania", "144" = "Luxembourg", "145" = "North Macedonia", "146" = "Malaysia", "148" = "Malta", "150" = "Mauritius", "153" = "Mongolia", "154" = "Montenegro", "155" = "Namibia", "157" = "Nepal", "158" = "Nicaragua", "160" = "Norway", "163" = "Panama", "164" = "Paraguay", "165" = "Peru", "166" = "Portugal", "173" = "Serbia", "175" = "Slovakia", "176" = "Slovenia",  "183" = "Eswatini", "184" = "Switzerland", "185" = "Tajikistan", "186" = "The Gambia", "187" = "Togo", "190" = "Tunisia", "191" = "Turkmenistan", "193" = "United Arab Emirates",  "194" = "Uruguay",  "195" = "Uzbekistan", "197" = "Yemen", "198" = "Kosovo", "202" = "Northern Cyprus")


# Replace the original variable itself
wgm_18_sel$cntry <- ifelse(
  as.character(wgm_18_sel$cntry) %in% names(country_map),
  country_map[as.character(wgm_18_sel$cntry)],
  as.character(wgm_18_sel$cntry)
)

# Make cntry a factor
wgm_18_sel$cntry <- factor(wgm_18_sel$cntry)

# Add ISO3 to 2018
wgm_18_sel <- wgm_18_sel %>%
  mutate(
    iso3 = countrycode(cntry, origin = "country.name", destination = "iso3c"),
    iso3 = ifelse(cntry == "Kosovo", "XKX", iso3)
  )

# Renaming regions
regions_18 <- c("0"="Not assigned", "1"="Eastern Africa","2"="Central Africa","3"="North Africa","4"="Southern Africa","5"="Western Africa","6"="Central America and Mexico","7"="Northern America","8"="South America","9"="Central Asia","10"="East Asia","11"="Southeast Asia","12"="South Asia","13"="Middle East","14"="Eastern Europe","15"="Northern Europe","16"="Southern Europe","17"="Western Europe","18"="Aus/NZ")

wgm_18_sel$regions <- regions_18[as.character(wgm_18_sel$Regions_Report)]

# Harmonizing wgm 18 regions with wgm 20
wgm_18_sel <- wgm_18_sel %>%
  mutate(
    regions = case_when(
      cntry == "Russia" ~ "Russia/Caucasus/Central Asia",
      cntry %in% c(
        "Albania", "Bosnia Herzegovina", "Croatia", "Estonia",
        "Latvia", "Lithuania", "Montenegro", "North Macedonia",
        "Serbia", "Slovenia", "Kosovo"
      ) ~ "Eastern Europe",
      regions %in% c("Northern Europe", "Southern Europe", "Western Europe") ~ "Western Europe",
      regions == "Eastern Europe" ~ "Eastern Europe",
      regions == "Central Asia" ~ "Russia/Caucasus/Central Asia",
      regions == "Aus/NZ" ~ "Australia/New Zealand",
      regions == "East Asia" ~ "East Asia",
      regions == "Southeast Asia" ~ "Southeast Asia",
      regions == "South Asia" ~ "South Asia",
      regions %in% c("Central America and Mexico", "South America") ~ "Latin America",
      regions == "Northern America" ~ "Northern America",
      regions %in% c("North Africa", "Middle East") ~ "Middle East/North Africa",
      regions %in% c("Eastern Africa", "Central Africa", "Southern Africa", "Western Africa") ~ "Sub-Saharan Africa",
      TRUE ~ regions
    )
  )

# Filtering for the main model
wgm_18_sel <- wgm_18_sel %>%
  mutate(across(
    where(~ is.character(.) || is.factor(.)),
    ~ {
      x <- as.character(.)
      x <- trimws(x)
      x[x == ""] <- NA
      x
    }
  ))

# Recoding focal values so 1 - low trust and 4 - high trust
wgm_18_sel$trst_in_sci <- ifelse(
  wgm_18_sel$trst_in_sci %in% c(98, 99),
  NA,
  5 - wgm_18_sel$trst_in_sci
)

wgm_18_sel$trst_in_scientist <- ifelse(
  wgm_18_sel$trst_in_scientist %in% c(98, 99),
  NA,
  5 - wgm_18_sel$trst_in_scientist
)

# Filtering (Venezuela is excluded since it doesn't include data on Income)
wgm_18_main <- wgm_18_sel %>%
  dplyr::select(cntry, iso3, regions, wgt, wave, trst_in_scientist, trst_in_sci, Age, AgeCategories, Gender, Education, Household_Income,Subjective_Income, wbi) %>%
  filter(!is.na(trst_in_scientist) & !is.na(trst_in_sci) & as.numeric(Age) %in% c(23:99) & !is.na(AgeCategories) & Gender <= 2 & !is.na(Education) & !is.na(Household_Income) & Subjective_Income <= 3 & cntry != "Venezuela")


# Creating integrated variable
wgm_18_main <- wgm_18_main %>%
  mutate(
    sti = trst_in_sci + trst_in_scientist,
    sti = ordered(sti, levels = 2:8)
  )

# Formatiting vars 
wgm_18_main <- wgm_18_main %>%
  mutate(
    trst_in_sci = ordered(trst_in_sci, levels = 1:4),
    trst_in_scientist = ordered(trst_in_scientist, levels = 1:4),
    Education = factor(Education,
      levels = c("1", "2", "3"),
      labels = c("Primary", "Secondary", "Tertiary"),
      ordered = FALSE),
    Gender = factor(Gender),
    regions = factor(regions),
    cntry = factor(cntry),
    Household_Income = ordered(Household_Income),
    Subjective_Income = ordered(Subjective_Income),
    wbi = ordered(wbi)
  )

#---- WGM 2020 ----
wgm_20 <- read.csv("wgm2020.csv")

new_names_of_cols_20 <- c(cntry = "COUNTRYNEW", trst_in_sci = "W6", trst_in_scientist = "W5C", wave = "YEAR_WAVE",AgeCategories = "age_var1", wgt = "WGT")

wgm_20_sel <- wgm_20 %>%
  dplyr::select(COUNTRYNEW, #cntry,
         WGT,
         YEAR_WAVE, #wave,
         W5C, #Trust Scientists in This Country,
         W6, #Trust Science,
         Age,
         age_var1, #AgeCategories,
         Gender,
         Education,
         Household_Income,
         Global11Regions,
         Subjective_Income,
         wbi
         )%>%
  rename(all_of(new_names_of_cols_20))

# Finding cntrys in both survey waves
common_countries <- intersect(unique(wgm_18_sel$cntry), unique(wgm_20_sel$cntry))

# Filtering obs. from countries in both waves 
wgm_18_main <- wgm_18_main %>%
  filter(cntry %in% common_countries)

wgm_20_sel <- wgm_20_sel %>%
  filter(cntry %in% common_countries)


# Add ISO3 to 2020
wgm_20_sel <- wgm_20_sel %>%
  mutate(
    iso3 = countrycode(cntry, origin = "country.name", destination = "iso3c"),
    iso3 = ifelse(cntry == "Kosovo", "XKX", iso3)
  )

# Translating regions from number code to exact names
regions_20 <- c("1" = "Western Europe",
                "2" = "Eastern Europe",
                "3" = "Russia/Caucasus/Central Asia",
                "4" = "Australia/New Zealand",
                "5" = "East Asia",
                "6" = "Southeast Asia",
                "7" = "South Asia",
                "8" = "Latin America",
                "9" = "Northern America",
                "10" = "Middle East/North Africa",
                "11" = "Sub-Saharan Africa"
)
wgm_20_sel$regions <- regions_20[as.character(wgm_20_sel$Global11Regions)]

# Filtering for the main model
wgm_20_sel <- wgm_20_sel %>%
  mutate(across(
    where(~ is.character(.) || is.factor(.)),
    ~ {
      x <- as.character(.)
      x <- trimws(x)
      x[x == ""] <- NA
      x
    }
  ))

# Recoding focal values so 1 - low trust and 4 - high trust
wgm_20_sel$trst_in_sci <- ifelse(
  wgm_20_sel$trst_in_sci == 99,
  NA,
  5 - wgm_20_sel$trst_in_sci
)

wgm_20_sel$trst_in_scientist <- ifelse(
  wgm_20_sel$trst_in_scientist == 99,
  NA,
  5 - wgm_20_sel$trst_in_scientist
)


wgm_20_sel$Subjective_Income <- as.factor(wgm_20_sel$Subjective_Income)

# Recoding Subjective_Income so they match between waves: 2018 uses 3 lvls of it whereas 2020 4. I merged 3 and 4 levels into one
wgm_20_sel$Subjective_Income <- case_when(
  as.character(wgm_20_sel$Subjective_Income) == "1" ~ 1,
  as.character(wgm_20_sel$Subjective_Income) == "2" ~ 2,
  as.character(wgm_20_sel$Subjective_Income) %in% c("3", "4") ~ 3,
  as.character(wgm_20_sel$Subjective_Income) %in% c("5", "6") ~ NA_real_,
  TRUE ~ NA_real_
)

wgm_20_sel$Subjective_Income <- factor(
  wgm_20_sel$Subjective_Income,
  levels = c(1, 2, 3),
  ordered = FALSE
)


# Filtering df
wgm_20_main <- wgm_20_sel %>%
  dplyr::select(cntry, iso3, regions, wgt, wave, trst_in_scientist, trst_in_sci, Age, AgeCategories, Gender, Education, Household_Income,Subjective_Income, wbi) %>%
  filter(!is.na(trst_in_scientist) & !is.na(trst_in_sci) & Age %in% c(23:99) & AgeCategories <= 3 & Gender <= 2 & Education <= 3 & Household_Income <=5 & !is.na(Subjective_Income))



#Creating sti
wgm_20_main <- wgm_20_main %>%
  mutate(
    sti = trst_in_sci + trst_in_scientist,
    sti = ordered(sti, levels = 2:8)
  )
# Ensuring right type for vars.
wgm_20_main <- wgm_20_main %>%
  mutate(
    trst_in_sci = ordered(trst_in_sci, levels = 1:4),
    trst_in_scientist = ordered(trst_in_scientist, levels = 1:4),
    Education = factor(Education, levels = c("1", "2", "3"),
                       labels = c("Primary", "Secondary", "Tertiary"),
                       ordered = FALSE),
    Gender = factor(Gender),
    regions = factor(regions),
    cntry = factor(cntry),
    Household_Income = ordered(Household_Income),
    Subjective_Income = ordered(Subjective_Income),
    wbi = ordered(wbi)
  )

# ---- V-DEM ----
v_dem <- read.csv("V-Dem-CY-Core-v16.csv")

# Selecting Liberal Democracy Index and RoW regime classification
cntry_name <- unique(wgm_18_main$iso3)
v_dem_fil <- v_dem %>%
  filter(country_text_id %in% cntry_name & year %in% c(2013:2020)) %>%
  dplyr::select(country_text_id, year, v2x_libdem, v2x_regime)

v_dem_18 <- v_dem_fil %>%
  filter(year == 2018) %>%
  dplyr::select(country_text_id, v2x_libdem,v2x_regime)

v_dem_20 <- v_dem_fil %>%
  filter(year == 2020) %>%
  dplyr::select(country_text_id, v2x_libdem, v2x_regime)

# Merging
wgm_18_merged <- wgm_18_main %>%
  left_join(
    v_dem_18 %>% rename(LDI = v2x_libdem) %>% rename(RoW = v2x_regime),
    by = c("iso3" = "country_text_id")
  )

wgm_20_merged <- wgm_20_main %>%
  left_join(v_dem_20 %>% rename(LDI = v2x_libdem)%>% rename(RoW = v2x_regime),
            by = c("iso3" = "country_text_id"))

# ----  GDPpc ----
gpd_pc <- read.csv("GDPpc.csv", skip = 3, header = TRUE)
gdp_pc_fil <- gpd_pc %>%
  dplyr::select(Country.Code, X2013, X2014, X2015, X2016, X2017, X2018, X2019, X2020) %>%
  filter(Country.Code %in% cntry_name)  %>%
  rename_with(~str_remove(., "X"), starts_with("X"))

# Adding values for Taiwan separately (source: IMF)
twn_gdp_pc <- read_xls("taiwan_gpd_pc.xls") 
twn_gdp_pc <- twn_gdp_pc %>%
  filter(`GDP per capita, current prices
 (U.S. dollars per capita)` == "Taiwan Province of China") %>%
  mutate(Country.Code = "TWN") %>%
  dplyr::select(Country.Code, "2013","2014","2015","2016","2017","2018","2019","2020")

gdp_pc_full <- bind_rows(gdp_pc_fil, twn_gdp_pc)

gdp_pc_18 <- gdp_pc_full %>%
  dplyr::select(Country.Code, "2018") %>%
  rename(gdp_pc = "2018")

gdp_pc_20 <- gdp_pc_full %>%
  dplyr::select(Country.Code, "2020") %>%
  rename(gdp_pc = "2020")

# Merge
wgm_18_merged <- wgm_18_merged%>%
  left_join(gdp_pc_18, by = c("iso3" = "Country.Code"))

wgm_20_merged <- wgm_20_merged %>%
  left_join(gdp_pc_20, by = c("iso3" = "Country.Code"))



###  ----Control of Corruption -----
cntrl_for_corrup <- read.csv("v_dem_full.csv")
cntr_fil <- cntrl_for_corrup %>%
  dplyr::select(country_name, country_text_id, year, e_wbgi_cce) %>%
  filter(year %in% c(2013:2020) & country_text_id %in% cntry_name)

# Adding specific values to Kosovo as the original df doesn't have them but they're accessible at https://data.worldbank.org/indicator/CC.EST on the interactive map (original WB dataset doesn't include it too)
cntr_fil[217,4] <- -0.65 #2013
cntr_fil[218,4] <- -0.49 #2014
cntr_fil[219,4] <- -0.56 #2015
cntr_fil[220,4] <- -0.44 #2016
cntr_fil[221,4] <- -0.53 #2017
cntr_fil[222,4] <- -0.53 #2018
cntr_fil[223,4] <- -0.54 #2019
cntr_fil[224,4] <- -0.46 #2020



cntr_18 <- cntr_fil %>%
  filter(year == 2018) %>%
  dplyr::select(country_text_id, e_wbgi_cce) %>%
  rename(cce = "e_wbgi_cce")

cntr_20 <- cntr_fil %>%
  filter(year == 2020) %>%
  dplyr::select(country_text_id, e_wbgi_cce) %>%
  rename(cce = "e_wbgi_cce")

# Merging

wgm_18_merged <- wgm_18_merged %>%
  left_join(cntr_18, by = c("iso3" = "country_text_id"))

wgm_20_merged <- wgm_20_merged %>%
  left_join(cntr_20, by = c("iso3" = "country_text_id"))


# ---- R&D expenditure ----

r_d <- read.csv("r_d.csv", skip = 3, header = TRUE)

rd_fil <- r_d %>%
  dplyr::select(-Indicator.Name, -Indicator.Code, -X2021, -X2021,-X2022, -X2023, -X2024, -X2025, -X) %>%
  filter(Country.Code %in% cntry_name)

# Adding Taiwan data based on National Science and Technology Council https://wsts.nstc.gov.tw/stsweb/technology/TechnologyStatistics.aspx?language=E&ID=1 
twn <- data.frame(Country.Name = "Taiwan",Country.Code = "TWN", X2018 = 3.34, X2020 = 3.59)

rd_fil <-bind_rows(rd_fil,twn)

# Function: get nearest previous (or same-year) R&D value
get_rd_nearest_prior <- function(data, target_year) {
  
  # all year columns in the dataset
  year_cols <- paste0("X", 1960:2020)
  
  # keep only years up to the target year
  usable_years <- 1960:target_year
  usable_cols  <- paste0("X", usable_years)
  
  out <- data %>%
    rowwise() %>%
    mutate(
      # values for years up to target year
      rd_values_prior = list(c_across(all_of(usable_cols))),
      
      # all values across entire series
      rd_values_all = list(c_across(all_of(year_cols))),
      
      # index of last non-missing value up to target year
      last_idx_prior = {
        vals <- unlist(rd_values_prior)
        idx <- which(!is.na(vals))
        if (length(idx) == 0) NA_integer_ else max(idx)
      },
      
      # extracted R&D value
      rd_value = {
        vals <- unlist(rd_values_prior)
        idx <- which(!is.na(vals))
        if (length(idx) == 0) NA_real_ else vals[max(idx)]
      },
      
      # year actually used
      rd_year_used = ifelse(
        is.na(last_idx_prior),
        NA_integer_,
        usable_years[last_idx_prior]
      ),
      
      # whether value comes exactly from target year
      rd_exact_year = ifelse(!is.na(rd_year_used) & rd_year_used == target_year, 1, 0),
      
      # whether country has any observation at all in whole dataset
      rd_any_observation = {
        vals <- unlist(rd_values_all)
        any(!is.na(vals))
      },
      
      # whether country has no usable observation up to target year
      rd_no_prior_observation = is.na(rd_value)
    ) %>%
    ungroup() %>%
    dplyr::select(
      Country.Name,
      Country.Code,
      rd_value,
      rd_year_used,
      rd_exact_year,
      rd_no_prior_observation,
      rd_any_observation
    )
  
  return(out)
}

rd_2018_clean <- get_rd_nearest_prior(rd_fil, 2018)
rd_2020_clean <- get_rd_nearest_prior(rd_fil, 2020)

# Adding region and wbi for calculating estimates - 2018
wgm_cntry_18 <- wgm_18_main %>%
  group_by(iso3, regions, wbi) %>%
  summarise()

rd_2018_clean <- rd_2018_clean %>%
  left_join(wgm_cntry_18, by = c("Country.Code" = "iso3"))


# Compute medians at different levels
cell_medians <- rd_2018_clean %>%
  group_by(regions, wbi) %>%
  summarise(
    rd_med_cell = if (all(is.na(rd_value))) NA_real_ else median(rd_value, na.rm = TRUE),
    n_cell_obs = sum(!is.na(rd_value)),
    .groups = "drop"
  )

# Join medians back and create final imputed value
rd_2018_imputed <- rd_2018_clean %>%
  left_join(cell_medians, by = c("regions", "wbi")) %>%
  mutate(
    rd_value_final = case_when(
      !is.na(rd_value) ~ rd_value,
      is.na(rd_value) & !is.na(rd_med_cell) ~ rd_med_cell
    ),
    rd_imputed = ifelse(is.na(rd_value), 1, 0),
    rd_impute_source = case_when(
      !is.na(rd_value) ~ "observed",
      is.na(rd_value) & !is.na(rd_med_cell) ~ "region_wbi_median"
    )
  )
rd_18_fin <- rd_2018_imputed %>%
  dplyr::select(Country.Code, rd_value_final)%>%
  rename(rd = "rd_value_final")

wgm_18_merged <- wgm_18_merged %>%
  left_join(rd_18_fin, by = c("iso3" = "Country.Code"))

# Adding region and wbi for calculating estimates - 2020
wgm_cntry_20 <- wgm_20_main %>%
  group_by(iso3, regions, wbi) %>%
  summarise()

rd_2020_clean <- rd_2020_clean %>%
  left_join(wgm_cntry_20, by = c("Country.Code" = "iso3"))

cell_medians_20 <- rd_2020_clean %>%
  group_by(regions, wbi) %>%
  summarise(
    rd_med_cell = if (all(is.na(rd_value))) NA_real_ else median(rd_value, na.rm = TRUE),
    n_cell_obs = sum(!is.na(rd_value)),
    .groups = "drop"
  )

# Join medians back and create final imputed value
rd_2020_imputed <- rd_2020_clean %>%
  left_join(cell_medians_20, by = c("regions", "wbi")) %>%
  mutate(
    rd_value_final = case_when(
      !is.na(rd_value) ~ rd_value,
      is.na(rd_value) & !is.na(rd_med_cell) ~ rd_med_cell
    ),
    rd_imputed = ifelse(is.na(rd_value), 1, 0),
    rd_impute_source = case_when(
      !is.na(rd_value) ~ "observed",
      is.na(rd_value) & !is.na(rd_med_cell) ~ "region_wbi_median"
    )
  )
rd_20_fin <- rd_2020_imputed %>%
  dplyr::select(Country.Code, rd_value_final)%>%
  rename(rd = "rd_value_final")

wgm_20_merged <- wgm_20_merged %>%
  left_join(rd_20_fin, by = c("iso3" = "Country.Code"))

# ---- Infant mortality----
im <- read.csv("infant_mortality_un.csv", header = TRUE)

im_fil <- im %>%
  dplyr::select(Location, Iso3, Time, Value) %>%
  rename(imr = "Value") %>%
  filter(Iso3 %in% cntry_name) 

im_2018 <- im_fil %>%
  filter(Time == 2018)%>%
  dplyr::select(Iso3, imr)

wgm_18_merged  <- wgm_18_merged %>% 
  left_join(im_2018, by =  c("iso3" = "Iso3"))

im_2020 <- im_fil %>%
  filter(Time == 2020)%>%
  dplyr::select(Iso3, imr)

wgm_20_merged  <- wgm_20_merged %>% 
  left_join(im_2020, by =  c("iso3" = "Iso3"))

# ---- GINI ----
gini <- read.csv("pip.csv")
gini_fil <- gini %>%
  dplyr::select(country_name, country_code, reporting_year, welfare_type, comparable_spell, gini)%>%
  filter(country_code %in% cntry_name)

# Function to check whether target_year is included in comparable_spell
covers_target_year <- function(spell, target_year = 2018) {
  if (is.na(spell) || spell == "") return(FALSE)
  
  # extract all 4-digit years from the string
  yrs <- str_extract_all(spell, "\\d{4}")[[1]]
  yrs <- as.integer(yrs)
  
  if (length(yrs) == 0) {
    return(FALSE)
  } else if (length(yrs) == 1) {
    return(yrs == target_year)
  } else {
    return(target_year >= min(yrs, na.rm = TRUE) & target_year <= max(yrs, na.rm = TRUE))
  }
}

# 2018 
target_year <- 2018
gini_one_per_country_18 <- gini_fil %>%
  mutate(
    spell_covers_2018 = vapply(comparable_spell, covers_target_year, logical(1), target_year = target_year),
    
    selection_reason = case_when(
      welfare_type == "income"      & reporting_year == target_year ~ "exact_2018_income",
      welfare_type == "income"      & spell_covers_2018             ~ "spell_covers_2018_income",
      welfare_type == "income"      & reporting_year < target_year  ~ "closest_past_income",
      welfare_type == "consumption" & reporting_year == target_year ~ "exact_2018_consumption",
      welfare_type == "consumption" & spell_covers_2018             ~ "spell_covers_2018_consumption",
      welfare_type == "consumption" & reporting_year < target_year  ~ "closest_past_consumption",
      TRUE ~ "not_selected"
    ),
    
    selection_priority = case_when(
      selection_reason == "exact_2018_income"              ~ 1L,
      selection_reason == "spell_covers_2018_income"       ~ 2L,
      selection_reason == "closest_past_income"            ~ 3L,
      selection_reason == "exact_2018_consumption"         ~ 4L,
      selection_reason == "spell_covers_2018_consumption"  ~ 5L,
      selection_reason == "closest_past_consumption"       ~ 6L,
      TRUE ~ 99L
    ),
    
    distance_to_2018 = case_when(
      reporting_year == target_year ~ 0,
      spell_covers_2018 ~ 0,
      reporting_year < target_year ~ target_year - reporting_year,
      TRUE ~ Inf
    )
  ) %>%
  filter(
    welfare_type %in% c("income", "consumption"),
    reporting_year <= target_year | spell_covers_2018
  ) %>%
  group_by(country_code, country_name) %>%
  arrange(
    selection_priority,
    distance_to_2018,
    abs(reporting_year - target_year),
    desc(reporting_year),
    .by_group = TRUE
  ) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  dplyr::select(
    country_name,
    country_code,
    gini,
    reporting_year,
    welfare_type,
    comparable_spell,
    selection_reason
  )
gini_18 <- wgm_cntry_18 %>%
  left_join(gini_one_per_country_18, by = c("iso3" = "country_code"))

#Imputation for missing cntrys 2018

Mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

khm_pool <- gini_18 %>%
  filter(!is.na(gini), regions == "Southeast Asia", wbi == 2)

sau_pool <- gini_18 %>%
  filter(!is.na(gini), regions == "Middle East/North Africa", wbi == 4)

nzl_pool <- gini_18 %>%
  filter(!is.na(gini), regions %in% c("Australia/New Zealand", "Western Europe", "Northern America"), wbi == 4)

imputation_tbl <- tibble(
  iso3 = c("KHM", "SAU", "NZL"),
  gini_fill = c(
    median(khm_pool$gini, na.rm = TRUE),
    median(sau_pool$gini, na.rm = TRUE),
    median(nzl_pool$gini, na.rm = TRUE)
  ),
  reporting_year_fill = c(2018L, 2018L, 2018L),
  welfare_type_fill = c(
    Mode(khm_pool$welfare_type),
    Mode(sau_pool$welfare_type),
    Mode(nzl_pool$welfare_type)
  ),
  comparable_spell_fill = c("2018", "2018", "2018"),
  selection_reason_fill = c(
    "imputed_region_wbi_median",
    "imputed_region_wbi_median",
    "imputed_extended_region_wbi_median"
  ),
  imputation_flag = c(
    "imputed_region_wbi",
    "imputed_region_wbi",
    "imputed_extended_region_wbi"
  )
)

gini_18_imp <- gini_18 %>%
  left_join(imputation_tbl, by = "iso3") %>%
  mutate(
    gini = coalesce(gini, gini_fill),
    reporting_year = coalesce(reporting_year, reporting_year_fill),
    welfare_type = coalesce(welfare_type, welfare_type_fill),
    comparable_spell = coalesce(comparable_spell, comparable_spell_fill),
    selection_reason = coalesce(selection_reason, selection_reason_fill)
  ) %>%
  dplyr::select(-gini_fill, -reporting_year_fill, -welfare_type_fill,
         -comparable_spell_fill, -selection_reason_fill)

# Add cntry names
gini_18_imp[56, "country_name"] <- "Cambodia"
gini_18_imp[80, "country_name"] <- "New Zealand"
gini_18_imp[88, "country_name"] <- "Saudi Arabia"

# 2020
target_year <- 2020

gini_one_per_country_20 <- gini_fil %>%
  mutate(
    spell_covers_2020 = vapply(comparable_spell, covers_target_year, logical(1), target_year = target_year),
    
    selection_reason = case_when(
      welfare_type == "income"      & reporting_year == target_year ~ "exact_2020_income",
      welfare_type == "income"      & spell_covers_2020             ~ "spell_covers_2020_income",
      welfare_type == "income"      & reporting_year < target_year  ~ "closest_past_income",
      welfare_type == "consumption" & reporting_year == target_year ~ "exact_2020_consumption",
      welfare_type == "consumption" & spell_covers_2020             ~ "spell_covers_2020_consumption",
      welfare_type == "consumption" & reporting_year < target_year  ~ "closest_past_consumption",
      TRUE ~ "not_selected"
    ),
    
    selection_priority = case_when(
      selection_reason == "exact_2020_income"              ~ 1L,
      selection_reason == "spell_covers_2020_income"       ~ 2L,
      selection_reason == "closest_past_income"            ~ 3L,
      selection_reason == "exact_2020_consumption"         ~ 4L,
      selection_reason == "spell_covers_2020_consumption"  ~ 5L,
      selection_reason == "closest_past_consumption"       ~ 6L,
      TRUE ~ 99L
    ),
    
    distance_to_2020 = case_when(
      reporting_year == target_year ~ 0,
      spell_covers_2020 ~ 0,
      reporting_year < target_year ~ target_year - reporting_year,
      TRUE ~ Inf
    )
  ) %>%
  filter(
    welfare_type %in% c("income", "consumption"),
    reporting_year <= target_year | spell_covers_2020
  ) %>%
  group_by(country_code, country_name) %>%
  arrange(
    selection_priority,
    distance_to_2020,
    abs(reporting_year - target_year),
    desc(reporting_year),
    .by_group = TRUE
  ) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  dplyr::select(
    country_name,
    country_code,
    gini,
    reporting_year,
    welfare_type,
    comparable_spell,
    selection_reason
  )
gini_20 <- wgm_cntry_20 %>%
  left_join(gini_one_per_country_20, by = c("iso3" = "country_code"))
# Adding names
gini_20[56, "country_name"] <- "Cambodia"
gini_20[80, "country_name"] <- "New Zealand"
gini_20[88, "country_name"] <- "Saudi Arabia"

# Imputation for 2020
Mode <- function(x) {
  ux <- na.omit(unique(x))
  ux[which.max(tabulate(match(x, ux)))]
}

# donor pools
khm_pool_20 <- gini_20 %>%
  filter(!is.na(gini), regions == "Southeast Asia", wbi == 2)

sau_pool_20 <- gini_20 %>%
  filter(!is.na(gini), regions == "Middle East/North Africa", wbi == 4)

nzl_pool_20 <- gini_20 %>%
  filter(
    !is.na(gini),
    regions %in% c("Australia/New Zealand", "Western Europe", "Northern America"),
    wbi == 4
  )

# imputation table
imputation_tbl_20 <- tibble(
  iso3 = c("KHM", "SAU", "NZL"),
  gini_fill = c(
    median(khm_pool_20$gini, na.rm = TRUE),
    median(sau_pool_20$gini, na.rm = TRUE),
    median(nzl_pool_20$gini, na.rm = TRUE)
  ),
  reporting_year_fill = c(2020L, 2020L, 2020L),
  welfare_type_fill = c(
    Mode(khm_pool_20$welfare_type),
    Mode(sau_pool_20$welfare_type),
    Mode(nzl_pool_20$welfare_type)
  ),
  comparable_spell_fill = c("2020", "2020", "2020"),
  selection_reason_fill = c(
    "imputed_region_wbi_median",
    "imputed_region_wbi_median",
    "imputed_extended_region_wbi_median"
  ),
  imputation_flag = c(
    "imputed_region_wbi",
    "imputed_region_wbi",
    "imputed_extended_region_wbi"
  )
)

# join + fill
gini_20_imp <- gini_20 %>%
  left_join(imputation_tbl_20, by = "iso3") %>%
  mutate(
    gini = coalesce(gini, gini_fill),
    reporting_year = coalesce(reporting_year, reporting_year_fill),
    welfare_type = coalesce(welfare_type, welfare_type_fill),
    comparable_spell = coalesce(comparable_spell, comparable_spell_fill),
    selection_reason = coalesce(selection_reason, selection_reason_fill)
  ) %>%
  dplyr::select(
    -gini_fill,
    -reporting_year_fill,
    -welfare_type_fill,
    -comparable_spell_fill,
    -selection_reason_fill
  )

# Mergence
# 2018
gini_18_imp <- gini_18_imp %>% ungroup()
gini_18_imp_fin <- gini_18_imp %>%
  dplyr::select(iso3, gini)
wgm_18_merged <- wgm_18_merged %>%
  left_join(gini_18_imp_fin, by = "iso3")

# 2020
gini_20_imp <- gini_20_imp %>% ungroup()
gini_20_imp_fin <- gini_20_imp %>%
  dplyr::select(iso3, gini)
wgm_20_merged <- wgm_20_merged %>%
  left_join(gini_20_imp_fin, by = "iso3")



# --- Prepareation for the analysis ---- 

# WGM 2018
wgm_18_merged_fin <- wgm_18_merged %>%
  mutate(ldi_alt_05 = ifelse(LDI >= 0.5, 1, 0),
         ldi_alt_06 = ifelse(LDI >= 0.6, 1, 0),
         LDI_c = LDI - mean(LDI),
         Age = as.numeric(Age),
         Age_c = Age- mean(Age),
         log_gdp_pc = log(gdp_pc),
         log_gdp_pc_c = log_gdp_pc - mean(log_gdp_pc),
         log_rnd = log(rd),
         log_rnd_c = log_rnd - mean(log_rnd),
         log_imr = log(imr),
         log_imr_c = log_imr - mean(log_imr),
         gini_c = gini - mean(gini)
         )
wgm_18_merged_fin$Education <- factor(
  wgm_18_merged_fin$Education,
  levels = c("Primary", "Secondary", "Tertiary"),
  ordered = FALSE
)
wgm_18_merged_fin$Education <- relevel(wgm_18_merged_fin$Education, ref = "Primary")

wgm_18_merged_fin$Subjective_Income <- factor(
  wgm_18_merged_fin$Subjective_Income,
  levels = c("1", "2", "3"),
  ordered = FALSE
)
wgm_18_merged_fin$ldi_alt_06 <- factor(
  wgm_18_merged_fin$ldi_alt_06,
  levels = c("0", "1"),
  ordered = FALSE
)
wgm_18_merged_fin$ldi_alt_05 <- factor(
  wgm_18_merged_fin$ldi_alt_05,
  levels = c("0", "1"),
  ordered = FALSE
)
# Labeling RoW regimes
wgm_18_merged_fin$RoW <- factor(
  wgm_18_merged_fin$RoW,
  levels = c(0, 1, 2, 3),
  labels = c("Closed autocracy", "Electoral autocracy", "Electoral democracy", "Liberal democracy"),
  ordered = FALSE
)
wgm_18_merged_fin <- wgm_18_merged_fin %>%
  mutate(
    wave = 2018
  )


# WGM 20
wgm_20_merged_fin <- wgm_20_merged %>%
  mutate(ldi_alt_05 = ifelse(LDI >= 0.5, 1, 0),
         ldi_alt_06 = ifelse(LDI >= 0.6, 1, 0),
         LDI_c = LDI - mean(LDI),
         Age = as.numeric(Age),
         Age_c = Age- mean(Age),
         log_gdp_pc = log(gdp_pc),
         log_gdp_pc_c = log_gdp_pc - mean(log_gdp_pc),
         log_rnd = log(rd),
         log_rnd_c = log_rnd - mean(log_rnd),
         log_imr = log(imr),
         log_imr_c = log_imr - mean(log_imr),
         gini_c = gini - mean(gini)
  )

wgm_20_merged_fin$Education <- relevel(wgm_20_merged_fin$Education, ref = "Primary")

wgm_20_merged_fin$Subjective_Income <- factor(
  wgm_20_merged_fin$Subjective_Income,
  levels = c("1", "2", "3"),
  ordered = FALSE
)
wgm_20_merged_fin$ldi_alt_06 <- factor(
  wgm_20_merged_fin$ldi_alt_06,
  levels = c("0", "1"),
  ordered = FALSE
)

wgm_20_merged_fin$ldi_alt_05 <- factor(
  wgm_20_merged_fin$ldi_alt_05,
  levels = c("0", "1"),
  ordered = FALSE
)

wgm_20_merged_fin$RoW <- factor(
  wgm_20_merged_fin$RoW,
  levels = c(0, 1, 2, 3),
  labels = c("Closed autocracy", "Electoral autocracy", "Electoral democracy", "Liberal democracy"),
  ordered = FALSE
)

wgm_20_merged_fin <- wgm_20_merged_fin %>%
  mutate(
    AgeCategories = as.factor(AgeCategories)
  )

# ----Merging 2018 and 2020 wave ----
wgm_pooled_fin <- bind_rows(wgm_18_merged_fin, wgm_20_merged_fin) %>%
  mutate(
    wave = case_when(
      wave == 2018 ~ 0,
      wave == 2020 ~ 1
    )
  )

#str(wgm_pooled_fin)
#table(wgm_pooled_fin$wave, useNA = "ifany")

wgm_pooled_fin <- wgm_pooled_fin %>%
  mutate(
    wave = as.numeric(wave),   # keep 0/1
    Age_c = Age - mean(Age, na.rm = TRUE),
    LDI_c = LDI - mean(LDI, na.rm = TRUE),
    log_gdp_pc_c = log_gdp_pc - mean(log_gdp_pc, na.rm = TRUE),
    log_rnd_c = log_rnd - mean(log_rnd, na.rm = TRUE),
    log_imr_c = log_imr - mean(log_imr, na.rm = TRUE),
    gini_c = gini - mean(gini, na.rm = TRUE),
    sti = ordered(sti, levels = 2:8),
    Education = relevel(Education, ref = "Primary"),
    Subjective_Income = relevel(Subjective_Income, ref = "1"),
    Gender = relevel(Gender, ref = levels(Gender)[1]),
    cntry = factor(cntry),
    regions = factor(regions),
    ldi_alt_06 = relevel(ldi_alt_06, ref = "0"),
    ldi_alt_05 = relevel(ldi_alt_05, ref = "0"),
    RoW = relevel(RoW, ref = "Closed autocracy")
  )

#----- After join preparation ----
wgm_18_merged_fin <- wgm_18_merged_fin %>%
  mutate(
    sti = ordered(sti, levels = 2:8),
    Education = relevel(Education, ref = "Primary"),
    Subjective_Income = relevel(Subjective_Income, ref = "1"),
    Gender = relevel(Gender, ref = levels(Gender)[1]),
    cntry = factor(cntry),
    regions = factor(regions),
    ldi_alt_06 = relevel(ldi_alt_06, ref = "0"),
    ldi_alt_05 = relevel(ldi_alt_05, ref = "0"),
    RoW = relevel(RoW, ref = "Closed autocracy")
  )
# Creating 3 levels of trust
wgm_18_merged_fin <- wgm_18_merged_fin %>%
  mutate(
    sti_3cat = case_when(
      sti %in% c("2", "3", "4") ~ "Low trust",
      sti %in% c("5", "6") ~ "Medium trust",
      sti %in% c("7", "8") ~ "High trust"
    ),
    sti_3cat = ordered(sti_3cat, levels = c("Low trust", "Medium trust", "High trust"))
  )

wgm_20_merged_fin <- wgm_20_merged_fin %>%
  mutate(
    sti = ordered(sti, levels = 2:8),
    Education = relevel(Education, ref = "Primary"),
    Subjective_Income = relevel(Subjective_Income, ref = "1"),
    Gender = relevel(Gender, ref = levels(Gender)[1]),
    cntry = factor(cntry),
    regions = factor(regions),
    ldi_alt_06 = relevel(ldi_alt_06, ref = "0"),
    ldi_alt_05 = relevel(ldi_alt_05, ref = "0"),
    RoW = relevel(RoW, ref = "Closed autocracy")
  )
wgm_20_merged_fin <- wgm_20_merged_fin %>%
  mutate(
    sti_3cat = case_when(
      sti %in% c("2", "3", "4") ~ "Low trust",
      sti %in% c("5", "6") ~ "Medium trust",
      sti %in% c("7", "8") ~ "High trust"
    ),
    sti_3cat = ordered(sti_3cat, levels = c("Low trust", "Medium trust", "High trust"))
  )

wgm_pooled_fin <- wgm_pooled_fin %>%
  mutate(
    sti_3cat = case_when(
      sti %in% c("2", "3", "4") ~ "Low trust",
      sti %in% c("5", "6") ~ "Medium trust",
      sti %in% c("7", "8") ~ "High trust"
    ),
    sti_3cat = ordered(sti_3cat, levels = c("Low trust", "Medium trust", "High trust"))
  )


# ---- Polychroic correlation ----
check_polychoric <- function(data, var1, var2, dataset_name = "dataset") {
  
  df <- data %>%
    dplyr::select(all_of(c(var1, var2))) %>%
    filter(!is.na(.data[[var1]]), !is.na(.data[[var2]]))
  
  cat("Polychoric correlation for:", dataset_name, "\n")
  cat("Variables:", var1, "and", var2, "\n")
  cat("N used:", nrow(df), "\n")
  
  # Show distributions
  cat("Distribution of", var1, ":\n")
  print(table(df[[var1]], useNA = "ifany"))
  
  cat("\nDistribution of", var2, ":\n")
  print(table(df[[var2]], useNA = "ifany"))
  
  cat("\nCross-tabulation:\n")
  print(table(df[[var1]], df[[var2]], useNA = "ifany"))
  
  # Convert to ordered factors if needed
  df <- df %>%
    mutate(
      across(
        all_of(c(var1, var2)),
        ~ ordered(.x)
      )
    )
  
  # Polychoric correlation
  pc <- psych::polychoric(df, correct = 0)
  
  cat("\nPolychoric correlation matrix:\n")
  print(pc$rho)
  
  cat("\nMain correlation:\n")
  print(pc$rho[1, 2])
  
  invisible(pc)
}

pc_18 <- check_polychoric(
  data = wgm_18_merged_fin,
  var1 = "trst_in_scientist",
  var2 = "trst_in_sci",
  dataset_name = "WGM 2018"
)

pc_20 <- check_polychoric(
  data = wgm_20_merged_fin,
  var1 = "trst_in_scientist",
  var2 = "trst_in_sci",
  dataset_name = "WGM 2020"
)

pc_pooled <- check_polychoric(
  data = wgm_pooled_fin,
  var1 = "trst_in_scientist",
  var2 = "trst_in_sci",
  dataset_name = "WGM pooled 2018 + 2020"
)

polychoric_summary <- tibble(
  dataset = c("2018", "2020", "pooled"),
  polychoric_r = c(
    pc_18$rho[1, 2],
    pc_20$rho[1, 2],
    pc_pooled$rho[1, 2]
  )
)

polychoric_summary
