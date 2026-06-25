# prep_forecast_dalys_9023.R
#
# Description aim:
#
# This script performs demographic forecasting and scenario analysis
# to model the effects of disease eradication and aging interventions.
# (both projections with eradication, and without)
# ------------------------------------------------------------------------------

rm(list=ls())

library(here)
here::i_am("code/5. prep_forecast_dalys_9023.R")
here::dr_here()

library(ggplot2)
library(ggpubr)
library(readxl)
library(tidyverse)
library(janitor)
library(beepr)

# Call custom functions
source(here("code", "functions_9023.R"))



# Constants ---------------------------------------------------------------

# Define some important groups
income_groups <- c("Global", "High Income", "Upper Middle Income", "Lower Middle Income", "Low Income")


# Import data -------------------------------------------------------------

gbd_df_all <- readRDS(here("output-data", "gbd_global_all.rds")) %>%
  # Clean location names and create income group factor
  mutate(location_name = factor(str_remove(location_name, "World Bank "), ordered = T, levels = income_groups),
         income_group = location_name)


# Demographic and epidemiological datasets
all_population_df <- readRDS(here("output-data", "country_lifetab_data.rds")) %>%
  filter(location_name %in% income_groups) %>%
  rename(location = location_name)

all_fertility_df <- readRDS(here("output-data", "wpp_fertility_rates.rds")) %>%
  filter(location_name %in% income_groups)  %>%
  rename(location = location_name)


# Age-specific mortality and disability 
# only for years 1990 and 2023 -> not complete time series, see note on backfilling in 4.
all_mortality_df <- read_csv(here("output-data", "nob_mortality_medium.csv"))
all_disability_df <- read_csv(here("output-data", "nob_disability_medium.csv"))

# Cause hierarchy and income transition data
cause_tree_df <- readRDS(here("output-data", "gbd_data_cause_hierarchy.rds"))
income_transition_df <- read_csv(here("output-data", "income_transition_shares.csv"))

# Extract unique locations
locations <- unique(all_mortality_df$location)


# ------------------------------------------------------------------------------

# Sense check figures - data visualization

# mortality and disability plots by age will be a step-function, given different ages in the same age band will have the same values

# Visualize age-specific mortality rates (by 100,000 people) by income region
all_mortality_df %>%
  filter(year == 2023) %>%
  filter(age <= 100) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~location) + 
  geom_line(aes(x = age, y = mortality/1e5))

# Visualize age-specific disability rates by income region
all_disability_df %>%
  filter(year == 2023) %>%
  filter(age <= 100) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~location) + 
  geom_line(aes(x = age, y = disability/1e5))

# Visualization setup 
# Starting point
# Define color schemes for consistent plotting
loc_cols <- c("Global" = "black","High Income" = "forestgreen", 
              "Upper Middle Income" = "green", "Lower Middle Income" = "orange",
              "Low Income" = "red")

cluster_cols <- c("Ageing-related" = "firebrick1", "Ageing-related (no COVID)" = "firebrick1",
                  "COVID" = "orange", "Adult (late)" = "blue3", 
                  "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")


# Population by age and region
pop_plt <- ggplot(filter(all_population_df, 
                         location %in% locations & year <= 2023 & year >= 1990)) + theme_bw() +
  geom_line(aes(x = age, y = population, color = location, group = interaction(year, location))) + 
  scale_color_manual(values = loc_cols) + labs(x = "Age", y = "Population", color = "Region")
pop_plt

# Mortality by age and region
mort_plt <- all_mortality_df %>%
  filter(location %in% locations & year == 2023) %>%
  ggplot() + theme_bw() +
  geom_line(aes(x = age, y = mortality/100000, color = location)) + 
  scale_color_manual(values = loc_cols) + labs(x = "Age", y = "Mortality", color = "Region")
mort_plt

# Disability by age and region
disab_plt <- all_disability_df %>%
  filter(location %in% locations & year == 2023) %>%
  ggplot() + theme_bw() +
  geom_line(aes(x = age, y = disability/100000, color = location)) + 
  scale_color_manual(values = loc_cols) + labs(x = "Age", y = "Disability", color = "Region")
disab_plt




# Analysis parameters -----------------------------------------------------

# Choose options
# Set key parameters for demographic projections
start_year <- 2023          # Starting year for projections
end_year <- start_year+125  # End year (projections 125 years into future)
end_age <- 150             # Maximum age in model
no_births <- FALSE         # Include births in projections
fertility_type <- "fertility_med"  # Use medium fertility variant
growth_transitions <- TRUE  # Include economic growth transitions
new_vars <- "none"         # Baseline scenario (no interventions)
loc_name <- "Regions"      # Analyze all income regions


# Data preparation for projections ---------------------------------------------

# Select which region to focus on and which years to use as a starting point
# I can only use 2023 as a starting point


# All analysis and projections at income-group level 

population_df <- all_population_df %>%
  filter(location %in% c("High Income", "Low Income",
                         "Lower Middle Income", "Upper Middle Income"))


all_fertility_df <- readRDS(here("output-data", "wpp_fertility_rates.rds")) %>%
  filter(location_name %in% income_groups) %>%
  rename(location = location_name) %>%
  mutate(
    location = case_when(
      location == "World Bank High Income" ~ "High Income",
      location == "World Bank Upper Middle Income" ~ "Upper Middle Income",
      location == "World Bank Lower Middle Income" ~ "Lower Middle Income",
      location == "World Bank Low Income" ~ "Low Income",
      TRUE ~ location
    )
  )


# UN 
fertility_df <- all_fertility_df

# Create mortality data, with growth transitions and extended age range
# Keep the end_age = 100 here as we will fill the later years in the next bit

# Setting the start year to 2023
all_mortality_df <- all_mortality_df %>%
  mutate(
    location = case_when(
      location == "World Bank High Income" ~ "High Income",
      location == "World Bank Upper Middle Income" ~ "Upper Middle Income",
      location == "World Bank Lower Middle Income" ~ "Lower Middle Income",
      location == "World Bank Low Income" ~ "Low Income",
      TRUE ~ location
    )
  )

mortality_df <- create_mortality_df(
  all_mortality_df, loc_name = loc_name, start_year = 2023, end_year = end_year, 
  end_age = 100, growth_transitions = TRUE, 
  income_transition_df = income_transition_df
)

# Visualize mortality trends by location over time
mortality_df %>%
  filter(location == "High Income") %>%
  #  filter(year >2017, year<2050) %>%
  ggplot() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = (mortality/1e5), group = year, color = year))



# Create disability data, with growth transitions and extended age range
all_disability_df <- all_disability_df %>%
  mutate(
    location = case_when(
      location == "World Bank High Income" ~ "High Income",
      location == "World Bank Upper Middle Income" ~ "Upper Middle Income",
      location == "World Bank Lower Middle Income" ~ "Lower Middle Income",
      location == "World Bank Low Income" ~ "Low Income",
      TRUE ~ location
    )
  )

disability_df <- create_disability_df(all_disability_df, loc_name = loc_name, start_year = start_year, end_year = end_year, 
                                      end_age = 100, growth_transitions = TRUE, 
                                      income_transition_df = income_transition_df)

# Visualize disability trends
disability_df %>%
  ggplot() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = log(disability_infant/1e5), group = year, color = year))


# Now have projected mortality and disability


# Get mortality and disability for different scenarios --------------------

# Extend mortality rates to age 150 using exponential extrapolation
# This uses age 75-100 trends to project beyond age 100

# Play forward mortality based on exponential starting at 75
mortality_df <- mortality_df %>% 
  
  # Create complete age grid up to 150
  full_join(crossing(location = unique(mortality_df$location), year = unique(mortality_df$year), age = 0:150)) %>%
  arrange(location, year, age) %>%
  group_by(location, year) %>%
  
  # Define age range for trend fitting (75-100 years)
  # Only use the trend from 75 onwards
  mutate(age_NAs = case_when(age >= 75 & age <= 100 ~ age)) %>%
  
  # Transform cause-specific mortality rates to log scale for linear fitting
  mutate(log_mortality_infant = case_when(age >= 75 ~ log(mortality_infant))) %>%
  mutate(log_mortality_adult_early = case_when(age >= 75 ~ log(mortality_adult_early))) %>%
  mutate(log_mortality_adult_late = case_when(age >= 75 ~ log(mortality_adult_late))) %>%
  mutate(log_mortality_ageing = case_when(age >= 75 ~ log(mortality_ageing))) %>%
  mutate(log_mortality_ageing_nocov = case_when(age >= 75 ~ log(mortality_ageing_nocov))) %>%
  mutate(log_mortality_covid = case_when(age >= 75 ~ log(mortality_covid))) %>%
  
  # Project forward infant mortality
  mutate(beta1 = cov(log_mortality_infant, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_mortality_infant, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(mortality_infant_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # Project forward adult_early mortality
  mutate(beta1 = cov(log_mortality_adult_early, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_mortality_adult_early, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(mortality_adult_early_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  # Project forward adult_late mortality
  mutate(beta1 = cov(log_mortality_adult_late, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_mortality_adult_late, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(mortality_adult_late_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # Project forward senescent (aging-related) mortality
  mutate(beta1 = cov(log_mortality_ageing, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_mortality_ageing, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(mortality_ageing_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # Project forward senescent mortality (no covid)
  mutate(beta1 = cov(log_mortality_ageing_nocov, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_mortality_ageing_nocov, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(mortality_ageing_nocov_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # Project forward covid mortality
  mutate(beta1 = cov(log_mortality_covid, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_mortality_covid, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(mortality_covid_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # remove log values
  select(-c(log_mortality_infant, log_mortality_adult_early, log_mortality_adult_late, 
            log_mortality_ageing, log_mortality_ageing_nocov, log_mortality_covid, beta1, beta0)) %>%
  
  # Create projected mortality rates combining observed and fitted values
  mutate(mortality_infant_proj       = case_when(age <= 100 ~ mortality_infant, 
                                                 age > 100 ~ mortality_infant_fit)) %>%
  mutate(mortality_adult_early_proj  = case_when(age <= 100 ~ mortality_adult_early, 
                                                 age > 100 ~ mortality_adult_early_fit)) %>%
  mutate(mortality_adult_late_proj   = case_when(age <= 100 ~ mortality_adult_late, 
                                                 age > 100 ~ mortality_adult_late_fit)) %>%
  mutate(mortality_ageing_proj       = case_when(age <= 100 ~ mortality_ageing, 
                                                 age > 100 ~ mortality_ageing_fit)) %>%
  mutate(mortality_ageing_nocov_proj = case_when(age <= 100 ~ mortality_ageing_nocov, 
                                                 age > 100 ~ mortality_ageing_nocov_fit)) %>%
  mutate(mortality_covid_proj        = case_when(age <= 100 ~ mortality_covid, 
                                                 age > 100 ~ mortality_covid_fit)) %>%
  
  # Calculate total projected mortality as sum of all causes
  mutate(mortality_proj = mortality_infant_proj+mortality_adult_early_proj+mortality_adult_late_proj+
           mortality_ageing_proj) %>%
  
  # Reorganize columns for clarity
  relocate(year, age, mortality, mortality_proj,
           mortality_infant, mortality_infant_fit, mortality_infant_proj,
           mortality_adult_early, mortality_adult_early_fit, mortality_adult_early_proj,
           mortality_adult_late, mortality_adult_late_fit, mortality_adult_late_proj,
           mortality_ageing, mortality_ageing_fit, mortality_ageing_proj,
           mortality_ageing_nocov, mortality_ageing_nocov_fit, mortality_ageing_nocov_proj,
           mortality_covid, mortality_covid_fit, mortality_covid_proj,
           .after = location) %>%
  ungroup() 

# Visualize projected vs observed mortality rates
mortality_df %>%
  filter(year >2017, year<2050) %>%
  ggplot() + facet_wrap(~location) + 
  geom_line(aes(x = age, y = (mortality/1e5), group = year, color = year)) + 
  geom_line(aes(x = age, y = (mortality_proj/1e5), group = year, color = year), linetype = "dashed") + 
  coord_cartesian(ylim = c(0,1))

# Quality check: verify that component mortalities sum correctly - test_proj uses projected versions combining ages up to 100 and fitted values above
x <- mortality_df %>%
  mutate(test = mortality_infant + mortality_adult_early + mortality_adult_late + mortality_ageing,
         test_proj = mortality_infant_proj + mortality_adult_early_proj + mortality_adult_late_proj + mortality_ageing_proj) %>%
  relocate(test, test_proj, .after = mortality)

# Check residuals
hist(x$test - x$mortality)      # Should be near zero for observed data --> clusters at 0, other spikes also very small value
hist(x$test_proj - x$mortality_proj)  # Should be near zero for projected data --> appears to be wrong here --> need to go through


resid_obs  <- x$test - x$mortality
resid_proj <- x$test_proj - x$mortality_proj
c(
  obs_nonfinite  = sum(!is.finite(resid_obs)),
  proj_nonfinite = sum(!is.finite(resid_proj)),
  obs_max_abs    = max(abs(resid_obs[is.finite(resid_obs)])),
  proj_max_abs   = max(abs(resid_proj[is.finite(resid_proj)]))
)
hist(resid_proj[is.finite(resid_proj)], breaks = 100,
     main = "Finite residuals: test_proj - mortality_proj",
     xlab = "residual")

# ------------------------------------------------------------------------------


# Disability data extrapolation -------------------------------------------

# Apply identical extrapolation procedure to disability rates
# Play forward disability based on exponential starting at 75
disability_df <- disability_df %>% 
  full_join(crossing(location = unique(disability_df$location), year = unique(disability_df$year), age = 0:150)) %>%
  arrange(location, year, age) %>%
  group_by(location, year) %>%
  # Only use the trend from 75 onwards
  mutate(age_NAs = case_when(age >= 75 & age <= 100 ~ age)) %>%
  
  # Log-transform disability rates for linear extrapolation
  mutate(log_disability_infant = case_when(age >= 75 ~ log(disability_infant))) %>%
  mutate(log_disability_adult_early = case_when(age >= 75 ~ log(disability_adult_early))) %>%
  mutate(log_disability_adult_late = case_when(age >= 75 ~ log(disability_adult_late))) %>%
  mutate(log_disability_ageing = case_when(age >= 75 ~ log(disability_ageing))) %>%
  mutate(log_disability_ageing_nocov = case_when(age >= 75 ~ log(disability_ageing_nocov))) %>%
  mutate(log_disability_covid = case_when(age >= 75 ~ log(disability_covid))) %>%
  
  # Fit and extrapolate each disability category (same method as mortality)
  
  # Project forward infant
  mutate(beta1 = cov(log_disability_infant, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_disability_infant, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(disability_infant_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # Project forward adult_early
  mutate(beta1 = cov(log_disability_adult_early, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_disability_adult_early, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(disability_adult_early_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # Project forward adult_late
  mutate(beta1 = cov(log_disability_adult_late, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_disability_adult_late, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(disability_adult_late_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # Project forward senescent
  mutate(beta1 = cov(log_disability_ageing, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_disability_ageing, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(disability_ageing_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # Project forward senescent without covid
  mutate(beta1 = cov(log_disability_ageing_nocov, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_disability_ageing_nocov, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(disability_ageing_nocov_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # Project forward covid
  mutate(beta1 = cov(log_disability_covid, age_NAs, use = "pairwise.complete.obs")/var(age_NAs, na.rm = T)) %>%
  mutate(beta0 = mean(log_disability_covid, na.rm = T) - beta1*mean(age_NAs, na.rm = T)) %>%
  mutate(disability_covid_fit = case_when(age >= 75 ~ exp(beta0 + beta1*age))) %>% 
  
  # remove log values
  select(-c(log_disability_infant, log_disability_adult_early, log_disability_adult_late, 
            log_disability_ageing, log_disability_ageing_nocov, log_disability_covid, beta1, beta0)) %>%
  
  # Add in the projections
  mutate(disability_infant_proj       = case_when(age <= 100 ~ disability_infant, 
                                                  age > 100 ~ disability_infant_fit)) %>%
  mutate(disability_adult_early_proj  = case_when(age <= 100 ~ disability_adult_early, 
                                                  age > 100 ~ disability_adult_early_fit)) %>%
  mutate(disability_adult_late_proj   = case_when(age <= 100 ~ disability_adult_late, 
                                                  age > 100 ~ disability_adult_late_fit)) %>%
  mutate(disability_ageing_proj       = case_when(age <= 100 ~ disability_ageing, 
                                                  age > 100 ~ disability_ageing_fit)) %>%
  mutate(disability_ageing_nocov_proj = case_when(age <= 100 ~ disability_ageing_nocov, 
                                                  age > 100 ~ disability_ageing_nocov_fit)) %>%
  mutate(disability_covid_proj        = case_when(age <= 100 ~ disability_covid, 
                                                  age > 100 ~ disability_covid_fit)) %>%
  
  # Calculate total projected disability
  mutate(disability_proj = disability_infant_proj+disability_adult_early_proj+disability_adult_late_proj+
           disability_ageing_proj) %>%
  
  # Reorganize columns
  relocate(year, age, disability, disability_proj,
           disability_infant, disability_infant_fit, disability_infant_proj,
           disability_adult_early, disability_adult_early_fit, disability_adult_early_proj,
           disability_adult_late, disability_adult_late_fit, disability_adult_late_proj,
           disability_ageing, disability_ageing_fit, disability_ageing_proj,
           disability_ageing_nocov, disability_ageing_nocov_fit, disability_ageing_nocov_proj,
           disability_covid, disability_covid_fit, disability_covid_proj,
           .after = location)  %>%
  ungroup() 



# Visualization of projections --------------------------------------------

# Plot the mortality projections
# Create comprehensive visualization of mortality projections by disease category
p1 <- mortality_df %>%
  filter(year %in% c(2023) & age < 150) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~location+year, ncol = 2) +
  geom_hline(aes(yintercept = log(1e5)), linetype = "dashed") +
  
  # Observed rates (solid lines)
  geom_line(aes(x = age, log(mortality_ageing_fit), color = "Ageing-related"), linetype = "dashed") + 
  geom_line(aes(x = age, log(mortality_ageing), color = "Ageing-related")) + 
  geom_line(aes(x = age, log(mortality_ageing_nocov_fit), color = "Ageing-related (no COVID)"), linetype = "dashed") + 
  geom_line(aes(x = age, log(mortality_ageing_nocov), color = "Ageing-related (no COVID)")) + 
  geom_line(aes(x = age, log(mortality_infant_fit), color = "Infant"), linetype = "dashed") + 
  geom_line(aes(x = age, log(mortality_infant), color = "Infant")) + 
  
  # Projected rates (dashed lines)
  geom_line(aes(x = age, log(mortality_adult_early_fit), color = "Adult (early)"), linetype = "dashed") + 
  geom_line(aes(x = age, log(mortality_adult_early), color = "Adult (early)")) + 
  geom_line(aes(x = age, log(mortality_adult_late_fit), color = "Adult (late)"), linetype = "dashed") + 
  geom_line(aes(x = age, log(mortality_adult_late), color = "Adult (late)")) +
  scale_color_manual(values = cluster_cols) + 
  labs(x = "Age", y = "log mortality", color = "Category", title = "Mortality")
p1

# Create same visualization for disability projections
p2 <- disability_df %>%
  filter(year  %in% c(2023) & age < 150) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~location+year, ncol = 2) +
  geom_hline(aes(yintercept = log(1e5)), linetype = "dashed") +
  
  # Observed rates (solid lines)
  geom_line(aes(x = age, log(disability_ageing_fit), color = "Ageing-related"), linetype = "dashed") + 
  geom_line(aes(x = age, log(disability_ageing), color = "Ageing-related")) + 
  geom_line(aes(x = age, log(disability_ageing_nocov_fit), color = "Ageing-related (no COVID)"), linetype = "dashed") + 
  geom_line(aes(x = age, log(disability_ageing_nocov), color = "Ageing-related (no COVID)")) + 
  geom_line(aes(x = age, log(disability_infant_fit), color = "Infant"), linetype = "dashed") + 
  geom_line(aes(x = age, log(disability_infant), color = "Infant")) + 
  
  # Projected rates (dashed lines)
  geom_line(aes(x = age, log(disability_adult_early_fit), color = "Adult (early)"), linetype = "dashed") + 
  geom_line(aes(x = age, log(disability_adult_early), color = "Adult (early)")) + 
  geom_line(aes(x = age, log(disability_adult_late_fit), color = "Adult (late)"), linetype = "dashed") + 
  geom_line(aes(x = age, log(disability_adult_late), color = "Adult (late)")) +
  scale_color_manual(values = cluster_cols) + 
  labs(x = "Age", y = "log disability", color = "Category", title = "Disability")
p2



# Create detailed decomposition plot for a single region
mortality_df %>% ungroup() %>%
  filter(location == "Lower Middle Income", year == 2023) %>%
  mutate(Total = mortality_proj) %>%
  select(age, Total, mortality_infant_proj, mortality_adult_early_proj, 
         mortality_adult_late_proj, mortality_ageing_nocov_proj, mortality_covid_proj) %>%
  pivot_longer(cols = -c(age, Total)) %>%
  rbind(disability_df %>% ungroup() %>%
          filter(location == "Lower Middle Income", year == 2023) %>%
          mutate(Total = disability_proj) %>%
          select(age, Total, disability_infant_proj, disability_adult_early_proj,
                 disability_adult_late_proj, disability_ageing_nocov_proj, disability_covid_proj) %>%
          pivot_longer(cols = -c(age, Total))) %>%
  # Add indicator for projected vs observed values
  mutate(proj = case_when(age > 100 ~ 0.5, TRUE ~ 1)) %>%
  mutate(value = case_when(Total > 1e5 ~ NA_real_, TRUE ~ value)) %>%
  group_by(name) %>% fill(value, .direction = "down") %>%
  mutate(value = value/1e5, Total = Total/1e5,
         variable = case_when(str_detect(name, "disability") ~ "Disability", TRUE ~ "Mortality"),
         cluster = case_when(str_detect(name, "infant") ~ "Infant",
                             str_detect(name, "adult_early") ~ "Adult (early)",
                             str_detect(name, "adult_late") ~ "Adult (late)",
                             str_detect(name, "ageing") ~ "Ageing-related", 
                             str_detect(name, "covid") ~ "COVID", 
                             TRUE ~ "All")) %>%
  ggplot(aes(x = age)) + theme_bw() + facet_wrap(~ variable) + 
  geom_line(aes(y = Total)) +
  geom_bar(aes(y = value, fill = cluster, alpha = proj), stat = "identity", position = "stack") +
  scale_fill_manual("Region", values = cluster_cols) + 
  scale_alpha(guide="none", range = c(0.5, 1)) +
  lims(y = c(0,1))



# Compare growth vs no growth scenarios -----------------------------------


# current problem with fertility - use Julian's instead --> change 'fertilty_est' back to fertility_med
fertility_df <- all_jfertility %>%
  rename(location = location_name)

# Compare demographic projections with and without economic growth transitions
# This shows how changing economic conditions affect health outcomes over time

# Scenario 1: No economic growth (static mortality/disability patterns)
forecasts_nogrowth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                     start_year = 2023, end_year = end_year, end_age = end_age, 
                                     no_births = no_births, fertility_type = "fertility_est", loc_name = loc_name,
                                     growth_transitions = FALSE, project_100plus = TRUE) %>% 
  mutate(growth = "No Growth", fertility = "2023")

# Scenario 2: With economic growth transitions
forecasts_growth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                   start_year = 2023, end_year = end_year, end_age = end_age, 
                                   no_births = no_births, fertility_type = "fertility_est", loc_name = loc_name, 
                                   growth_transitions = TRUE, project_100plus = TRUE) %>% 
  mutate(growth = "Growth", fertility = "2023")

# Combine initial scenarios
forecasts_both <- rbind(forecasts_nogrowth, forecasts_growth)

# Scenario 3: No economic growth with medium fertility
forecasts_nogrowth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                     start_year = 2023, end_year = end_year, end_age = end_age, 
                                     no_births = no_births, fertility_type = "fertility_med", loc_name = loc_name,
                                     growth_transitions = FALSE, project_100plus = TRUE) %>% 
  mutate(growth = "No Growth", fertility = "Medium")

# Scenario 4: With economic growth and medium fertility
forecasts_growth <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, new = "none",
                                   start_year = 2023, end_year = end_year, end_age = end_age, 
                                   no_births = no_births, fertility_type = "fertility_med", loc_name = loc_name, 
                                   growth_transitions = TRUE, project_100plus = TRUE) %>% 
  mutate(growth = "Growth", fertility = "Medium")

# Combine all growth/fertility scenarios
forecasts_both <- rbind(forecasts_both, forecasts_nogrowth, forecasts_growth) %>% 
  distinct()

# Visualize population age structure under different growth scenarios
forecasts_both %>%
  filter(location == "Low Income") %>%
  ggplot() + theme_bw() + 
  facet_wrap(vars(growth, fertility), scales = "free") + 
  geom_line(aes(x = age, y = population, color = year, group = year)) + 
  labs(x = "Age", y = "Population", color = "Year", title = "Low Income")

# Visualize mortality patterns under growth scenarios
forecasts_both %>%
  filter(location == "Low Income") %>%
  ggplot() + theme_bw() + 
  facet_wrap(vars(growth, fertility), scales = "free") + 
  geom_line(aes(x = age, y = mortality, color = year, group = year)) + 
  labs(x = "Age", y = "Population", color = "Year", title = "Low Income")




# Disease eradication impact visualization -------------------------------

# Visualize the impact of different levels of disease eradication
mortality_df %>%
  filter(location == "Lower Middle Income", year == 2023) %>%
  mutate(Total = mortality_proj) %>%
  select(age, Total, mortality_infant_proj, mortality_adult_early_proj, 
         mortality_adult_late_proj, mortality_ageing_proj, mortality_ageing_nocov_proj) %>%
  pivot_longer(cols = -c(age, Total)) %>%
  rbind(disability_df %>% 
          filter(location == "Lower Middle Income", year == 2023) %>%
          mutate(Total = disability_proj) %>%
          select(age, Total, disability_infant_proj, disability_adult_early_proj, 
                 disability_adult_late_proj, disability_ageing_proj, disability_ageing_nocov_proj) %>%
          pivot_longer(cols = -c(age, Total))) %>%
  mutate(value = value/1e5, Total = Total/1e5,
         variable = case_when(str_detect(name, "disability") ~ "Disability", TRUE ~ "Mortality"),
         cluster = case_when(str_detect(name, "infant") ~ "Infant",
                             str_detect(name, "adult_early") ~ "Adult (early)",
                             str_detect(name, "adult_late") ~ "Adult (late)",
                             str_detect(name, "nocov") ~ "Ageing-related (no COVID)",
                             str_detect(name, "ageing") ~ "Ageing-related", TRUE ~ "All")) %>%
  # Create multiple eradication scenarios
  mutate(`10% reduction` = Total - 0.1*value, 
         `20% reduction` = Total - 0.2*value, 
         `50% reduction` = Total - 0.5*value, 
         `Eradication` = Total - value) %>%
  pivot_longer(cols = c(`10% reduction`, `20% reduction`, `50% reduction`, `Eradication`), 
               names_to = "scenario", values_to = "value_new") %>%
  ggplot(aes(x = age)) + theme_bw() + 
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  facet_wrap(~variable+cluster, nrow = 2, scales = "free_y") + 
  geom_hline(aes(yintercept = log(1)), linetype = "dashed") +
  geom_line(aes(y = log(pmin(Total, 1))), color = "black") +
  geom_line(aes(y = log(pmin(value_new,1)), color = scenario), linetype = "dashed") +
  scale_color_manual("Scenario", values = c("red4", "red3", "red2", "red1")) + 
  labs(x = "Age", y = "log scale")


# Create derivative plots showing marginal effects of disease eradication
## These eradications pin down dmu/derr and dh/derr. 
mortality_df %>%
  filter(year == 2023) %>%
  mutate(Total = mortality_proj) %>%
  select(location, age, Total, mortality_infant_proj, mortality_adult_early_proj, 
         mortality_adult_late_proj, mortality_ageing_proj, mortality_ageing_nocov_proj) %>%
  pivot_longer(cols = -c(location, age, Total)) %>%
  rbind(disability_df %>% 
          filter(year == 2023) %>%
          mutate(Total = disability_proj) %>%
          select(location, age, Total, disability_infant_proj, disability_adult_early_proj, 
                 disability_adult_late_proj, disability_ageing_proj, disability_ageing_nocov_proj) %>%
          pivot_longer(cols = -c(location, age, Total))) %>%
  mutate(value = value/1e5, Total = Total/1e5,
         variable = case_when(str_detect(name, "disability") ~ "Health", TRUE ~ "Mortality"),
         cluster = case_when(str_detect(name, "infant") ~ "Infant",
                             str_detect(name, "adult_early") ~ "Adult (early)",
                             str_detect(name, "adult_late") ~ "Adult (late)",
                             str_detect(name, "nocov") ~ "Ageing-related (no COVID)",
                             str_detect(name, "ageing") ~ "Ageing-related", TRUE ~ "All"),
         cluster = factor(cluster, ordered = T, levels = c("Infant", "Adult (early)",  "Adult (late)", "Ageing-related", "Ageing-related (no COVID)"))) %>%
  mutate(value = case_when(Total > 1.01 ~ NA_real_, TRUE ~ value)) %>%
  filter(location %in% c("High Income", "Low Income")) %>%
  ggplot(aes(x = age)) + theme_bw() + 
  facet_wrap(~location+cluster, nrow = 2, scales = "free_y") + 
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  geom_line(aes(y = value, color = variable)) + 
  labs(x = "Age", y = "Derivative wrt eradication", color = "Location")


# ------------------------------------------------------------------------------


# Specific disease eradication examples ----------------------------------

# Example 1: Complete eradication of aging-related diseases
mortality_df <- mortality_df %>%
  mutate(mortality_proj_new = mortality_proj - mortality_ageing_proj)
disability_df <- disability_df %>%
  mutate(disability_proj_new = disability_proj - disability_ageing_proj)

# Example 2: Complete eradication of aging-related diseases (excluding COVID)
mortality_df <- mortality_df %>%
  mutate(mortality_proj_new = mortality_proj - mortality_ageing_nocov_proj)
disability_df <- disability_df %>%
  mutate(disability_proj_new = disability_proj - disability_ageing_nocov_proj)

# Example 3: Complete eradication of infant diseases
mortality_df <- mortality_df %>%
  mutate(mortality_proj_new = mortality_proj - mortality_infant_proj)
disability_df <- disability_df %>%
  mutate(disability_proj_new = disability_proj - disability_infant_proj)



# Comprehensive scenario comparison ---------------------------------------

# Run comprehensive comparison of intervention scenarios with economic growth
dalys_compare_growth <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, loc_name = loc_name, 
                                          start_year = 2023, end_year = end_year, no_births = FALSE, 
                                          fertility_type = fertility_type, growth_transitions = TRUE, 
                                          project_100plus = TRUE)

# Run same comparison without economic growth
dalys_compare_nogrowth <- compare_forecasts(population_df, fertility_df, mortality_df, disability_df, loc_name = loc_name, 
                                            start_year = 2023, end_year = end_year, no_births = FALSE, 
                                            fertility_type = fertility_type, growth_transitions = FALSE, 
                                            project_100plus = TRUE)



# Dervative analysis: population sensitivity to mortality changes ---------

# dN_dmu
# Calculate how population size responds to small changes in age-specific mortality
# This provides demographic derivatives dN/dμ for each age

# Prepare data for derivative calculations
# Convert to dataframes as tibbles don't cooperate...
mortality_data <- mortality_df %>%
  select(location, year, age_name, age, mortality, mortality_proj, 
         mortality_infant, mortality_infant_proj, mortality_adult_early, mortality_adult_early_proj,
         mortality_adult_late_proj, mortality_adult_late_proj, mortality_ageing, mortality_ageing_proj,
         mortality_ageing_nocov, mortality_ageing_nocov_proj, mortality_covid, mortality_covid_proj) %>%
  as.data.frame()

disability_data <- disability_df %>%
  select(location, year, age_name, age, disability, disability_proj,
         disability_infant, disability_infant_proj, disability_adult_early, disability_adult_early_proj,
         disability_adult_late, disability_adult_late_proj, disability_ageing, disability_ageing_proj,
         disability_ageing_nocov, disability_ageing_nocov_proj, disability_covid, disability_covid_proj) %>%
  as.data.frame()

# Run baseline forecast for comparison
# Define new mortality and disabilty fns
forecasts_base <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "none",
                                 start_year = 2023, end_year = 2146, end_age = end_age, 
                                 no_births = FALSE, fertility_type = "fertility_med", loc_name = "Regions", 
                                 growth_transitions = TRUE, project_100plus = TRUE)

# Calculate baseline population and life expectancy
pops_base <- forecasts_base %>%
  group_by(location, year) %>%
  mutate(survival = cumprod(1 - pmin(mortality, 1))) %>%
  summarise(pop = sum(population), LE_birth = sum(survival))

# Calculate population derivatives with respect to mortality at each age
dN_dmu_results <- tibble()
for (aa in 0:150){
  print(str_c("Computing dN_Dmu for age ", aa))
  
  # Create small perturbation in mortality at age aa
  mortality_data$mortality_proj_new <- mortality_data$mortality_proj
  mortality_data$mortality_proj_new[which(mortality_data$age == aa)] <- 
    mortality_data$mortality_proj[which(mortality_data$age == aa)] - 0.0001 #*mortality_data$mortality_proj #which mortality_data$age == aa
  
  # Run forecast with perturbed mortality
  forecasts_both <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "mortality",
                                   start_year = 2023, end_year = 2023+125, end_age = end_age, 
                                   no_births = FALSE, fertility_type = "fertility_med", loc_name = "Regions", 
                                   growth_transitions = TRUE, project_100plus = TRUE)
  
  # Calculate life expectancy with perturbed mortality
  LE_birth_results <- mortality_data %>%
    group_by(location, year) %>%
    mutate(survival = cumprod(1 - pmin(mortality_proj_new/1e5, 1))) %>%
    summarise(LE_birth_new = sum(survival))
  
  # Store results for this age
  dN_dmu_results <- rbind(dN_dmu_results, 
                          forecasts_both %>% mutate(age = aa) %>%
                            group_by(location, year, age) %>%
                            summarise(pop_new = sum(population)) %>%
                            left_join(LE_birth_results) %>%
                            right_join(pops_base)
  ) 
}
beep()

# Visualize population derivatives
dN_dmu_results %>%
  rbind(mutate(dN_dmu_results, location = "Global")) %>%
  group_by(location, year, age) %>% 
  summarise(pop_new = sum(pop_new), pop = sum(pop)) %>%
  filter(year %in% c(2022, 2050, 2100), location %in%  c("Global", "High Income", "Low Income")) %>%
  mutate(dN_dmu = pop_new - pop) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~location, scales = "free_y") +
  geom_line(aes(x = age, y = dN_dmu, color = factor(year)))
# Save
#ggsave("figures/derivatives/dN_dmu.pdf", width = 10, height = 4)

# Visualize life expectancy derivatives
dN_dmu_results %>%
  rbind(mutate(dN_dmu_results, location = "Global")) %>%
  group_by(location, year, age) %>% 
  summarise(LE_birth_new = sum(pop_new*LE_birth_new)/sum(pop_new), LE_birth = sum(pop*LE_birth)/sum(pop)) %>%
  filter(year %in% c(2022, 2050, 2100), location %in%  c("Global", "High Income", "Low Income")) %>%
  mutate(dLE_dmu = 1e5*(LE_birth_new - LE_birth)) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~location, scales = "free_y") +
  geom_line(aes(x = age, y = dLE_dmu, color = factor(year)))
# Save
#ggsave("figures/derivatives/dLE_dmu.pdf", width = 10, height = 4)



# ------------------------------------------------------------------------------
# Pure forecasting -------------------------------------------------------------

# Need for both transition tables and life expectancy analysis


# Pure forecasting (i.e. don't eradicate at all, just play forward)

# Options
start_years <- c(2023)
categories <- c("infant", "adult_early", "adult_late", "ageing", "ageing_nocov")
erad_opts <- c(0,1)
grwth_opts <- c(FALSE, TRUE)
birth_opts <- c(FALSE, TRUE)


# Convert to dataframes as tibbles don't cooperate...
mortality_data <- mortality_df %>%
  select(location, year, age_name, age, mortality, mortality_proj, 
         mortality_infant, mortality_infant_proj, mortality_adult_early, mortality_adult_early_proj,
         mortality_adult_late_proj, mortality_adult_late_proj, mortality_ageing, mortality_ageing_proj,
         mortality_ageing_nocov, mortality_ageing_nocov_proj, mortality_covid, mortality_covid_proj) %>%
  as.data.frame()
disability_data <- disability_df %>%
  select(location, year, age_name, age, disability, disability_proj,
         disability_infant, disability_infant_proj, disability_adult_early, disability_adult_early_proj,
         disability_adult_late, disability_adult_late_proj, disability_ageing, disability_ageing_proj,
         disability_ageing_nocov, disability_ageing_nocov_proj, disability_covid, disability_covid_proj) %>%
  as.data.frame()

# Compare to benchmark
sy <- start_years[1]
cat1 <- categories[5]
cat2 <- categories[1]
erad1 <- erad_opts[1]
erad2 <- erad_opts[1]
grwth <- grwth_opts[1]
brth <- birth_opts[1]

# loop over combos
forecasts_1set <- tibble()
forecasts_allsets <- tibble()
# Change the start year
n_iters <- length(start_years)*length(grwth_opts)*length(birth_opts)*length(categories)*length(erad_opts)*length(c("ageing"))*length(c(0))
ii<- 0

# Nested loop over all scenario combinations
pb <- txtProgressBar(min = 1, max = n_iters)
for (sy in start_years){
  # Include economic growth
  for (grwth in grwth_opts){
    # Include new births
    for (brth in birth_opts){
      # Which disease category 1
      for (cat1 in categories){
        # How much to eradicate of that disease
        for (erad1 in erad_opts){
          # Which disease category 2
          for (cat2 in c("ageing")){
            # How much to eradicate of second disease (for the cross derivs)
            for (erad2 in c(0)){ #erad_opts){
              # Only go ahead if erad*erad_s is less that 0.5
              #if (cat1 < cat2){
              
              ii <- ii+1
              setTxtProgressBar(pb, ii)
              # Progress update
              print(str_c("Start year ", sy, ", diseases1 ", cat1, " eradicating ", erad1, " and disease2 ", cat2, " eradicating ", erad2, 
                          ", growth ", grwth, ", no births ", brth))
              
              # Define new mortality and disabilty fns
              mortality_data$mortality_proj_new <- mortality_data$mortality_proj - erad1*mortality_data[,str_c("mortality_",cat1,"_proj")]
              mortality_data$mortality_proj_new <- mortality_data$mortality_proj_new - erad2*mortality_data[,str_c("mortality_",cat2,"_proj")]
              disability_data$disability_proj_new <- disability_data$disability_proj - erad1*disability_data[,str_c("disability_",cat1, "_proj")]
              disability_data$disability_proj_new <- disability_data$disability_proj_new - erad2*disability_data[,str_c("disability_",cat2,"_proj")]
              
              # Calculate Life Expectancy etc
              life_cycle_results <- mortality_data %>%
                
                arrange(location, year, age) %>%
                group_by(location, year) %>%
                mutate(survival = cumprod(1 - pmin(mortality_proj_new/1e5, 1)),
                       survival_lag = replace_na(lag(survival, n = 1), 1)) %>%
                mutate(LE_birth = sum(survival),
                       rem_LE = replace_na(revcumsum(survival)/survival_lag, 0)) %>%
                
                left_join(select(disability_data, location, year, age, disability_proj, disability_proj_new,
                                 disability_infant_proj, disability_adult_early_proj, disability_adult_late_proj, disability_ageing_proj,
                                 disability_ageing_nocov_proj, disability_covid_proj)) %>%
                
                select(location, year, age_name, age, mortality_proj, mortality_proj_new,  disability_proj, disability_proj_new,
                       survival, survival_lag, rem_LE, LE_birth,
                       mortality_infant_proj, mortality_adult_early_proj, mortality_adult_late_proj, mortality_ageing_proj,
                       mortality_ageing_nocov_proj, mortality_covid_proj,
                       disability_infant_proj, disability_adult_early_proj, disability_adult_late_proj, disability_ageing_proj,
                       disability_ageing_nocov_proj, disability_covid_proj) 
              if (grwth){
                life_cycle_results <- life_cycle_results
              } else {
                life_cycle_results<-life_cycle_results %>%
                  
                  mutate(
                    mortality_proj = case_when(year>sy ~ NA_real_, TRUE ~ mortality_proj),
                    mortality_proj_new = case_when(year>sy ~ NA_real_, TRUE ~ mortality_proj_new),
                    disability_proj = case_when(year>sy ~ NA_real_, TRUE ~ disability_proj),
                    disability_proj_new = case_when(year>sy ~ NA_real_, TRUE ~ disability_proj_new),
                    survival = case_when(year>sy ~ NA_real_, TRUE ~ survival),
                    survival_lag = case_when(year>sy ~ NA_real_, TRUE ~ survival_lag),
                    rem_LE = case_when(year>sy ~ NA_real_, TRUE ~ rem_LE),
                    LE_birth = case_when(year>sy ~ NA_real_, TRUE ~ LE_birth),
                    mortality_infant_proj = case_when(year>sy ~ NA_real_, TRUE ~ mortality_infant_proj),
                    mortality_adult_early_proj = case_when(year>sy ~ NA_real_, TRUE ~ mortality_adult_early_proj),
                    mortality_adult_late_proj = case_when(year>sy ~ NA_real_, TRUE ~ mortality_adult_late_proj),
                    mortality_ageing_proj = case_when(year>sy ~ NA_real_, TRUE ~ mortality_ageing_proj),
                    disability_infant_proj = case_when(year>sy ~ NA_real_, TRUE ~ disability_infant_proj),
                    disability_adult_early_proj = case_when(year>sy ~ NA_real_, TRUE ~ disability_adult_early_proj),
                    disability_adult_late_proj = case_when(year>sy ~ NA_real_, TRUE ~ disability_adult_late_proj),
                    disability_ageing_proj = case_when(year>sy ~ NA_real_, TRUE ~ disability_ageing_proj)
                  ) %>%
                  
                  group_by(location, age) %>%
                  fill(mortality_proj, mortality_proj_new,  disability_proj, disability_proj_new, survival, survival_lag, rem_LE, LE_birth,
                       mortality_infant_proj, mortality_adult_early_proj, mortality_adult_late_proj, mortality_ageing_proj,
                       mortality_ageing_nocov_proj, mortality_covid_proj,
                       disability_infant_proj, disability_adult_early_proj, disability_adult_late_proj, disability_ageing_proj,
                       disability_ageing_nocov_proj, disability_covid_proj,
                       .direction = "down") %>%
                  ungroup()
                
              }
              
              # Forecast eradicating on both mortality and disability
              forecasts_both <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "both",
                                               start_year = sy, end_year = sy+125, end_age = end_age, 
                                               no_births = brth, fertility_type = "fertility_med", loc_name = "Regions", 
                                               growth_transitions = grwth, project_100plus = TRUE) %>% 
                mutate(start_year = sy, no_births = brth, growth_transitions = grwth, 
                       disease1 = cat1, erad1 = erad1, disease2 = cat2, erad2 = erad2, type = "both") %>%
                select(start_year, no_births, growth_transitions, disease1, erad1, disease2, erad2, type, 
                       location, year, age, mortality, population, daly)
              
              # Forecast eradicating on just mortality
              forecasts_mort <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "mortality",
                                               start_year = sy, end_year = sy+125, end_age = end_age, 
                                               no_births = brth, fertility_type = "fertility_med", loc_name = "Regions", 
                                               growth_transitions = grwth, project_100plus = TRUE) %>% 
                mutate(start_year = sy, no_births = brth, growth_transitions = grwth, 
                       disease1 = cat1, erad1 = erad1, disease2 = cat2, erad2 = erad2, type = "mortality") %>%
                select(start_year, no_births, growth_transitions, disease1, erad1, disease2, erad2, type, location, year, age, mortality, population, daly)
              
              # Forecast eradicating on just disability
              forecasts_disab <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "disability",
                                                start_year = sy, end_year = sy+125, end_age = end_age, 
                                                no_births = brth, fertility_type = "fertility_med", loc_name = "Regions", 
                                                growth_transitions = grwth, project_100plus = TRUE) %>% 
                mutate(start_year = sy, no_births = brth, growth_transitions = grwth, 
                       disease1 = cat1, erad1 = erad1, disease2 = cat2, erad2 = erad2, type = "disability") %>%
                select(start_year, no_births, growth_transitions, disease1, erad1, disease2, erad2, type, location, year, age, mortality, population, daly)
              
              # Combine all three forecasts and join life cycle results
              forecasts_1set <- rbind(forecasts_both, forecasts_mort, forecasts_disab) %>%
                left_join(life_cycle_results)
              
              #}
              # Compare scenarios
              forecasts_allsets <- forecasts_allsets %>%
                rbind(forecasts_1set)
            }
          }
        }
        # Save intermediate results
        print("Saving file")
        forecasts_allsets %>%
          saveRDS("temp.rds")
      }
    }
  }
}
beep()
## Save final results
results_dir <- "output"
if (!dir.exists(results_dir)){
  dir.create(results_dir)
}
saveRDS(forecasts_allsets, file = file.path(results_dir, "forecast_examples.rds"))




# Options for partial eradication -----------------------------------------

# Systematically go through some options for partial eradication 

# Options
start_years <- c(2023)# c(1990, 2019, 2023)
categories <- c("infant", "adult_early", "adult_late", "ageing", "ageing_nocov")
erad_opts <- seq(-0.1,1,0.05)
grwth_opts <- c(TRUE, FALSE) #c(FALSE, TRUE)
birth_opts <- c(FALSE) #c(FALSE, TRUE)

# Convert to dataframes as tibbles don't cooperate...
mortality_data <- mortality_df %>%
  select(location, year, age_name, age, mortality, mortality_proj, 
         mortality_infant, mortality_infant_proj, mortality_adult_early, mortality_adult_early_proj,
         mortality_adult_late_proj, mortality_adult_late_proj, mortality_ageing, mortality_ageing_proj,
         mortality_ageing_nocov, mortality_ageing_nocov_proj, mortality_covid, mortality_covid_proj) %>%
  as.data.frame()

disability_data <- disability_df %>%
  select(location, year, age_name, age, disability, disability_proj,
         disability_infant, disability_infant_proj, disability_adult_early, disability_adult_early_proj,
         disability_adult_late, disability_adult_late_proj, disability_ageing, disability_ageing_proj,
         disability_ageing_nocov, disability_ageing_nocov_proj, disability_covid, disability_covid_proj) %>%
  as.data.frame()


# Quick validation of health-adjusted life expectancy calculations
mortality_data %>% 
  select(-age_name) %>%
  inner_join(select(disability_data, -age_name)) %>%
  filter(year >= year) %>%
  group_by(location, year) %>%
  mutate(survival = cumprod(1 - pmin(mortality_proj/1e5, 1)),
         hsurv = (1 - pmin(disability_proj/1e5, 1))*survival) %>%
  summarise(LE_birth = sum(survival),
            HLE_birth = sum(hsurv)) %>%
  filter(year == 2024)

# Compare to benchmark
sy <- 2023 # start_years[1]
cat1 <- "infant" # categories[4]
cat2 <- "ageing" # categories[4]
erad1 <- 1 # erad_opts[1]
erad2 <- 0 # erad_opts[1]
grwth <- TRUE # grwth_opts[1]
brth <- FALSE # birth_opts[1]

# loop over combos

# Initialize results storage
full_scenarios <- tibble()
W_scenarios <- tibble()

# Change the start year
# Calculate total number of scenario combinations
n_iters <- length(start_years)*length(birth_opts)*length(categories)*length(erad_opts)*length(c("ageing"))*length(c(0))
ii<- 0
pb <- txtProgressBar(min = 1, max = n_iters)

# Nested loop over all scenario combinations
for (sy in start_years){
  # Include economic growth
  for (grwth in grwth_opts){
    # Include new births
    for (brth in birth_opts){
      # Which disease category 1
      for (cat1 in categories){
        # How much to eradicate of that disease
        for (erad1 in erad_opts){
          # Which disease category 2
          for (cat2 in c("ageing")){
            # How much to eradicate of second disease (for the cross derivs)
            for (erad2 in c(0)){ #erad_opts){
              # Only go ahead if erad*erad_s is less that 0.5
              #if (cat1 < cat2){
              
              # Progress update
              ii <- ii+1
              setTxtProgressBar(pb, ii)
              print(str_c("Start year ", sy, ", diseases1 ", cat1, " eradicating ", erad1, " and disease2 ", cat2, " eradicating ", erad2, 
                          ", growth ", grwth, ", no births ", brth))
              
              # Define new mortality and disabilty fns
              mortality_data$mortality_proj_new <- mortality_data$mortality_proj - erad1*mortality_data[,str_c("mortality_",cat1,"_proj")]
              mortality_data$mortality_proj_new <- mortality_data$mortality_proj_new - erad2*mortality_data[,str_c("mortality_",cat2,"_proj")]
              disability_data$disability_proj_new <- disability_data$disability_proj - erad1*disability_data[,str_c("disability_",cat1, "_proj")]
              disability_data$disability_proj_new <- disability_data$disability_proj_new - erad2*disability_data[,str_c("disability_",cat2,"_proj")]
              
              # Calculate Life Expectancy under intervention
              LE_birth_results <- mortality_data %>% 
                select(-age_name) %>%
                inner_join(select(disability_data, -age_name)) %>%
                filter(year >= sy) %>%
                group_by(location, year) %>%
                mutate(survival = cumprod(1 - pmin(mortality_proj_new/1e5, 1)),
                       hsurv = (1 - pmin(disability_proj_new/1e5, 1))*survival) %>%
                summarise(LE_birth = sum(survival),
                          HLE_birth = sum(hsurv))
              
              # Run three intervention types: both, mortality-only, disability-only
              
              # Forecast eradicating on both mortality and disability
              forecasts_both <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "both",
                                               start_year = sy, end_year = sy+125, end_age = end_age, 
                                               no_births = brth, fertility_type = "fertility_med", loc_name = "Regions", 
                                               growth_transitions = grwth, project_100plus = TRUE) %>% 
                mutate(start_year = sy, no_births = brth, growth_transitions = grwth, 
                       disease1 = cat1, erad1 = erad1, disease2 = cat2, erad2 = erad2, type = "both") %>%
                select(start_year, no_births, growth_transitions, disease1, erad1, 
                       disease2, erad2, type, location, year, age, mortality, population, daly)
              
              # Forecast eradicating on just mortality
              forecasts_mort <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "mortality",
                                               start_year = sy, end_year = sy+125, end_age = end_age, 
                                               no_births = brth, fertility_type = "fertility_med", loc_name = "Regions", 
                                               growth_transitions = grwth, project_100plus = TRUE) %>% 
                mutate(start_year = sy, no_births = brth, growth_transitions = grwth, 
                       disease1 = cat1, erad1 = erad1, disease2 = cat2, erad2 = erad2, type = "mortality") %>%
                select(start_year, no_births, growth_transitions, disease1, erad1, 
                       disease2, erad2, type, location, year, age, mortality, population, daly)
              
              # Forecast eradicating on just disability
              forecasts_disab <- forecast_dalys(population_df, fertility_df, mortality_data, disability_data, new = "disability",
                                                start_year = sy, end_year = sy+125, end_age = end_age, 
                                                no_births = brth, fertility_type = "fertility_med", loc_name = "Regions", 
                                                growth_transitions = grwth, project_100plus = TRUE) %>% 
                mutate(start_year = sy, no_births = brth, growth_transitions = grwth, 
                       disease1 = cat1, erad1 = erad1, disease2 = cat2, erad2 = erad2, type = "disability") %>%
                select(start_year, no_births, growth_transitions, disease1, erad1, 
                       disease2, erad2, type, location, year, age, mortality, population, daly)
              
              # Aggregate results by year and calculate welfare measures
              full_scenarios <- rbind(forecasts_both, forecasts_mort, forecasts_disab) %>%
                mutate(newborn = case_when(year - start_year - age == 0 ~ 1, TRUE ~ 0)) %>%
                group_by(start_year, no_births, growth_transitions, disease1, erad1, disease2, erad2, type, location, year) %>%
                summarise(
                  W = sum(daly), 
                  Wnewborn = sum(daly*newborn), 
                  average_age = sum(age*population)/sum(population), 
                  pop_newborns = sum(newborn*population),
                  population = sum(population)
                ) %>%
                left_join(LE_birth_results)
              
              #}
              # Compare scenarios
              # Store results
              W_scenarios <- W_scenarios %>%
                rbind(full_scenarios)
            }
          }
        }
        # Periodic saving
        print("Saving file")
        W_scenarios %>%
          saveRDS("temp.rds")
      }
    }
  }
}
beep()

# Save final comprehensive results
results_dir <- "output"
if (!dir.exists(results_dir)){
  dir.create(results_dir)
}
saveRDS(W_scenarios, file = file.path(results_dir, "W_scenarios.rds"))


# Final aggregation and summary
full_scenarios %>%
  mutate(location = "Global") %>%
  group_by(start_year, no_births, growth_transitions, disease1, type, location, erad1, year) %>%
  summarise(W = sum(W), Wnewborn = sum(Wnewborn), 
            average_age = sum(average_age*population)/sum(population), LE_birth = sum(LE_birth*population)/sum(population),
            HLE_birth = sum(HLE_birth*population)/sum(population),
            population = sum(population), pop_newborns = sum(pop_newborns)) %>%
  
  # Create readable disease category labels
  mutate(diseases = case_when(str_detect(disease1, "infant") ~ "Infant",
                              str_detect(disease1, "adult_early") ~ "Adult (early)",
                              str_detect(disease1, "adult_late") ~ "Adult (late)",
                              str_detect(disease1, "ageing") ~ "Ageing-related", TRUE ~ "All")) %>%
  mutate(diseases = factor(diseases, ordered = T, levels = c("Infant", "Adult (early)",  "Adult (late)", "Ageing-related")),
         location = str_remove(location, "World Bank "),
         location = factor(location, ordered = T, levels = c("Global", "Low Income",  "Lower Middle Income", "Upper Middle Income", "High Income")))

























