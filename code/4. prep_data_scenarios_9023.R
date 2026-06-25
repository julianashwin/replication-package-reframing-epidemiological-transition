# Title: prep_data_scenarios_9023
# Description: Ingests macro data to compute GNI per capita growth by country, 
# projects those paths forward to 2200 using simple average growth assumptions, 
# reclassifies countries into World Bank–style income groups based on projected GNI 
# thresholds, and visualizes/exports the implied income-group transition shares over time.
#
#
# ------------------------------------------------------------------------------

rm(list=ls())

library(here)
here::i_am("code/4. prep_data_scenarios_9023.R")
here::dr_here()

library(tidyverse)
# library(ggpubr)
library(readxl)
library(stargazer)
library(janitor)


gbd_deaths_df <- readRDS(here("output-data", "gbd_deaths.rds"))
gbd_yld_df <- readRDS(here("output-data", "gbd_ylds.rds"))
clusters_df_2023 <- readRDS(here("output-data", "cluster4_assignment_2023.rds"))  

gbd_ages <- readRDS(here("output-data", "gbd_data_ages.rds"))

cluster_map23 <- clusters_df_2023 %>%
  distinct(cause_name, cluster_4)



# Constants ---------------------------------------------------------------

# Ordered vector of age groups to enforce consistent factor ordering
age_groups <- c("0-1 years", "1-2 years", "2-4 years", "5-9 years", "10-14 years", "15-19 years", 
                "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", 
                "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", 
                "80-84 years", "85-89 years", "90-94 years", "95-99 years")
# World Bank income groups in ordered form
income_groups <- c("Global", "High Income", "Upper Middle Income", 
                   "Lower Middle Income", "Low Income")
# Disease clusters (4 + COVID as a recoded overlay)
cluster_groups <- c("Ageing-related", "COVID-related", "Adult (late)", 
                    "Adult (early)", "Infant")

# Colors for income groups (for plots)
loc_cols <- c(
  "High Income"         = "forestgreen",
  "Upper Middle Income" = "green", 
  "Lower Middle Income" = "orange",
  "Low Income"          = "red"
)


# calling GBD countries
gbd_countries <- readRDS(here("output-data", "gbd_data_countries.rds"))


# ------------


# GBD deaths and YLDs, with income-group factor cleanup
gbd_deaths_df <- gbd_deaths_df %>%
  mutate(location_name = factor(str_remove(location_name, "World Bank "), ordered = T, levels = income_groups),
         income_group = location_name)

gbd_yld_df <- gbd_yld_df %>%
  mutate(location_name = factor(str_remove(location_name, "World Bank "), ordered = T, levels = income_groups),
         income_group = location_name)



# Mortality for medium, lower and upper variants --------------------------

# In creating age-year-income group time series for mortality and disability - 
# since projections and functions code only uses the last year and projects forwards, only need 2023.

# this I do in 3.static analysis - i.e. only for years 1990 and 2023:

mortality_med_df <- read_csv(here("output-data", "nob_mortality_medium.csv"))
health_med_df <- read_csv(here("output-data", "nob_disability_medium.csv"))



# Macro data: GNI per capita, growth rates, and projections -----------------

# Get GNI per capita for each country in each region
macro_df <- read_csv(here("output-data", "country_wdi_data.csv")) %>%
  mutate(
    location     = str_remove(location, "World Bank "),
    income_group = str_remove(income_group, "World Bank ")
  ) %>%
  filter(location_name %in% c(gbd_countries$location_name, income_groups)) %>% # filtered with 2021 GBD countries 
  mutate(time = as.numeric(as.factor(year))) %>%
  select(location, location_name, year, time, GNI_pc, WDI_population, income_group) %>%
  # Keep either (location != "Country") or rows with a known income_group
  filter(location != "Country" | !is.na(income_group)) %>%
  mutate(GNI_pc = case_when(GNI_pc == 0 ~ NA_real_, TRUE ~ GNI_pc)) %>%
  group_by(location_name) %>% 
  arrange(year) %>%
  mutate(
    GNI_pc_1lag   = lag(GNI_pc, order_by = year),
    GNI_growth    = log(GNI_pc) - log(GNI_pc_1lag),
    GNI_growth_mean = mean(GNI_growth, na.rm = TRUE)
  ) %>%
  ungroup()

# Check some special cases
# Inspect quirks: countries with negative avg growth & missing income group
unique(macro_df[which(macro_df$GNI_growth_mean < 0), c("location_name", 
                                                       "income_group", "GNI_growth_mean")])
# Negative: Brazil, Nigeria and Yemen --> very small, Yemen a bit larger

unique(macro_df[which(is.na(macro_df$income_group)), c("location_name", 
                                                       "income_group")])

# Plot the distribution of average growth rates (GNI p.c.)
# Histogram of average growth rates by income group
macro_df %>%
  filter(location == "Country") %>%
  select(location_name, income_group, GNI_growth_mean) %>%
  distinct() %>%
  ggplot() + theme_bw() + 
  scale_fill_manual("Region", values = loc_cols) +
  geom_histogram(
    aes(x = GNI_growth_mean, fill = income_group),
    position = "dodge",
    breaks = seq(-0.09, 0.14, 0.01)
  ) +  labs(x = "Average GNI growth rate", y = "")


# Plot GNI p.c. over time in each region
macro_df %>%
  filter(location == "Country") %>%
  distinct() %>%
  ggplot() + theme_bw() + 
  facet_wrap(.~ income_group) + guides(color = "none") +
  geom_line(aes(x = year, y = log(GNI_pc), group = location_name), alpha = 0.3) +
  geom_smooth(aes(x = year, y = log(GNI_pc)), method = "lm", se = F) +
  labs(y = "log GNI per capita", x = "Year")

# missing values in plot coming from NA values in GNI pc in earlier years - i.e. missing for lots of countries through 1960s

# Snapshot stats for 2023 & preparation for projections
gni_stats <- macro_df %>% 
  arrange(location_name, year) %>%
  group_by(location_name) %>% 
  fill(GNI_pc, WDI_population) %>% 
  filter(location == "Country" & year == 2023) %>% 
  filter(!is.na(GNI_pc), !is.na(WDI_population)) %>%
  group_by(income_group) %>%
  summarise(
    min        = min(GNI_pc, na.rm = TRUE),
    max        = max(GNI_pc, na.rm = TRUE), 
    median     = median(GNI_pc, na.rm = TRUE),
    mean_grwth = mean(GNI_growth_mean, na.rm = TRUE),
    total_pop  = sum(WDI_population, na.rm = TRUE),
    .groups    = "drop"
  )
gni_stats

# All country names with non-missing GNI_pc somewhere
all_names <- macro_df %>% 
  filter(location == "Country") %>%
  filter(!is.na(GNI_pc)) %>%
  distinct(location_name)


# Growth projections (country-level), 2023–2200
# - Carry forward last observed GNI_pc, WDI_population, and mean growth
# - Apply constant mean growth after 2023
# - Classify projected income group by GNI_pc thresholds
growth_proj <- macro_df %>% 
  filter(location == "Country") %>%
  select(-location, -GNI_growth_mean, -GNI_growth, -GNI_pc_1lag) %>%
  arrange(location_name, year) %>%
  group_by(location_name) %>% 
  fill(GNI_pc, WDI_population) %>% 
  filter(year >= 2022) %>%
  filter(!is.na(GNI_pc), !is.na(WDI_population)) %>%
  # Build a complete future panel (2023..2200) for each country
  full_join(crossing(location_name = all_names$location_name, year = 2023:2200),
            by = c("location_name", "year")) %>% 
  arrange(location_name, year) %>%
  # Attach per-income-group average growth and total pop (for shares)
  left_join(select(gni_stats, income_group, mean_grwth, total_pop), by = "income_group") %>%
  group_by(location_name) %>% 
  fill(GNI_pc, mean_grwth, income_group, WDI_population, total_pop, .direction = "down") %>%
  # Zero growth up to 2023; then apply constant mean growth
  mutate(mean_grwth = case_when(year <= 2023 ~ 0, TRUE ~ mean_grwth)) %>%
  mutate(factor = cumprod(1 + mean_grwth), pop_share = WDI_population / total_pop) %>%
  mutate(GNI_pc = GNI_pc * factor) %>%
  # Income class rules of thumb (see WB thresholds in comment below)
  mutate(
    income_group_proj = case_when(
      year <= 2023           ~ income_group,
      GNI_pc <  1150        ~ "Low Income",
      GNI_pc <  4470        ~ "Lower Middle Income",
      GNI_pc < 14000        ~ "Upper Middle Income",
      GNI_pc >= 14000       ~ "High Income"
    )
  )
# for the WB thresholds see here: 
# https://blogs.worldbank.org/en/opendata/new-world-bank-country-classifications-income-level-2022-2023

# Sanity check: projected classes at baseline year
growth_proj %>%
  filter(year == 2023) %>%
  tabyl(income_group, income_group_proj)

# Visualize projected log(GNI_pc) trajectories by original income group
growth_proj %>%
  filter(year < 2124) %>%
  ggplot() + facet_wrap(~income_group) +
  theme_bw() + scale_color_manual("Region", values = loc_cols) +
  geom_line(aes(x = year, y = log(GNI_pc), group = location_name, color = income_group_proj)) +
  geom_smooth(aes(x = year, y = log(GNI_pc)), method = "lm", se = F) +
  labs(y = "log GNI per capita", x = "Year")

# Income-group transition shares over time
# Compute, for each original income group, the share of its population
# that falls into each *projected* class by year.
income_transition_shares <- growth_proj %>%
  group_by(income_group, income_group_proj, year) %>%
  summarise(pop_share = sum(pop_share)) %>%
  # Before classification year (pre-2023), treat share as 1 in own group
  mutate(pop_share = case_when(year <= 2023 ~ 1, TRUE ~ pop_share)) %>%
  arrange(income_group, year, income_group_proj)

# Quick peek for random year
income_transition_shares %>% filter(year == 2025)

# Stacked bars: projected shares by original income group
income_transition_shares %>%
  filter(year >= 2022 & year <= 2124) %>%
  ggplot(aes(x = year)) + theme_bw() + 
  facet_wrap(~ income_group) +
  scale_fill_manual("Region", values = loc_cols) +
  scale_color_manual("Region", values = loc_cols) +
  geom_bar(
    aes(y = pop_share, fill = income_group_proj, color = income_group_proj),
    width = 1, stat = "identity", position = "stack"
  ) +
  guides(color = "none") + 
  labs(x = "Year", y = "Projected share")
#ggsave("output/WB_region_transitions.pdf", width = 8, height = 4)

# Wide table of transition shares (one col per projected class)
income_transition_shares <- income_transition_shares %>%
  pivot_wider(
    id_cols     = c(income_group, year),
    names_from  = income_group_proj, 
    values_from = pop_share, values_fill = 0
  )
# Save as CSV
write_csv(income_transition_shares, here("output-data", "income_transition_shares.csv"))


# income_transition_shares: for each baseline income group and year, what fraction of that group's baseline-weighted population is classified into each projected income group
# based on projected GNI per capita path












