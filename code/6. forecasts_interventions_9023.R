# Title: forecasts_interventions_9023.R
# 
# Description aim: Full, 50%, 25% disease eradication analysis, replicates Figure 4 and 5
#                  Computes remaining life expectancy effects at age 70
#                  Plots and tables of pure projections of disease burdens - i.e. not making any epidemiological assumptions
# ------------------------------------------------------------------------------

rm(list=ls())

library(here)
here::i_am("code/6. forecasts_interventions_9023.R")
here::dr_here()


library(tidyverse)
library(ggplot2)
library(ggpubr)
library(readxl)
library(janitor)
library(beepr)
library(stargazer)

# Call custom functions
source("code/functions_9023.R")

dir.create(here("output", "figures_pdfs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figure_underlying_data"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "covid_as_ageing"), recursive = TRUE, showWarnings = FALSE)



# Constants ---------------------------------------------------------------

# Define consistent color schemes for regions and disease clusters
loc_cols <- c("Global" = "black","High Income" = "forestgreen", 
              "Upper Middle Income" = "green", "Lower Middle Income" = "orange",
              "Low Income" = "red")

cluster_cols <- c("Ageing-related" = "firebrick1", "Ageing-related (no COVID)" = "firebrick1",
                  "COVID-related" = "firebrick4", "Adult (late)" = "blue3", 
                  "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")

# Define ordered factor levels for consistent plotting
cluster_groups <- c("Ageing-related", "Ageing-related (no COVID)", 
                    "COVID-related", "Adult (late)", "Adult (early)", "Infant")
income_groups <- c("Global", "High Income", "Upper Middle Income", 
                   "Lower Middle Income", "Low Income")


# data
clusters_df_2023 <- readRDS(here("output-data", "cluster4_assignment_2023.rds")) %>%  
  mutate(cluster_4 = case_when(str_detect(cause_name, "COVID") ~ "COVID-related", 
                               str_detect(cluster_4, "Ageing") ~ "Ageing-related (no COVID)", 
                               TRUE ~ cluster_4)) %>%
  distinct(cause_name, cluster_4) %>%
  mutate(cluster = factor(cluster_4, ordered = T, levels = cluster_groups))

jlifetab_df <- readRDS(here("output-data", "country_lifetab_data.rds"))

# ------------------------------------------------------------------------------

# Reproducing Figure 4 - global health gains from reducing prevalence for world population (in DALYs)
# scenarios of permanent reductions in disease prevalence of each cluster 


# Eradication scenarios

# Import comprehensive welfare scenario results
W_scenarios <- read_rds(here("output", "W_scenarios.rds"))
  
  
# Clean scenario data by removing cross-partial terms (focus on single-disease effects)
# Remove the cross partials 
W_scenarios_nocross <- W_scenarios %>%
  ungroup() %>%
  filter(erad2 == 0) %>%
  select(-erad2, -disease2) %>%
  filter(!is.na(W)) %>%
  distinct()

# Add global aggregations and clean disease category names
W_scenarios_nocross <- W_scenarios_nocross %>%
  mutate(location = "Global") %>%
  group_by(start_year, no_births, growth_transitions, disease1, type, location, erad1, year) %>%
  summarise(W = sum(W), Wnewborn = sum(Wnewborn), 
            average_age = sum(average_age*population)/sum(population), LE_birth = sum(LE_birth*population)/sum(population),
            HLE_birth = sum(HLE_birth*population)/sum(population),
            population = sum(population), pop_newborns = sum(pop_newborns)) %>%
  rbind(W_scenarios_nocross) %>%
  # Create readable disease labels
  mutate(diseases = case_when(str_detect(disease1, "infant") ~ "Infant",
                              str_detect(disease1, "adult_early") ~ "Adult (early)",
                              str_detect(disease1, "adult_late") ~ "Adult (late)",
                              str_detect(disease1, "nocov") ~ "Ageing-related (no COVID)",
                              str_detect(disease1, "ageing") ~ "Ageing-related", TRUE ~ "All")) %>%
  mutate(diseases = factor(diseases, ordered = T, levels = c("Infant", "Adult (early)",  "Adult (late)", "Ageing-related", "Ageing-related (no COVID)")),
         location = str_remove(location, "World Bank "),
         location = factor(location, ordered = T, levels = c("Global", "Low Income",  "Lower Middle Income", "Upper Middle Income", "High Income")))



# Full eradication scenario analysis --------------------------------------

W_scenarios_nocross %>%
  group_by(start_year, no_births, growth_transitions, diseases, type, location) %>%
  mutate(W_start = sum((erad1 == 0)*W),
         Wnewborn_start = sum((erad1 == 0)*Wnewborn))

# Analyze complete (100%) disease eradication scenarios
full_erad_example <- W_scenarios_nocross %>%
  filter(start_year == 2023, no_births == FALSE, erad1 %in% c(0,1)) %>%
  distinct(growth_transitions, diseases, type, location, erad1, year, W, LE_birth, HLE_birth) %>%
  pivot_wider(id_cols = c(growth_transitions, diseases, type, location, year), 
              names_from = erad1, values_from = c(W, LE_birth, HLE_birth)) %>%
  mutate(change = W_1 - W_0, # Welfare change from eradication
         scenario = "100% reduction")


# Partial eradication scenarios -------------------------------------------

# Analyze partial eradication scenarios (25% and 50% reduction) --> previously 20% but as per editor comments

pc25_erad_example <- W_scenarios_nocross %>%
  mutate(erad1 = round(erad1, 3)) %>%
  filter(erad1 %in% c(0,0.25)) %>%
  filter(start_year == 2023, no_births == FALSE, erad1 %in% c(0,0.25)) %>%  
  distinct(growth_transitions, diseases, type, location, erad1, year, W, LE_birth, HLE_birth) %>%
  pivot_wider(id_cols = c(growth_transitions, diseases, type, location, year), 
              names_from = erad1, values_from = c(W, LE_birth, HLE_birth)) %>%
  mutate(change = W_0.25 - W_0,
         scenario = "25% reduction")

pc50_erad_example <- W_scenarios_nocross %>%
  mutate(erad1 = round(erad1, 3)) %>%
  filter(erad1 %in% c(0,0.50)) %>%
  filter(start_year == 2023, no_births == FALSE, erad1 %in% c(0,0.50)) %>%  
  distinct(growth_transitions, diseases, type, location, erad1, year, W, LE_birth, HLE_birth) %>%
  pivot_wider(id_cols = c(growth_transitions, diseases, type, location, year), names_from = erad1, values_from = c(W, LE_birth, HLE_birth)) %>%
  mutate(change = W_0.5 - W_0,
         scenario = "50% reduction")

# Combine all eradication levels for comparison
partial_erad_examples <- full_erad_example %>%
  rbind(pc25_erad_example) %>%
  rbind(pc50_erad_example) %>%
  mutate(scenario = factor(scenario, levels = c("25% reduction", "50% reduction", "100% reduction"))) %>%
  mutate(change = change/1e6) %>%
  pivot_wider(id_cols = c(growth_transitions, location, year, diseases, scenario), names_from = type, values_from = change) 

# Visualize partial eradication effects with economic growth
partial_erad_examples %>%
  filter(diseases != "Ageing-related") %>%
  filter(location == "Global", growth_transitions == TRUE) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~scenario+diseases, nrow = 3) +
  geom_line(aes(x = year, y = both), color = "black") + 
  geom_bar(aes(x = year, y = both, fill = "Complementarity"), stat = "identity") + 
  geom_bar(aes(x = year, y = mortality+disability, fill = "Mortality"), stat = "identity") + 
  geom_bar(aes(x = year, y = disability, fill = "Disability"), stat = "identity") + 
  labs(x = "Year", y = "Extra DALYs lived as a result of reduced prevalence (millions)", 
       fill = "Channel")
# if toggle growth transitions, change saving name
ggsave(here("output", "projections", "fig4_eradication25_gr.jpg"), 
       width = 10, height = 8)


# without economic growth
partial_erad_examples %>%
  filter(diseases != "Ageing-related") %>%
  filter(location == "Global", growth_transitions == FALSE) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~scenario+diseases, nrow = 3) +
  geom_line(aes(x = year, y = both), color = "black") + 
  geom_bar(aes(x = year, y = both, fill = "Complementarity"), stat = "identity") + 
  geom_bar(aes(x = year, y = mortality+disability, fill = "Mortality"), stat = "identity") + 
  geom_bar(aes(x = year, y = disability, fill = "Disability"), stat = "identity") + 
  labs(x = "Year", y = "Extra DALYs lived as a result of reduced prevalence (millions)", 
       fill = "Channel")
# if toggle growth transitions, change saving name
ggsave(here("output", "projections", "fig4_eradication25_nogr.jpg"),
       width = 10, height = 8)


# ----------------------------------------

# Ageing - including COVID --> will be main

partial_erad_examples %>%
  filter(diseases != "Ageing-related (no COVID)") %>%
  filter(location == "Global", growth_transitions == TRUE) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~scenario+diseases, nrow = 3) +
  geom_line(aes(x = year, y = both), color = "black") + 
  geom_bar(aes(x = year, y = both, fill = "Complementarity"), stat = "identity") + 
  geom_bar(aes(x = year, y = mortality+disability, fill = "Mortality"), stat = "identity") + 
  geom_bar(aes(x = year, y = disability, fill = "Disability"), stat = "identity") + 
  labs(x = "Year", y = "Extra DALYs lived as a result of reduced prevalence (millions)", 
       fill = "Channel")
# if toggling growth transitions to be FALSE, rename saving _gr to _nogr
ggsave(here("output", "covid_as_ageing", "fig4_c_eradication25_gr.jpg"),
       width = 10, height = 8)



# without economic growth
f4_reducing_prevalence_projection_data <- partial_erad_examples %>%
  filter(diseases != "Ageing-related (no COVID)") %>%
  filter(location == "Global", growth_transitions == FALSE)

write_csv(f4_reducing_prevalence_projection_data, here("output", "figure_underlying_data", "f4_reducing_prevalence_projection.csv"))

f4_reducing_prevalence_projection_plot <- f4_reducing_prevalence_projection_data %>%
  ggplot() + theme_bw() + 
  facet_wrap(~scenario+diseases, nrow = 3) +
  geom_line(aes(x = year, y = both), color = "black") + 
  geom_bar(aes(x = year, y = both, fill = "Complementarity"), stat = "identity") + 
  geom_bar(aes(x = year, y = mortality+disability, fill = "Mortality"), stat = "identity") + 
  geom_bar(aes(x = year, y = disability, fill = "Disability"), stat = "identity") + 
  labs(x = "Year", y = "Extra DALYs lived as a result of reduced prevalence (millions)", 
       fill = "Channel")
# if toggling growth transitions to be FALSE, rename saving _gr to _nogr
ggsave(here("output", "covid_as_ageing", "fig4_c_eradication25_nogr.jpg"),
       plot = f4_reducing_prevalence_projection_plot,
       width = 10, height = 8)

ggsave(
  here("output", "figures_pdfs", "f4_reducing_prevalence_projection.pdf"),
  plot = f4_reducing_prevalence_projection_plot +
    labs(title = "Figure 4: Global Health Gains from Reducing Disease Prevalence") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")),
  width = 8.27, height = 11.69, units = "in"
)



# ------------------------------------------------------------------------------

# Replicating for Figure 5 - increasing returns for reducing disease prevalence

# i.e. for income groups and different clusters, gains in world health measure w* from reductions in disease prevalence
# through the different channels: disability / mortality and both 


# Welfare and semi-elasticity

# Compute the welfare measures and semi-elasticities across eradication levels
elast_df <- W_scenarios_nocross %>%
  rename(eradication = erad1) %>%
  # Aggregate welfare over entire time horizon
  group_by(start_year, no_births, growth_transitions, diseases, type, location, eradication) %>%
  summarise(W = sum(W), Wnewborn = sum(Wnewborn)) %>%
  # Calculate baseline welfare (no eradication)
  group_by(start_year, no_births, growth_transitions, diseases, type, location) %>%
  mutate(W_start = sum((eradication == 0)*W),
         Wnewborn_start = sum((eradication == 0)*Wnewborn)) %>%
  # Calculate derivatives and elasticities
  group_by(start_year, no_births, growth_transitions, diseases, location, type) %>%
  arrange(eradication) %>%
  mutate(erad = 1 - eradication) %>% # Convert to disease prevalence
  # Calculate first derivatives (marginal effects)
  mutate(W_lag = lag(W, n = 1, order_by = eradication),
         Wnewborn_lag = lag(Wnewborn, n = 1, order_by = eradication),
         erad_lag = lag(erad, n = 1, order_by = eradication),
         erad_diff = erad - erad_lag) %>%
  mutate(dW_dd = (W - lag(W, n = 1, order_by = eradication))/erad_diff,
         dWnewborn_dd = (Wnewborn - lag(Wnewborn, n = 1, order_by = eradication))/erad_diff) %>%
  # Calculate second derivatives (curvature)
  mutate(d2W_dd2 = (lead(dW_dd, n=1, order_by = eradication) - dW_dd)/erad_diff^2,
         d2Wnewborn_dd2 =  (lead(dWnewborn_dd, n=1, order_by = eradication) - dWnewborn_dd)/erad_diff^2) %>%
  filter(eradication >= 0 ) %>%
  na.omit() %>%
  # Calculate elasticities (percentage effects)
  mutate(W_elast_1 = dW_dd*(1/W_lag),
         Wnewborn_elast_1 = dWnewborn_dd*(1/Wnewborn_lag),
         W_elast_2 = d2W_dd2*(1/W_lag),
         Wnewborn_elast_2 = d2Wnewborn_dd2*(1/Wnewborn_lag)) %>%
  # Calculate complementarity effects (difference between joint and sum of individual effects)
  group_by(start_year, no_births, growth_transitions, diseases, location, eradication) %>%
  mutate(W_sum = sum((type != "both")*W) - W_start,
         W_sum = case_when(type == "both" ~ W_sum, TRUE ~ W),
         Wnewborn_sum = sum((type != "both")*Wnewborn) - Wnewborn_start,
         Wnewborn_sum = case_when(type == "both" ~ Wnewborn_sum, TRUE ~ Wnewborn),
         # Complementarity in derivatives
         dW_dd_sum = sum((type != "both")*dW_dd),
         dW_dd_sum = case_when(type == "both" ~ dW_dd_sum, TRUE ~ dW_dd),
         dWnewborn_dd_sum = sum((type != "both")*dWnewborn_dd),
         dWnewborn_dd_sum = case_when(type == "both" ~ dWnewborn_dd_sum, TRUE ~ dWnewborn_dd),
         d2W_dd2_sum = sum((type != "both")*d2W_dd2),
         d2W_dd2_sum = case_when(type == "both" ~ d2W_dd2_sum, TRUE ~ d2W_dd2),
         # Complementarity in elasticities
         W_elast_1_sum = sum((type != "both")*W_elast_1),
         W_elast_1_sum = case_when(type == "both" ~ W_elast_1_sum, TRUE ~ W_elast_1),
         W_elast_2_sum = sum((type != "both")*W_elast_2),
         W_elast_2_sum = case_when(type == "both" ~ W_elast_2_sum, TRUE ~ W_elast_2)) %>%
  ungroup()



# Welfare - global measure, visualisations -------------------------------------

# Main paper figures (COVID as ageing-related)

f5_increasing_returns_data <- elast_df %>%
  filter(diseases != "Ageing-related (no COVID)") %>%
  filter(location %in% c("Global", "High Income", "Low Income")) %>% 
  filter(start_year == 2023, no_births == FALSE, growth_transitions == FALSE) %>%
  mutate(
    type = str_to_title(type),
    W_relative = W/W_start,
    W_sum_relative = W_sum/W_start
  )

write_csv(f5_increasing_returns_data, here("output", "figure_underlying_data", "f5_increasing_returns.csv"))

f5_increasing_returns_plot <- f5_increasing_returns_data %>%
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, nrow = 3) +
  geom_line(aes(y = W_relative, color = type)) +
  geom_ribbon(aes(ymax = W_relative, ymin = W_sum_relative, fill = type), alpha = 0.5) + 
  guides(fill="none") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Proportional Reduction in Disease Prevalence", y = "W*", color = "Effect", title = "No Economic Growth")

ggsave(here("output", "covid_as_ageing", "fig6_nogr.png"),
       plot = f5_increasing_returns_plot,
       width = 8.5, height = 4.5)

ggsave(
  here("output", "figures_pdfs", "f5_increasing_returns.pdf"),
  plot = f5_increasing_returns_plot +
    labs(
      title = "Figure 5: Increasing Returns from Reducing Disease Prevalence",
      subtitle = "No Economic Growth"
    ) +
    theme(plot.title = element_text(hjust = 0.5, face = "bold"),
          plot.subtitle = element_text(hjust = 0.5)),
  width = 8.27, height = 11.69, units = "in"
)


elast_df %>%
  filter(diseases != "Ageing-related (no COVID)") %>%
  filter(location %in% c("Global", "High Income", "Low Income")) %>% 
  filter(start_year == 2023, no_births == FALSE, growth_transitions == TRUE) %>%
  mutate(type = str_to_title(type)) %>%
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, nrow = 3) +
  geom_line(aes(y = W/W_start, color = type)) +
  geom_ribbon(aes(ymax = W/W_start, ymin = W_sum/W_start, fill = type), alpha = 0.5) + 
  guides(fill="none") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Proportional Reduction in Disease Prevalence", y = "W*", color = "Effect", title = "With Economic Growth")

ggsave(here("output", "covid_as_ageing", "fig6_gr.png"),
       width = 8.5, height = 4.5)

# ------------------------------------------
 
# Appendix - COVID not ageing-related

elast_df %>%
  filter(diseases != "Ageing-related") %>%
  filter(location %in% c("Global", "High Income", "Low Income")) %>% 
  filter(start_year == 2023, no_births == FALSE, growth_transitions == FALSE) %>%
  mutate(type = str_to_title(type)) %>%
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, nrow = 3) +
  geom_line(aes(y = W/W_start, color = type)) +
  geom_ribbon(aes(ymax = W/W_start, ymin = W_sum/W_start, fill = type), alpha = 0.5) + 
  guides(fill="none") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Proportional Reduction in Disease Prevalence", y = "W*", color = "Effect", title = "No Economic Growth")

ggsave(here("output", "projections", "appx_fig6_nogr.png"),
       width = 8.5, height = 4.5)


elast_df %>%
  filter(diseases != "Ageing-related") %>%
  filter(location %in% c("Global", "High Income", "Low Income")) %>% 
  filter(start_year == 2023, no_births == FALSE, growth_transitions == TRUE) %>%
  mutate(type = str_to_title(type)) %>%
  ggplot(aes(x = eradication)) + theme_bw() + 
  facet_wrap(~location+diseases, nrow = 3) +
  geom_line(aes(y = W/W_start, color = type)) +
  geom_ribbon(aes(ymax = W/W_start, ymin = W_sum/W_start, fill = type), alpha = 0.5) + 
  guides(fill="none") +   theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) + 
  labs(x = "Proportional Reduction in Disease Prevalence", y = "W*", color = "Effect", title = "With Economic Growth")

ggsave(here("output", "projections", "appx_fig6_gr.png"),
       width = 8.5, height = 4.5)


# complementary effect shaded --> banded


# ------------------------------------------------------------------------------

# Life expectancy effects of eradication
forecast_examples <- read_rds("output/forecast_examples.rds")

# Analyze remaining life expectancy effects of complete disease eradication
remLE_df <- forecast_examples %>%
  tibble() %>%
  filter(start_year == 2023, growth_transitions == TRUE, no_births == FALSE, year == 2023, type == "both",
         erad2 == 0, disease2 == "ageing") %>%
  select(-start_year, -growth_transitions, -no_births, -year, -type,, -erad2, -disease2, 
         -mortality, -mortality_proj_new, -disability_proj_new) %>%
  distinct(disease1, erad1, location, age, population, rem_LE, LE_birth)

# Calculate percentage increase in remaining life expectancy from disease eradication
remLE_df %>%
  mutate(location = "Global") %>%
  group_by(disease1, erad1,location,  age) %>%
  summarise(rem_LE = sum(population*rem_LE)/sum(population),
            LE_birth = sum(population*LE_birth)/sum(population),
            population = sum(population)) %>%
  ungroup() %>%
  rbind(remLE_df) %>%
  filter(age %in% c(0, 70)) %>%
  pivot_wider(id_cols = c(disease1, location, age), names_from = erad1, values_from = rem_LE) %>%
  mutate(rem_LE_perc_increase = 100*(`1`/`0`-1)) %>%
  pivot_wider(id_cols = c(location, age), names_from = disease1, values_from = rem_LE_perc_increase)


# ------------------------------------------------------------------------------

# Pure forecasting projections - showing ageing-related disease burden over time


# Forecasts without any reductions in prevalence

# Import comprehensive simulation results from demographic modeling
forecast_examples <- read_rds("output/forecast_examples.rds")

# Quick validation: check remaining life expectancy for specific scenarios
forecast_examples %>%
  filter(!no_births, start_year == 2023, year == 2023, age == 0) %>%
  select(-erad2, -disease2, -type) %>%
  distinct(disease1, erad1, location, rem_LE) %>%
  mutate(rem_le = rem_LE + 1) %>%
  filter(location == "High Income")

# Extract baseline forecasts (no interventions) for comparison
baseline_forecasts <- forecast_examples %>%
  select(-erad2, -disease2) %>%
  filter(!no_births, start_year == 2023, erad1 == 0, disease1 == "ageing") %>%
  select(-erad1, -disease1, -type) %>%
  distinct()
#distinct(start_year, no_births, growth_transitions, location, year, age)

# Validation check: ensure mortality components sum correctly
baseline_forecasts %>%
  mutate(x = mortality_infant_proj+mortality_adult_early_proj+mortality_adult_late_proj+mortality_ageing_proj) %>%
  select(location, year, age, population, mortality, mortality_proj, mortality_proj_new, x) %>%
  filter(age > 70)

# Visualize mortality decomposition for validation
baseline_forecasts %>%
  filter(year == 2023, growth_transitions == TRUE, location == "High Income") %>%
  #select(location, year, age, mortality, mortality_proj, mortality_ageing_proj) %>% filter(age > 50)
  ggplot() + 
  facet_wrap(~location) +
  geom_bar(aes(x = age, y = ((mortality_ageing_nocov_proj+mortality_infant_proj)/1e5)), stat = "identity", fill = "forestgreen") + 
  geom_bar(aes(x = age, y = (mortality_ageing_nocov_proj/1e5)), stat = "identity", fill = "red") + 
  geom_line(aes(x = age, y = mortality_proj/1e5)) + 
  coord_cartesian(ylim = c(0,1), xlim = c(0,100))



# DALY burden calculation and validation ----------------------------------


# first need to build the YLL converter:

# Years of life lost converter --------------------------------------------

# Create conversion factors from deaths to Years of Life Lost (YLLs)
# This accounts for remaining life expectancy at each age when someone dies
# But we don't have YLLs in the dataset themselves - so we need to get them from DALYs - YLDs

# Extract age conversion table for mapping between numeric ages and age group names
jlifetab_df <- jlifetab_df %>%
  rename(location = location_name) %>%
  mutate(
    location = case_when(
      location == "High Income" ~ "World Bank High Income",
      location == "Upper Middle Income" ~ "World Bank Upper Middle Income",
      location == "Lower Middle Income" ~ "World Bank Lower Middle Income",
      location == "Low Income" ~ "World Bank Low Income",
      TRUE ~ location
    )
  )

ages_conversion <- jlifetab_df %>%
  select(age, age_name) %>%
  distinct()

gbd_df_all <- readRDS(here("output-data", "gbd_global_all.rds"))

gbd_df_yll <- gbd_df_all %>%
  filter(year == 2023,
         measure_name %in% c("YLDs", "DALYs", "Deaths"),
         metric_name == "Number") %>%
  mutate(across(c(val, upper, lower), as.numeric)) %>%
  summarise(
    across(c(val, upper, lower), \(x) sum(x, na.rm = TRUE)),
    .by = c(location_name, year, cause_name, age_name, sex_name, metric_name, measure_name)
  ) %>%
  pivot_wider(
    names_from  = measure_name,
    values_from = c(val, upper, lower),
    names_sep   = "_"
  ) %>%
  mutate(
    val_YLLs   = val_DALYs   - val_YLDs,
    # intervals can go negative; clamp if you want non-negative YLL bounds:
    upper_YLLs = pmax(0, upper_DALYs - upper_YLDs),
    lower_YLLs = pmax(0, lower_DALYs - lower_YLDs)
  ) %>%
  pivot_longer(
    cols      = matches("^(val|upper|lower)_"),
    names_to  = c(".value", "measure_name"),
    names_sep = "_"
  )

yll_converter_df <- gbd_df_yll %>%
  filter(year == 2023, measure_name %in% c("Deaths", "YLLs", "YLDs"), metric_name == "Number") %>%
  pivot_wider(id_cols = c(location_name, cause_name, age_name), names_from = measure_name, values_from = val) %>% 
  left_join(clusters_df_2023) %>%
  # Aggregate by disease cluster and demographic group
  group_by(location_name, age_name, cluster) %>%
  summarise(YLLs = sum(YLLs, na.rm = T), Deaths = sum(Deaths, na.rm = T), YLDs = sum(YLDs, na.rm = T)) 

# Combine COVID and non-COVID aging-related diseases for some analyses
yll_converter_df <- yll_converter_df %>%
  filter(cluster %in% c("Ageing-related (no COVID)", "COVID-related")) %>%
  mutate(cluster = "Ageing-related") %>%
  group_by(location_name, age_name, cluster) %>%
  summarise(YLLs = sum(YLLs, na.rm = T), Deaths = sum(Deaths, na.rm = T), YLDs = sum(YLDs, na.rm = T)) %>%
  rbind(yll_converter_df) %>%
  ungroup() %>%
  
  # Calculate YLL per death ratio (accounts for age-specific life expectancy)
  mutate(yll_converter = YLLs/Deaths) %>%
  rename(location = location_name) %>%
  
  # Adjust for age group widths (some age groups span multiple years)
  mutate(age_adjuster = case_when(age_name == "0-1 years" ~ 1, 
                                  age_name == "1-2 years" ~ 1, 
                                  age_name == "2-4 years" ~ 3, 
                                  TRUE ~ 5)) %>%
  
  # Expand to single-year ages for modeling
  left_join(crossing(ages_conversion, cluster = cluster_groups, location = income_groups)) %>%
  mutate(YLLs = YLLs/age_adjuster, 
         Deaths = Deaths/age_adjuster, 
         YLDs = YLDs/age_adjuster) %>%
  mutate(cluster = factor(cluster, ordered = T, levels = cluster_groups))


# Visualize YLL converter patterns by age
yll_converter_df%>%
  filter(location == "Global", cluster == "Infant") %>%
  ggplot() + theme_bw() + 
  geom_line(aes(x = age, y = yll_converter))

# Check specific age points for infant diseases
yll_converter_df%>%
  filter(location == "Global", cluster == "Infant", age %in% c(0, 30, 60, 90)) 

beep()


strip_wb <- function(x) stringr::str_remove(x, "^World Bank ")

jlifetab_df <- readRDS(here("output-data", "country_lifetab_data.rds")) %>%
  dplyr::rename(location = location_name) %>%
  dplyr::mutate(location = strip_wb(location))

max_age <- 100 
yll_converter_df <- yll_converter_df %>%
  mutate(location = strip_wb(location)) %>%
  mutate(
    age_min = as.integer(str_extract(age_name, "^\\d+")),
    age_max = if_else(
      str_detect(age_name, "\\+"),
      max_age,
      as.integer(str_extract(age_name, "(?<=-)\\d+"))
    ),
    age = map2(age_min, age_max, ~ seq(.x, .y))
  ) %>%
  unnest(age) %>%
  select(-age_min, -age_max)


# Convert simulation results to DALY burden estimates compatible with GBD data
baseline_dalys <- baseline_forecasts %>%
  select(location, growth_transitions, year, age, population, mortality_proj, disability_proj, rem_LE, 
         mortality_infant_proj, mortality_adult_early_proj, mortality_adult_late_proj, 
         mortality_ageing_proj, mortality_ageing_nocov_proj,
         disability_infant_proj, disability_adult_early_proj, disability_adult_late_proj, 
         disability_ageing_proj, disability_ageing_nocov_proj) %>%
  # Handle extreme ages where rates may exceed 100% (model artifacts)
  # If mortality proj is greater than 1e5, then hold everything constant at last legit value
  mutate(mortality_proj = case_when(mortality_proj > 1e5 ~ NA_real_, TRUE ~ mortality_proj),
         disability_proj = case_when(disability_proj > 1e5 ~ NA_real_, TRUE ~ disability_proj),
         # Propagate missing values to component rates
         mortality_infant_proj = case_when(is.na(mortality_proj) ~ NA_real_, TRUE ~ mortality_infant_proj), 
         mortality_adult_early_proj = case_when(is.na(mortality_proj) ~ NA_real_, TRUE ~ mortality_adult_early_proj), 
         mortality_adult_late_proj = case_when(is.na(mortality_proj) ~ NA_real_, TRUE ~ mortality_adult_late_proj), 
         mortality_ageing_proj = case_when(is.na(mortality_proj) ~ NA_real_, TRUE ~ mortality_ageing_proj), 
         mortality_ageing_nocov_proj = case_when(is.na(mortality_proj) ~ NA_real_, TRUE ~ mortality_ageing_nocov_proj),
         # Calculate COVID component as difference
         mortality_covid_proj = mortality_ageing_proj - mortality_ageing_nocov_proj,
         # Apply same logic to disability rates
         disability_infant_proj = case_when(is.na(disability_proj) ~ NA_real_, TRUE ~ disability_infant_proj), 
         disability_adult_early_proj = case_when(is.na(disability_proj) ~ NA_real_, TRUE ~ disability_adult_early_proj), 
         disability_adult_late_proj = case_when(is.na(disability_proj) ~ NA_real_, TRUE ~ disability_adult_late_proj), 
         disability_ageing_proj = case_when(is.na(disability_proj) ~ NA_real_, TRUE ~ disability_ageing_proj),
         disability_ageing_nocov_proj = case_when(is.na(disability_proj) ~ NA_real_, TRUE ~ disability_ageing_nocov_proj),
         disability_covid_proj = disability_ageing_proj - disability_ageing_nocov_proj) %>%
  # Reshape to long format for disease-specific calculations
  pivot_longer(cols = c(mortality_infant_proj, mortality_adult_early_proj, mortality_adult_late_proj, 
                        mortality_ageing_proj, mortality_ageing_nocov_proj, mortality_covid_proj,
                        disability_infant_proj, disability_adult_early_proj, disability_adult_late_proj, 
                        disability_ageing_proj, disability_ageing_nocov_proj, disability_covid_proj)) %>%
  # Forward fill missing values to handle extreme ages
  group_by(location, growth_transitions, year, name) %>%
  arrange(location, growth_transitions, year, name, age) %>%
  fill(mortality_proj, disability_proj, value, .direction = "down") %>%
  ungroup() %>%
  arrange(location, growth_transitions, year, age, name) %>%
  # Create disease cluster categories
  mutate(cluster = case_when(str_detect(name, "infant") ~ "Infant",
                             str_detect(name, "adult_early") ~ "Adult (early)",
                             str_detect(name, "adult_late") ~ "Adult (late)",
                             str_detect(name, "nocov") ~ "Ageing-related (no COVID)",
                             str_detect(name, "ageing") ~ "Ageing-related",
                             str_detect(name, "covid") ~ "COVID-related"),
         channel = case_when(str_detect(name, "mortality") ~ "mortality_effect",
                             str_detect(name, "disability") ~ "disability_effect"),
         # Convert to proportions
         mortality_proj = mortality_proj/1e5,
         disability_proj = disability_proj/1e5,
         value = value/1e5) %>%
  # Reshape to separate mortality and disability effects
  pivot_wider(id_cols = c(location, growth_transitions, year, age, population, mortality_proj, disability_proj, rem_LE, 
                          cluster), names_from = channel, values_from = value) %>%
  # Add remaining life expectancy and YLL conversion factors
  left_join(select(jlifetab_df, location, age, year, remaining_le)) %>%
  left_join(select(yll_converter_df, -age_name)) %>%
  #left_join(mortality_check_df) %>%
  # Handle missing values with reasonable defaults
  mutate(remaining_le = replace_na(remaining_le, 1)) %>%
  mutate(yll_converter = replace_na(yll_converter, 1)) %>%
  # Calculate health burden measures
  mutate(deaths = population*mortality_effect) %>%
  group_by(location, growth_transitions, year, age) %>%
  #mutate(Deaths_adj = Deaths/age_adjuster, YLLs_adj = YLLs/age_adjuster, YLDs_adj = YLDs/age_adjuster) %>%
  mutate(ylds = population*disability_effect, 
         ylls = deaths*yll_converter,
         dalys = ylds + ylls) %>%
  ungroup() 


# ----------------------------------------------

# Compute burden over time in the forecast

# Aggregate disease burden by region, growth scenario, and time
baseline_dalys_overtime <- baseline_dalys %>%
  rbind(mutate(baseline_dalys, location = "Global")) %>% # Add global totals
  mutate(growth_transitions = case_when(growth_transitions ~ "Economic Growth", 
                                        TRUE ~ "No Economic Growth")) %>%
  mutate(location = factor(location, ordered = T, levels = income_groups)) %>%
  group_by(location, growth_transitions, year, cluster) %>%
  summarise(ylds = sum(ylds), ylls = sum(ylls), dalys = sum(dalys), 
            YLDs = sum(YLDs, na.rm = T), YLLs = sum(YLLs, na.rm = T),
            #ylls_new = sum(ylls_new), dalys_new = sum(dalys_new),
            #ylls_wpp = sum(ylls_wpp), dalys_wpp = sum(dalys_wpp),
            population = sum(population)) %>%
  mutate(DALYs = YLDs + YLLs)

# Plot predicted DALY burden over time
baseline_dalys_overtime %>%
  filter(!(cluster %in% c("Ageing-related (no COVID)", "COVID-related"))) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~growth_transitions+location, nrow = 2) + 
  geom_line(aes(x = year, y = dalys, color = cluster)) +
  scale_color_manual(values = cluster_cols) +
  labs(x = "Year", y = "Expected DALY burden", color = "Disease cluster")
# Save
ggsave(here("output", "forward_projs_growth.png"),
       width = 10, height = 5)


# Projections table for paper
baseline_dalys_overtime %>%
  filter(!(cluster %in% c("Ageing-related (no COVID)", "COVID-related"))) %>%
  mutate(year = as.character(year)) %>%
  # Add time period aggregations
  rbind(mutate(filter(baseline_dalys_overtime, year %in% 2023:2032, 
                      !(cluster %in% c("Ageing-related (no COVID)", "COVID-related"))), year = "Next 10 Years")) %>%
  rbind(mutate(filter(baseline_dalys_overtime, year %in% 2023:2072, 
                      !(cluster %in% c("Ageing-related (no COVID)", "COVID-related"))), year = "Next 50")) %>%
  rbind(mutate(filter(baseline_dalys_overtime, year %in% 2023:2122, 
                      !(cluster %in% c("Ageing-related (no COVID)", "COVID-related"))), year = "Next 100")) %>%
  group_by(location, growth_transitions, year, cluster) %>%
  summarise(dalys = sum(dalys)) %>%
  group_by(location, growth_transitions, year) %>%
  mutate(dalys_perc = sprintf(100*dalys/sum(dalys), fmt = '%#.1f')) %>%
  filter(cluster == "Ageing-related") %>%
  filter(year %in% c("2023", "2050", "2075", "2100", "2125", "Next 10 Years", "Next 50", "Next 100")) %>%
  pivot_wider(id_cols = c(location, growth_transitions), names_from = year, values_from = dalys_perc) %>%
  arrange(growth_transitions, location)


# Similar table for aging-related diseases excluding COVID
baseline_dalys_overtime %>%
  filter(!(cluster %in% c("Ageing-related"))) %>%
  mutate(year = as.character(year)) %>%
  rbind(mutate(filter(baseline_dalys_overtime, year %in% 2023:2032, 
                      !(cluster %in% c("Ageing-related"))), year = "Next 10 Years")) %>%
  rbind(mutate(filter(baseline_dalys_overtime, year %in% 2023:2072, 
                      !(cluster %in% c("Ageing-related"))), year = "Next 50")) %>%
  rbind(mutate(filter(baseline_dalys_overtime, year %in% 2023:2122, 
                      !(cluster %in% c("Ageing-related"))), year = "Next 100")) %>%
  group_by(location, growth_transitions, year, cluster) %>%
  summarise(dalys = sum(dalys)) %>%
  group_by(location, growth_transitions, year) %>%
  mutate(dalys_perc = sprintf(100*dalys/sum(dalys), fmt = '%#.1f')) %>%
  filter(cluster == "Ageing-related (no COVID)") %>%
  filter(year %in% c("2023", "2050", "2075", "2100", "2125", "Next 10 Years", "Next 50", "Next 100")) %>%
  pivot_wider(id_cols = c(location, growth_transitions), names_from = year, values_from = dalys_perc) %>%
  arrange(growth_transitions, location)


# ------------------------------------------------------------------------------
# --- Now we need the same stats for 1990 - what was the ageing-related disease burden


# will match 2023 clustering to 1990 disease data - using DALY burden directly 

gbd_df_yll90 <- gbd_df_all %>%
  filter(year == 1990,
         measure_name %in% c("YLDs", "DALYs", "Deaths"),
         metric_name == "Number") %>%
  mutate(across(c(val, upper, lower), as.numeric)) %>%
  summarise(
    across(c(val, upper, lower), \(x) sum(x, na.rm = TRUE)),
    .by = c(location_name, year, cause_name, age_name, sex_name, metric_name, measure_name)
  ) %>%
  pivot_wider(
    names_from  = measure_name,
    values_from = c(val, upper, lower),
    names_sep   = "_"
  ) %>%
  mutate(
    val_YLLs   = val_DALYs   - val_YLDs,
    # intervals can go negative; clamp if you want non-negative YLL bounds:
    upper_YLLs = pmax(0, upper_DALYs - upper_YLDs),
    lower_YLLs = pmax(0, lower_DALYs - lower_YLDs)
  ) %>%
  pivot_longer(
    cols      = matches("^(val|upper|lower)_"),
    names_to  = c(".value", "measure_name"),
    names_sep = "_"
  )

gbd_df90_daly <- gbd_df_yll90 %>%
  left_join(clusters_df_2023) %>%
  filter(measure_name == "DALYs") %>%
  group_by(location_name, cluster) %>%
  summarise(total_daly_cluster = sum(val), .groups = "drop") %>%
  group_by(location_name) %>%
  mutate(total_daly = sum(total_daly_cluster), .groups = "drop") %>%
  mutate(cluster_percent = (total_daly_cluster / total_daly) * 100)




# Binding baseline DALYs overtime and 1990s DALYs for Andrew 
# i.e. a spreadsheet with the 4 disease categories and lists percentage of disease burden for each

dalys23 <- baseline_dalys_overtime %>%
  filter(!(cluster %in% c("Ageing-related (no COVID)", "COVID-related"))) %>%
  mutate(year = as.character(year)) %>%
  group_by(location, growth_transitions, year, cluster) %>%
  summarise(dalys = sum(dalys)) %>%
  group_by(location, growth_transitions, year) %>%
  mutate(dalys_perc = sprintf(100*dalys/sum(dalys), fmt = '%#.1f')) %>%
  filter(year %in% c("2023", "2050", "2075", "2100", "2125", "Next 10 Years")) %>%
  pivot_wider(id_cols = c(location, growth_transitions, cluster), names_from = year, values_from = dalys_perc) %>%
  arrange(growth_transitions, location)

# adding 1990 as a column to this
dalys90 <- gbd_df90_daly %>%
  filter(!(cluster %in% c("COVID-related"))) %>%
  mutate(
    cluster = recode(
      cluster,
      "Ageing-related (no COVID)" = "Ageing-related"
    )
  ) %>%
  select(location_name, cluster, cluster_percent) %>%
  mutate(cluster_percent = round(cluster_percent, 1)) %>%
  rename("1990" = cluster_percent)

dalys90_grwth <- bind_rows(
  dalys90 %>% mutate(growth_transitions = "Economic Growth"),
  dalys90 %>% mutate(growth_transitions = "No Economic Growth")
  ) %>%
  rename(location = location_name) %>%
  mutate(location = strip_wb(location))

table2 <- dalys23 %>%
  left_join(dalys90_grwth) %>%
  relocate(`1990`, .before = `2023`)

write.csv(table2, here("output", "andrew_table2_data.csv"), row.names = FALSE)

# ------------------------------------------------------------------------------

# Life expectancy analysis


# Analyze remaining life expectancy effects of complete disease eradication
remLE_df <- forecast_examples %>%
  tibble() %>%
  filter(start_year == 2023, growth_transitions == TRUE, no_births == FALSE, year == 2023, type == "both",
         erad2 == 0, disease2 == "ageing") %>%
  select(-start_year, -growth_transitions, -no_births, -year, -type,, -erad2, -disease2, 
         -mortality, -mortality_proj_new, -disability_proj_new) %>%
  distinct(disease1, erad1, location, age, population, rem_LE, LE_birth)

# Calculate percentage increase in remaining life expectancy from disease eradication
remLE_df %>%
  mutate(location = "Global") %>%
  group_by(disease1, erad1,location,  age) %>%
  summarise(rem_LE = sum(population*rem_LE)/sum(population),
            LE_birth = sum(population*LE_birth)/sum(population),
            population = sum(population)) %>%
  ungroup() %>%
  rbind(remLE_df) %>%
  filter(age %in% c(0, 70)) %>%
  pivot_wider(id_cols = c(disease1, location, age), names_from = erad1, values_from = rem_LE) %>%
  mutate(rem_LE_perc_increase = 100*(`1`/`0`-1)) %>%
  pivot_wider(id_cols = c(location, age), names_from = disease1, values_from = rem_LE_perc_increase)



# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------









