# Title: 3.static_analysis_9023.R
# Description: Creates summary lifetable statistics for income groups (Table 1),
#              reproduces Figures 2 (mortality and disability burden by cluster and age 2023), 3 (expected lifetime burden), 6 (heatmap of burden by cluster and income group)
#              Creates the mortality and disability values by cluster for 1990 and 2023 for all locations (aggregates mortality-disability data frames into epidemiological clusters)
#              Exports medium scenarios - csv files inputted into 4. --> feed through to scenarios and projections
#
# ------------------------------------------------------------------------------

rm(list=ls())

library(here)
here::i_am("code/3. static_analysis_9023.R")
here::dr_here()

library(tidyverse)
library(readxl)
library(janitor)
library(stargazer)

dir.create(here("output", "figures_pdfs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figure_underlying_data"), recursive = TRUE, showWarnings = FALSE)

gbd_df_all <- readRDS(here("output-data", "gbd_global_all.rds"))
gbd_dalys <- readRDS(here("output-data", "gbd_dalys.rds"))
all_causes <- readRDS(here("output-data", "gbd_data_causes.rds"))

gbd_deaths_df <- readRDS(here("output-data", "gbd_deaths.rds"))
gbd_ylds_df   <- readRDS(here("output-data", "gbd_ylds.rds"))


# Life table data (age-specific population, mortality, survival, remaining LE)
jlifetab_df <- readRDS(here("output-data", "country_lifetab_data.rds"))

clusters_df_2023 <- readRDS(here("output-data", "cluster4_assignment_2023.rds"))  

cluster_map23 <- clusters_df_2023 %>%
  distinct(cause_name, cluster_4)

# ------------------------------------------------------------------------------

# Reproducing Table 1 - want 1973 and 2023 averages by income group, consistent with 2023-defined income groups

# World Bank income groups in ordered form
income_groups <- c("Global", "High Income", "Upper Middle Income", 
                   "Lower Middle Income", "Low Income")


# data frame for income group assignment of countries in 2023 (using data calendar year)
wb_income_class23_df <- read_xlsx(here("raw-data", "world_bank_country_income_class.xlsx"),
  sheet = "Country Analytical History",
  skip = 5,               
  .name_repair = "unique"   
) %>%
  filter(!if_all(everything(), ~ is.na(.) | trimws(as.character(.)) == "")) %>%
  select(1:2, any_of(c("2023"))) %>%
  rename(location_name = "Data for calendar year :")
  

# Merging 2023 income groups to lifetable

# changing some of the country names for income group classification to be consistent with 
wb_income_class23_df <- wb_income_class23_df %>%
  mutate(location_name = case_when(str_detect(location_name, "Bahamas, The") ~ "Bahamas",
         str_detect(location_name, "Bolivia") ~ "Bolivia (Plurinational State of)",
         str_detect(location_name, "Congo, Dem. Rep.") ~ "Democratic Republic of the Congo",
         str_detect(location_name, "Congo, Rep.") ~ "Congo",
         str_detect(location_name, "Korea, Dem. Rep.") ~ "Democratic People's Republic of Korea",
         str_detect(location_name, "Egypt, Arab Rep.") ~ "Egypt",
         str_detect(location_name, "Gambia, The") ~ "Gambia",
         str_detect(location_name, "Iran, Islamic Rep.") ~ "Iran (Islamic Republic of)",
         str_detect(location_name, "Kyrgyz Republic") ~ "Kyrgyzstan",
         str_detect(location_name, "Lao PDR") ~ "Lao People's Democratic Republic",
         str_detect(location_name, "Micronesia, Fed. Sts.") ~ "Micronesia (Federal States of)",
         str_detect(location_name, "Korea, Rep.") ~ "Republic of Korea",
         str_detect(location_name, "Moldova") ~ "Republic of Moldova",
         str_detect(location_name, "St. Kitts and Nevis") ~ "Saint Kitts and Nevis",
         
         str_detect(location_name, "St. Lucia") ~ "Saint Lucia",
         str_detect(location_name, "São Tomé and Príncipe") ~ "Sao Tome and Principe",
         str_detect(location_name, "Slovak Republic") ~ "Slovakia",
         str_detect(location_name, "Somalia, Fed. Rep.") ~ "Somalia",
         str_detect(location_name, "Taiwan, China") ~ "Taiwan (Province of China)",
         str_detect(location_name, "Türkiye") ~ "Turkey",
         str_detect(location_name, "Tanzania") ~ "United Republic of Tanzania",
         str_detect(location_name, "Virgin Islands (U.S.)") ~ "United States Virgin Islands",
         str_detect(location_name, "United States") ~ "United States of America",
         str_detect(location_name, "Venezuela, RB.") ~ "Venezuela (Bolivarian Republic of)",
         str_detect(location_name, "Yemen, Rep.") ~ "Yemen",
         
         str_detect(location_name, "Yemen, Rep.") ~ "Yemen",
         str_detect(location_name, "Yemen, Rep.") ~ "Yemen",
         str_detect(location_name, "Yemen, Rep.") ~ "Yemen",
         str_detect(location_name, "Yemen, Rep.") ~ "Yemen",
         TRUE ~ location_name)) %>%
  rename(income_group23 = "2023")     

lifetab_incgroup_df <- jlifetab_df %>%
  left_join((wb_income_class23_df), by = c("location_name")) %>%
  filter(type == "Country/Area")    
      
         
# want to see how much of population in 2023 is in each income group
popshare_23 <- lifetab_incgroup_df %>%
  filter(year == 2023) %>%
  group_by(income_group23) %>%
  summarise(pop_2023 = sum(population, na.rm = TRUE), .groups = "drop") %>%
  mutate(world_pop_2023 = sum(pop_2023),
         pct_world = 100 * pop_2023 / world_pop_2023) %>%
  arrange(desc(pct_world))


# Then, by income group and year, compute age-specific averages, weighted by age populations for each country 
# Outcome: weighted averages for each income group by each age for years 1973 and 2023

# Sum of population for each age in each income group - stick to age instead of age_name -> will want individual ages for survival rates
lifetab_incgroup73_df <- lifetab_incgroup_df %>%
  filter(year %in% c(1973, 2023)) %>%
  group_by(year, age, income_group23) %>%
  mutate(pop_sum_age = sum(population, na.rm = TRUE),
         pop_weighting = if_else(
           pop_sum_age > 0,                          
           population / pop_sum_age,
           NA_real_
         )
  ) %>%
  ungroup()

sum_lifetab_incgroup_df <- lifetab_incgroup73_df %>%
  group_by(year, income_group23, age) %>%
  summarise(
    mortality_mean = weighted.mean(mortality, w = pop_weighting, na.rm = TRUE), 
    survival_mean = weighted.mean(survival, w = pop_weighting, na.rm = TRUE),
    remaining_le_mean = weighted.mean(remaining_le, w = pop_weighting, na.rm = TRUE), 
    .groups = "drop"
  )
    
# survival rates for ages 65 to 80 --> probability of survival to 80, conditional on being alive at 65
surv_65_80 <- sum_lifetab_incgroup_df %>%
  group_by(year, income_group23) %>%   # adjust to your grouping vars
  summarise(
    surv_65_80 = survival_mean[age == 80] / survival_mean[age == 65],
    .groups = "drop"
  )



# seeing if gone down for any:
le_change <- lifetab_incgroup_df %>%
  filter(age == 0) %>%
  select(location_name, year, remaining_le) %>%   # change var name if yours differs
  distinct() %>%
  pivot_wider(names_from = year, values_from = remaining_le) %>%
  mutate(
    change = `2023` - `1973`,
    going_down = change < 0
  ) %>%
  arrange("2023")

# Countries where life expectancy went down
down_countries <- le_change %>%
  filter(going_down) %>%
  arrange(change)

le_change %>% 
  filter(`2023` > 80) %>%
  distinct(location_name) %>%
  summarise(n_locations = n())  
  

# ------------------------------------------------------------------------------

# Ratio of Life Expectancy to Healthy Life Expectancy - refer to 6.forecast file


# Other introduction / abstract stats:

# proportion of population aged over 65:
over65 <- jlifetab_df %>%
  filter(year %in% c(1973, 2023)) %>%
  filter(location_name == "Global") %>%
    mutate(over65 = case_when(age >= 65 ~ "65+", TRUE ~ "<65")) %>%
    group_by(year, location_name) %>%
    summarise(
      pop_total = sum(population, na.rm = TRUE),
      pop_65plus = sum(population[age >= 65], na.rm = TRUE),
      prop_65plus = pop_65plus / pop_total,
      .groups = "drop"
    )
  
over65


gbd_causes <- readRDS(here("output-data", "gbd_data_causes.rds"))


# ------------------------------------------------------------------------------

# GBD stats mentioned in paper 

# Global burden for measles - share of total burden to each disease

# Only location in this dataset is global - 1 location (no countries)

# total dalys lost in year, then total dalys lost by cause in a year, proportion from each cause
gbd_daly_percent <- gbd_dalys %>%
  filter(metric_name == "Number") %>%
  select(location_name, year, cause_name, age_name, sex_name, measure_name, metric_name, val) %>%
  group_by(year) %>%
  mutate(
    sum_daly_lost = sum(val, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(year, cause_name) %>%
  mutate(
    sum_daly_cause = sum(val, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(percent_dalys_cause = (sum_daly_cause / sum_daly_lost) * 100)



# ------------------------------------------------------------------------------

# Reproducing Figure 2

# figure 2: mortality and disability burden by cluster. Mortality and disability rates for disease clusters using GBD
# Taken directly from global age-specific deaths and years lost to disability for each cause provided by UN WPP:
# - expressed as rate of deaths / YLDs per person due to each cause.
# cluster level rates: sum of rates across all causes assigned to that cluster.


# Expand (cross join)the lifetable across all causes 
# so we can weight GBD rates by population/survival
jlifetab_expanded_df <- jlifetab_df %>%
  filter(year == 2023) %>%
  cross_join(all_causes) 


# Definitions for consistent ordering

age_groups <- c("0-1 years", "1-2 years", "2-4 years", "5-9 years", "10-14 years", "15-19 years", 
                "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", 
                "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", 
                "80-84 years", "85-89 years", "90-94 years", "95-99 years")
cluster_groups <- c("Ageing-related", "COVID-related", "Adult (late)", 
                    "Adult (early)", "Infant")


# Figure 2: Decompose mortality and disability by cluster ----------------------
# (2023; stacked bars by age groups)


# Mortality
global_mort_df <- gbd_df_all %>%
  filter(measure_name == "Deaths") %>%
  filter(metric_name == "Rate") %>%
  filter(location_name == "Global") %>%
  # Join age-structured populations (2023) for weighting
  left_join(filter(jlifetab_expanded_df, year %in% c(2023)),
            by = c("location_name", "year", "age_name", "cause_name")) %>%
  group_by(year, age_name, cause_name) %>%
  # Population-weighted average rate per 100k
  summarise(val = sum(population * val) / sum(population) / 1e5, .groups = "drop") %>%
  # Add clusters - tagging COVID
  left_join(distinct(clusters_df_2023, cause_name, cluster_4), by = "cause_name") %>%
  mutate(cluster_4 = case_when(
    str_detect(cause_name, "COVID") ~ "COVID-related",
    TRUE                            ~ cluster_4
  )) %>%
  group_by(year, age_name, cluster_4) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age     = factor(str_remove(age_name, " years"), ordered = TRUE,
                     levels = str_remove(age_groups, " years")),
    measure = "Mortality"
  )

# Disability
global_disab_df <- gbd_df_all %>%
  filter(measure_name == "YLDs") %>%
  filter(metric_name == "Rate") %>%
  filter(location_name == "Global") %>%
  left_join(filter(jlifetab_expanded_df, year %in% c(2023)),
            by = c("location_name", "year", "age_name", "cause_name")) %>%
  group_by(year, age_name, cause_name) %>%
  summarise(val = sum(population * val) / sum(population) / 1e5, .groups = "drop") %>%
  left_join(distinct(clusters_df_2023, cause_name, cluster_4), by = "cause_name") %>%
  mutate(cluster_4 = case_when(
    str_detect(cause_name, "COVID") ~ "COVID-related",
    TRUE                            ~ cluster_4
  )) %>%
  group_by(year, age_name, cluster_4) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age     = factor(str_remove(age_name, " years"), ordered = TRUE,
                     levels = str_remove(age_groups, " years")),
    measure = "Disability"
  )

# Figure for Appendix - COVID separate 

# 2023 snapshot only
appendix_mdrates_clusters4_data <- global_mort_df %>%
  rbind(global_disab_df) %>%
  filter(year == 2023) %>%
  mutate(
    measure    = factor(measure, levels = c("Mortality", "Disability"), ordered = TRUE),
    cluster_4  = factor(cluster_4, levels = cluster_groups, ordered = TRUE)
  )

appendix_mdrates_clusters4_plot <- appendix_mdrates_clusters4_data %>%
  ggplot(aes(x = age, y = val)) + theme_bw() + 
  facet_wrap(~ measure) +
  geom_bar(aes(fill = cluster_4), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c(
    "Ageing-related" = "firebrick1",
    "COVID-related"  = "orange",
    "Adult (late)"   = "blue3", 
    "Adult (early)"  = "cornflowerblue",
    "Infant"         = "forestgreen"
  )) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(y = "Rate", x = "Age", fill = "Disease cluster")

ggsave( #
  filename = here("output", "global_mdrates_clusters4_2023.jpg"),          
  plot = appendix_mdrates_clusters4_plot,
  width = 10, height = 4, units = "in",     
  dpi = 300                                     
)



# --- Main Figure 2: COVID is kept as an ageing-related disease ------

# Figure 2: Decompose mortality and disability by cluster ----------------------
# (2023; stacked bars by age groups)


# Mortality
global_mort_df <- gbd_df_all %>%
  filter(measure_name == "Deaths") %>%
  filter(metric_name == "Rate") %>%
  filter(location_name == "Global") %>%
  # Join age-structured populations (2023) for weighting
  left_join(filter(jlifetab_expanded_df, year %in% c(2023)),
            by = c("location_name", "year", "age_name", "cause_name")) %>%
  group_by(year, age_name, cause_name) %>%
  # Population-weighted average rate per 100k
  summarise(val = sum(population * val) / sum(population) / 1e5, .groups = "drop") %>%
  # Add clusters - this time leaving COVID as an Ageing-related disease
  left_join(distinct(clusters_df_2023, cause_name, cluster_4), by = "cause_name") %>%
  group_by(year, age_name, cluster_4) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age     = factor(str_remove(age_name, " years"), ordered = TRUE,
                     levels = str_remove(age_groups, " years")),
    measure = "Mortality"
  )

# Disability
global_disab_df <- gbd_df_all %>%
  filter(measure_name == "YLDs") %>%
  filter(metric_name == "Rate") %>%
  filter(location_name == "Global") %>%
  left_join(filter(jlifetab_expanded_df, year %in% c(2023)),
            by = c("location_name", "year", "age_name", "cause_name")) %>%
  group_by(year, age_name, cause_name) %>%
  summarise(val = sum(population * val) / sum(population) / 1e5, .groups = "drop") %>%
  left_join(distinct(clusters_df_2023, cause_name, cluster_4), by = "cause_name") %>%
  group_by(year, age_name, cluster_4) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age     = factor(str_remove(age_name, " years"), ordered = TRUE,
                     levels = str_remove(age_groups, " years")),
    measure = "Disability"
  )


# 2023 snapshot only
f2_burden_age_cluster_data <- global_mort_df %>%
  rbind(global_disab_df) %>%
  filter(year == 2023) %>%
  mutate(
    measure    = factor(measure, levels = c("Mortality", "Disability"), ordered = TRUE),
    cluster_4  = factor(cluster_4, levels = cluster_groups, ordered = TRUE)
  )

write_csv(
  f2_burden_age_cluster_data %>%
    mutate(age = paste0(age, " years")),
  here("output", "figure_underlying_data", "f2_burden_age_cluster.csv")
)

f2_burden_age_cluster_plot <- f2_burden_age_cluster_data %>%
  ggplot(aes(x = age, y = val)) + theme_bw() + 
  facet_wrap(~ measure) +
  geom_bar(aes(fill = cluster_4), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c(
    "Ageing-related" = "firebrick1",
    "Adult (late)"   = "blue3", 
    "Adult (early)"  = "cornflowerblue",
    "Infant"         = "forestgreen"
  )) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(y = "Rate", x = "Age", fill = "Disease cluster")

ggsave( #
  filename = here("output", "covid_as_ageing", "c_figure2.jpg"),          
  plot = f2_burden_age_cluster_plot,
  width = 10, height = 4, units = "in",     
  dpi = 300                                     
)

ggsave(
  filename = here("output", "figures_pdfs", "f2_burden_age_cluster.pdf"),
  plot = f2_burden_age_cluster_plot +
    labs(title = "Figure 2: Mortality and Disability Burden by Cluster, 2023") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")),
  width = 8.27, height = 11.69, units = "in"
)



# ------------------------------------------------------------------------------


# Reproducing Figure 3: Exp YLD and YLL for newborn in 2023


# Figure 3: calculate mean survival probabilities and remaining life expectancy means for each for the income groups
#          (weighting each country by new-born population/total newborn population in the income group)
#          merge to income-level GBD dataset and sum death/disability rates across all ages for each cause, weighting by survival probability and RLE
#          then aggregate up to cluster-level


# now take the population at age 0 for each country to generate the weightings 
# then get to mean remaining life expectancy and survival probability by income group


# computing newborn weights by country within each income group for 2023:
newborn_wts_23 <- lifetab_incgroup_df %>%
  filter(year == 2023, age == 0) %>%
  group_by(year, income_group23, location_name) %>%
  summarise(newborn_pop = sum(population, na.rm = TRUE), .groups = "drop") %>%
  group_by(year, income_group23) %>%
  mutate(
    newborn_pop_sum = sum(newborn_pop, na.rm = TRUE),
    pop_weighting0 = if_else(newborn_pop_sum > 0, newborn_pop / newborn_pop_sum, NA_real_)
  ) %>%
  ungroup() %>%
  select(year, income_group23, location_name, pop_weighting0)

# double checking weights sum to 1
newborn_wts_23 %>%
  group_by(year, income_group23) %>%
  summarise(w_sum = sum(pop_weighting0, na.rm = TRUE), .groups = "drop")


# attaching weights to lifetables --> then computing averages for income groups
lifetabnb_incgroup23_df <- lifetab_incgroup_df %>%
  filter(year == 2023) %>%
  left_join(newborn_wts_23, by = c("year", "income_group23", "location_name")) %>%
  group_by(year, income_group23, age, age_name) %>%
  summarise(
    mortality_mean = weighted.mean(mortality, w = pop_weighting0, na.rm = TRUE),
    survival_mean = weighted.mean(survival, w = pop_weighting0, na.rm = TRUE),
    remaining_le_mean = weighted.mean(remaining_le, w = pop_weighting0, na.rm = TRUE),
    .groups = "drop"
  )


# changing lifetable income group to match GBD labels
sum_lifetabnb23_incgroup_df <- lifetabnb_incgroup23_df %>%
  mutate(
    income_group23 = case_when(
      income_group23 == "H" ~ "World Bank High Income",
      income_group23 == "UM" ~ "World Bank Upper Middle Income",
      income_group23 == "LM" ~ "World Bank Lower Middle Income",
      income_group23 == "L" ~ "World Bank Low Income",
      TRUE ~ income_group23
    )
  ) %>%
  mutate(location_name = income_group23) %>%
  select(-income_group23)
# this has population-weighted income group averages by age

lifetabnb_inc23_expanded_df <- sum_lifetabnb23_incgroup_df %>%
  cross_join(all_causes) 


# Multiply each death prob by remaining LE at that age
# Expected lifetime YLL: sum over ages of (death rate × survival × remaining LE)
df_p1b <- gbd_deaths_df %>%
  # Get mortality probability of each 
  filter(metric_name == "Rate") %>%
  filter(year == 2023) %>%
  mutate(val = val/1e5) %>%
  # Multiply by survival probability
  left_join(filter(lifetabnb_inc23_expanded_df, year %in% c(2023))) %>%
  group_by(year, location_name, cause_name) %>%
  summarise(exp_yll = sum(val*survival_mean*remaining_le_mean, na.rm = T)) %>%
  # Aggregate up from causes to cluster
  inner_join(distinct(cluster_map23, cause_name, cluster_4)) %>%
  mutate(cluster_4 = case_when(str_detect(cause_name, "COVID") ~ "COVID-related", TRUE ~ cluster_4)) %>%
  mutate(cluster_4 = factor(cluster_4, ordered = T, levels = cluster_groups)) %>%
  group_by(year, location_name, cluster_4) %>%
  summarise(exp_loss = sum(exp_yll, na.rm = T)) %>%
  mutate(type = "Expected YLL")


# Expected lifetime YLD: sum over ages of (YLD rate × survival)
df_p2b <- gbd_ylds_df %>%
  # Get disability probability of each 
  filter(metric_name == "Rate") %>%
  mutate(val = val/1e5) %>%
  # Multiply by survival probability
  left_join(filter(lifetabnb_inc23_expanded_df, year %in% c(2023))) %>%
  group_by(year, location_name, cause_name) %>%
  summarise(exp_yld = sum(val*survival_mean, na.rm = T)) %>%
  # Aggregate up to cause_group
  inner_join(distinct(cluster_map23, cause_name, cluster_4)) %>%
  mutate(cluster_4 = case_when(str_detect(cause_name, "COVID") ~ "COVID-related", TRUE ~ cluster_4)) %>%
  mutate(cluster_4 = factor(cluster_4, ordered = T, levels = cluster_groups)) %>%
  group_by(year, location_name, cluster_4) %>%
  summarise(exp_loss = sum(exp_yld, na.rm = T)) %>% 
  mutate(type = "Expected YLD")


# 2023 expected losses
df_p1b %>%
  rbind(df_p2b) %>%
  filter(year == 2023) %>%
  filter(location_name != "Global") %>%
  mutate(
    income_group = str_remove(location_name, "^World Bank\\s+"),
    income_group = factor(
      income_group,
      levels = c("High Income", "Upper Middle Income", "Lower Middle Income", "Low Income"),
      ordered = TRUE
    )
  ) %>%
  ggplot() + theme_bw() + facet_wrap(~ type) + 
  geom_bar(aes(x = income_group, y = exp_loss, fill = cluster_4), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("Ageing-related" = "firebrick1", "COVID-related" = "orange", "Adult (late)" = "blue3", 
                               "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Expected years lost", x = "", fill = "Disease cluster")

ggsave(
  filename = here("output", "appx_fig3_approach2_newbornwght.jpg"),          
  width = 7, height = 3.5, units = "in",     
  dpi = 300                                     
)


# double check this but newborn weightings yields similar results 


# --------------------------------------------

# with COVID as an ageing-related disease


# Multiply each death prob by remaining LE at that age
# Expected lifetime YLL: sum over ages of (death rate × survival × remaining LE)
df_cp1b <- gbd_deaths_df %>%
  # Get mortality probability of each 
  filter(metric_name == "Rate") %>%
  filter(year == 2023) %>%
  mutate(val = val/1e5) %>%
  # Multiply by survival probability
  left_join(filter(lifetabnb_inc23_expanded_df, year %in% c(2023))) %>%
  group_by(year, location_name, cause_name) %>%
  summarise(exp_yll = sum(val*survival_mean*remaining_le_mean, na.rm = T)) %>%
  # Aggregate up from causes to cluster
  inner_join(distinct(cluster_map23, cause_name, cluster_4)) %>%
  mutate(cluster_4 = factor(cluster_4, ordered = T, levels = cluster_groups)) %>%
  group_by(year, location_name, cluster_4) %>%
  summarise(exp_loss = sum(exp_yll, na.rm = T)) %>%
  mutate(type = "Expected YLL")


# Expected lifetime YLD: sum over ages of (YLD rate × survival)
df_cp2b <- gbd_ylds_df %>%
  # Get disability probability of each 
  filter(metric_name == "Rate") %>%
  mutate(val = val/1e5) %>%
  # Multiply by survival probability
  left_join(filter(lifetabnb_inc23_expanded_df, year %in% c(2023))) %>%
  group_by(year, location_name, cause_name) %>%
  summarise(exp_yld = sum(val*survival_mean, na.rm = T)) %>%
  # Aggregate up to cause_group
  inner_join(distinct(cluster_map23, cause_name, cluster_4)) %>%
  mutate(cluster_4 = factor(cluster_4, ordered = T, levels = cluster_groups)) %>%
  group_by(year, location_name, cluster_4) %>%
  summarise(exp_loss = sum(exp_yld, na.rm = T)) %>% 
  mutate(type = "Expected YLD")


# 2023 expected losses
f3_expected_yll_yld_data <- df_cp1b %>%
  rbind(df_cp2b) %>%
  filter(year == 2023) %>%
  filter(location_name != "Global") %>%
  mutate(
    income_group = str_remove(location_name, "^World Bank\\s+"),
    income_group = factor(
      income_group,
      levels = c("High Income", "Upper Middle Income", "Lower Middle Income", "Low Income"),
      ordered = TRUE
    )
  )

write_csv(f3_expected_yll_yld_data, here("output", "figure_underlying_data", "f3_expected_yll_yld.csv"))

f3_expected_yll_yld_plot <- f3_expected_yll_yld_data %>%
  ggplot() + theme_bw() + facet_wrap(~ type) + 
  geom_bar(aes(x = income_group, y = exp_loss, fill = cluster_4), stat = "identity", position = "stack") + 
  scale_fill_manual(values = c("Ageing-related" = "firebrick1", "Adult (late)" = "blue3", 
                               "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")) + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(y = "Expected years lost", x = "", fill = "Disease cluster")

ggsave(
  filename = here("output", "covid_as_ageing", "fig3_approach2_newbornwght.jpg"),          
  plot = f3_expected_yll_yld_plot,
  width = 7, height = 3.5, units = "in",     
  dpi = 300                                     
)

ggsave(
  filename = here("output", "figures_pdfs", "f3_expected_yll_yld.pdf"),
  plot = f3_expected_yll_yld_plot +
    labs(title = "Figure 3: Expected YLDs and YLLS  for a Newborn in 2023") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")),
  width = 8.27, height = 11.69, units = "in"
)


# saving dataset for comments
exp_newborn_df <- df_cp1b %>%
  rbind(df_cp2b) %>%
  filter(year == 2023) %>%
  filter(location_name != "Global")

write.csv(exp_newborn_df, here("output", "andrew_figure3_exp_newborn.csv"), row.names = FALSE)


# ------------------------------------------------------------------------------
# ------------------------------------------------------------------------------

# Reproducing Figure 6
# (figures 4 and 5 data prepped below and then produced in 4. and 5.)


# use DALYs and YLDs to back out YLLs (i.e. YLLs = DALYs - YLDs)

current_gbd_tab <- gbd_df_all %>%
  filter(year %in% c(2023)) %>%
  filter(measure_name %in% c("YLDs", "DALYs")) %>%
  filter(metric_name == "Number") %>%
  mutate(across(c(val, upper, lower), as.numeric)) %>%
  summarise(across(c(val, upper, lower), sum, na.rm = TRUE),
            .by = c(location_name, year, cause_name, age_name, sex_name, metric_name, measure_name)) %>%
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
  ) %>%
  inner_join(distinct(cluster_map23, cause_name, cluster_4), by = "cause_name") %>%
  mutate(cluster_4 = case_when(
    str_detect(cause_name, "COVID") ~ "COVID-related", TRUE ~ cluster_4
  )) %>%
  group_by(year, location_name, cluster_4, measure_name) %>%
  summarise(
    val   = sum(val,   na.rm = TRUE),
    upper = sum(upper, na.rm = TRUE),
    lower = sum(lower, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(year, location_name, measure_name) %>%
  mutate(
    perc     = val / sum(val),
    perc_lab = sprintf(perc, fmt = '%#.2f')
  ) %>%
  mutate(
    measure_name = factor(measure_name, levels = c("DALYs", "YLDs", "YLLs"), ordered = TRUE),
    cluster_4    = factor(cluster_4,    levels = cluster_groups, ordered = TRUE)
  )

# Heatmap plot for 2023
current_gbd_tab %>%
  filter(year == 2023, measure_name %in% c("YLLs", "YLDs", "DALYs")) %>%
  mutate(
    income_group = str_remove(location_name, "^World Bank\\s+"),
    income_group = factor(
      income_group,
      levels = c("Global", "High Income", "Upper Middle Income", "Lower Middle Income", "Low Income"),
      ordered = TRUE
    )
  ) %>%
  ggplot(aes(x = measure_name, y = fct_rev(cluster_4))) + theme_bw() + 
  facet_wrap(~ income_group, nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_tile(aes(fill = as.numeric(perc)), color = "black") +
  geom_text(aes(label = perc_lab), color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "firebrick") +
  labs(x = "Measure", y = "Disease Cluster", fill = "Proportion of current disease burden")

ggsave( #
  filename = here("output", "fig6_burdencluster.jpg"),          
  width = 10, height = 2.5, units = "in",     
  dpi = 300                                     
)

# ----- COVID included as ageing-related disease - main figure -----------------

current_gbd_tab <- gbd_df_all %>%
  filter(year %in% c(2023)) %>%
  filter(measure_name %in% c("YLDs", "DALYs")) %>%
  filter(metric_name == "Number") %>%
  select(location_name, year, cause_name, age_name, sex_name, measure_name, metric_name,
         val, upper, lower) %>%
  # backing out YLLs from DALYs - YLDs
  pivot_wider(
    names_from  = measure_name,
    values_from = c(val, upper, lower),
    names_sep   = "_"
  ) %>%
  mutate(
    val_YLLs   = val_DALYs   - val_YLDs,
    upper_YLLs = upper_DALYs - upper_YLDs,
    lower_YLLs = lower_DALYs - lower_YLDs
  ) %>%
  # back to long format
  pivot_longer(
    cols      = matches("^(val|upper|lower)_"),
    names_to  = c(".value", "measure_name"),
    names_sep = "_"
  ) %>%
  inner_join(distinct(cluster_map23, cause_name, cluster_4), by = "cause_name") %>%
  group_by(year, location_name, cluster_4, measure_name) %>%
  summarise(
    val   = sum(val,   na.rm = TRUE),
    upper = sum(upper, na.rm = TRUE),
    lower = sum(lower, na.rm = TRUE),
    .groups = "drop_last"
  ) %>%
  group_by(year, location_name, measure_name) %>%
  mutate(
    perc     = val / sum(val),
    perc_lab = sprintf(perc, fmt = '%#.2f')
  ) %>%
  mutate(
    measure_name = factor(measure_name, levels = c("DALYs", "YLDs", "YLLs"), ordered = TRUE),
    cluster_4    = factor(cluster_4,    levels = cluster_groups, ordered = TRUE)
  )

# Heatmap plot for 2023
current_gbd_tab %>%
  filter(year == 2023, measure_name %in% c("YLLs", "YLDs", "DALYs")) %>%
  mutate(
    income_group = str_remove(location_name, "^World Bank\\s+"),
    income_group = factor(
      income_group,
      levels = c("Global", "High Income", "Upper Middle Income", "Lower Middle Income", "Low Income"),
      ordered = TRUE
    )
  ) %>%
  ggplot(aes(x = measure_name, y = fct_rev(cluster_4))) + theme_bw() + 
  facet_wrap(~ income_group, nrow = 1) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_tile(aes(fill = as.numeric(perc)), color = "black") +
  geom_text(aes(label = perc_lab), color = "black", size = 3) +
  scale_fill_gradient2(low = "white", high = "firebrick") +
  labs(x = "Measure", y = "Disease Cluster", fill = "Proportion of current disease burden")

ggsave( #
  filename = here("output", "covid_as_ageing", "c_fig6_burdencluster.jpg"),          
  width = 10, height = 2.5, units = "in",     
  dpi = 300                                     
)



# ------------------------------------------------------------------------------

# mortality and disability CSV - data prep for forecasts


# only using 1990 and 2023:

# prepping gbd_deaths data so can expand out age bands --> need to extract the lower age of each band
gbd_deaths_df <- readRDS(here("output-data", "gbd_deaths.rds"))
gbd_ages <- readRDS(here("output-data", "gbd_data_ages.rds")) %>%
  transmute(
    age_name = as.character(age_name),
    age_low = case_when(
      str_detect(age_name, "Under|<") ~ 0,
      TRUE ~ parse_number(age_name) 
    )
  ) %>%
  filter(!is.na(age_low)) %>%
  distinct()


years <- sort(unique(gbd_deaths_df$year))
locations <- as.character(unique(gbd_deaths_df$location_name))
ages <- 0:100   # expand out til age 100

# to have full age-location-year time series, conditional on these aspects existing in the first place
fill_df <- crossing(
  year = years,
  location_name = locations,
  age_low = ages
)

mortality_med_df <- gbd_deaths_df %>%
  filter(metric_name == "Rate") %>%
  select(-upper, -lower, -filename, -last_modified) %>%
  left_join(cluster_map23, by = "cause_name") %>%
  left_join(gbd_ages, by = "age_name") %>%
  # aggregating across causes within each location-year-age within each cluster
  group_by(location_name, year, age_name, age_low) %>% 
  summarise(
    mortality = sum(val), # total mortality across all clusters
    mortality_infant = sum(val*(cluster_4 == "Infant")), # cluster condition is binary so only sums infant
    mortality_adult_early = sum(val*(cluster_4 == "Adult (early)")),
    mortality_adult_late = sum(val*(cluster_4 == "Adult (late)")),
    mortality_ageing = sum(val*(cluster_4 == "Ageing-related")),
    mortality_covid = sum(val*str_detect(cause_name, "COVID")),
    mortality_ageing_nocov = mortality_ageing - mortality_covid,
    .groups = "drop"
  ) %>%
  full_join(fill_df, by = c("year","location_name","age_low")) %>%
  arrange(location_name, year, age_low) %>%
  group_by(location_name, year) %>%
  # filling down: for each age band, allow the ages within the band to inherit the first age_low obs' mortality values
  # i.e. for age_low = 95, band = 95-99. Fill down ensures 96 - 99 takes on 95's values
  tidyr::fill(
    age_name, mortality, mortality_infant, mortality_adult_early, mortality_adult_late,
    mortality_ageing, mortality_covid, mortality_ageing_nocov,
    .direction = "down"
  ) %>%
  ungroup() %>%
  rename(age = age_low, location = location_name)

write.csv(mortality_med_df, here("output-data", "nob_mortality_medium.csv"), row.names = FALSE)


health_med_df <- readRDS(here("output-data", "gbd_ylds.rds")) %>%
  filter(metric_name == "Rate") %>%
  select(-upper, -lower, -filename, -last_modified) %>%
  left_join(cluster_map23, by = "cause_name") %>%
  left_join(gbd_ages, by = "age_name") %>%
  group_by(location_name, year, age_name, age_low) %>%
  summarise(
    disability = sum(val),
    disability_infant = sum(val*(cluster_4 == "Infant")),
    disability_adult_early = sum(val*(cluster_4 == "Adult (early)")),
    disability_adult_late = sum(val*(cluster_4 == "Adult (late)")),
    disability_ageing = sum(val*(cluster_4 == "Ageing-related")),
    disability_covid = sum(val*str_detect(cause_name, "COVID")),
    disability_ageing_nocov = disability_ageing - disability_covid,
    .groups = "drop"
  ) %>%
  full_join(fill_df, by = c("year","location_name","age_low")) %>%
  arrange(location_name, year, age_low) %>%
  group_by(location_name, year) %>%
  tidyr::fill(
    age_name, disability, disability_infant, disability_adult_early, disability_adult_late,
    disability_ageing, disability_covid, disability_ageing_nocov,
    .direction = "down"
  ) %>%
  ungroup() %>%
  rename(age = age_low, location = location_name)

write.csv(health_med_df, here("output-data", "nob_disability_medium.csv"), row.names = FALSE)



# we now calculate HLE:LE ratio directly from GBD data


mortality_df <- read_csv(here("output-data", "nob_mortality_medium.csv")) %>% 
  select(-age_name) %>%
  inner_join(select(read_csv(here("output-data", "nob_disability_medium.csv")), - age_name)) %>%
  # Convert rates from per 100,000 to proportions
  mutate(mortality = mortality/1e5,
         disability = disability/1e5,
         mortality_infant = mortality_infant/1e5,
         mortality_adult_early = mortality_adult_early/1e5,
         mortality_adult_late = mortality_adult_late/1e5,
         mortality_ageing_nocov = mortality_ageing_nocov/1e5,
         mortality_covid = mortality_covid/1e5) %>%
  
  # Reshape to long format for disease-specific analysis
  pivot_longer(cols = c(mortality_infant, mortality_adult_early, mortality_adult_late, mortality_ageing_nocov, mortality_covid), 
               names_to = "diseases", values_to = "contribution") %>%
  
  # Clean disease category names
  mutate(diseases = case_when(str_detect(diseases, "infant") ~ "Infant",
                              str_detect(diseases, "adult_early") ~ "Adult (early)",
                              str_detect(diseases, "adult_late") ~ "Adult (late)",
                              str_detect(diseases, "ageing") ~ "Ageing-related", 
                              str_detect(diseases, "covid") ~ "COVID-related", 
                              TRUE ~ "All"), 
         diseases = factor(diseases, levels = cluster_groups, ordered = T)) %>%
  
  # Create multiple eradication scenarios (0% to 100% reduction)
  crossing(reduction = seq(0, 1, 0.1)) %>%
  mutate(mortality_lower = mortality - reduction*contribution) %>%
  
  # Calculate survival and life expectancy under each scenario
  arrange(reduction, location, year, age) %>%
  group_by(diseases, reduction, location, year) %>%
  mutate(mortality_lag = replace_na(lag(mortality, n = 1), 0),
         survival = cumprod(1 - mortality_lag),
         mortality_lower_lag = replace_na(lag(mortality_lower, n = 1), 0),
         survival_lower = cumprod(1 - mortality_lower_lag)) %>%
  
  # Calculate remaining life expectancy at each age
  arrange(diseases, reduction, location, year, -age) %>%
  mutate(LE = cumsum(survival)/survival,
         LE_lower = cumsum(survival_lower)/survival_lower) %>%
  arrange(diseases, location, year, age, reduction)

# Calculate baseline life expectancy and health-adjusted life expectancy
mortality_df %>% 
  ungroup() %>%
  select(-diseases) %>%
  #filter(reduction == 0) %>%
  distinct(location, year, age, mortality, survival, disability) %>%
  filter(year %in% c(1990, 2023)) %>%
  group_by(location, year) %>%
  mutate(hsurv = (1 - pmin(disability, 1))*survival) %>%
  summarise(LE_birth = sum(survival),
            HLE_birth = sum(hsurv)) %>%
  mutate(ratio = HLE_birth/LE_birth)



# ------------------------------------------------------------------------------

# Correlation between clusters


# Function for a Correlation that safely returns 0 if no variance
cor_nas <- function(x,y){
  if (sd(y) >0 & sd(x) > 0){
    out_ <- cor(x,y)
  } else {
    out_ <- 0
  }
  return(out_)
}


# Build Age–cause crosswalk for complete grids
age_cause_converter <- distinct(jlifetab_df, age_name, age) %>%
  crossing(all_causes) %>%
  filter(age <100)


# Cluster-level correlation summary (Global)
cluster_level <- gbd_df_all %>%
  filter(
    measure_name %in% c("Deaths", "YLDs", "DALYs"),
    (measure_name %in% c("Deaths", "YLDs") & metric_name == "Rate") |
      (measure_name %in% c("DALYs")       & metric_name == "Number"),
    location_name == "Global",
    year == 2023
  ) %>%
  pivot_wider(
    id_cols     = c(location_name, year, cause_name, age_name),
    names_from  = measure_name,
    values_from = val
  ) %>%
  full_join(age_cause_converter, by = c("cause_name", "age_name")) %>% 
  mutate(
    Deaths = replace_na(Deaths, 0),
    YLDs   = replace_na(YLDs,   0),
    DALYs  = replace_na(DALYs,  0)
  ) %>%
  arrange(cause_name, age) %>%
  # From here it starts to differ from cause_level
  left_join(distinct(cluster_map23, cause_name, cluster_4), by = "cause_name") %>%
  # ⚠️ NOTE: The next two group_by() calls — the first (age, cluster_4) is
  # immediately overwritten by the second (cluster_4). If you intended to
  # correlate within each (age, cluster_4), remove the second group_by().
  # group_by(age, cluster_4) %>%
  group_by(cluster_4) %>%
  summarise(cluster_level_co = cor_nas(Deaths, YLDs), .groups = "drop")


# Cause-level profiles and death–disability
# correlation by age (Global)
cause_level <- gbd_df_all %>%
  # Keep measures and metrics that are comparable:
  # - Deaths/YLDs as RATES
  # - DALYs as NUMBER (to get shares later)
  filter(
    measure_name %in% c("Deaths", "YLDs", "DALYs"),
    (measure_name %in% c("Deaths", "YLDs") & metric_name == "Rate") |
      (measure_name %in% c("DALYs")       & metric_name == "Number"),
    location_name == "Global",
    year == 2023
  ) %>%
  # Wide by measure for (Deaths, YLDs, DALYs)
  pivot_wider(
    id_cols     = c(location_name, year, cause_name, age_name),
    names_from  = measure_name,
    values_from = val
  ) %>%
  # Complete grid to avoid dropped ages/causes
  full_join(age_cause_converter, by = c("cause_name", "age_name")) %>% 
  # Replace missing values with zeros (post-completion)
  mutate(
    Deaths = replace_na(Deaths, 0),
    YLDs   = replace_na(YLDs,   0),
    DALYs  = replace_na(DALYs,  0)
  ) %>%
  arrange(cause_name, age) %>%
  group_by(cause_name) %>%
  # Total DALYs for share weighting
  mutate(Total_DALYs = sum(DALYs)) %>%
  ungroup() %>%
  mutate(share_DALYs = Total_DALYs / sum(DALYs)) %>%
  # Attach clusters and compute per-cause correlation over age
  left_join(distinct(cluster_map23, cause_name, cluster_4), by = "cause_name") %>%
  group_by(cause_name) %>%
  mutate(deaths_dis_cor = cor_nas(Deaths, YLDs)) %>%
  ungroup()


# Aggregate per-cause correlations and compare
# with cluster-level correlations
cause_cors <- cause_level %>%
  distinct(cause_name, share_DALYs, cluster_4, deaths_dis_cor)

cause_cors %>%
  group_by(cluster_4) %>%
  summarise(mean_unwt = mean(deaths_dis_cor),
            mean_wt = sum(deaths_dis_cor*share_DALYs)/sum(share_DALYs)) %>%
  left_join(cluster_level)






