" Title: 1.clean_9023.R
# Description: Takes the GBD data for years 1990 and 2023. Does preliminary cleaning. Final output:
#              Working off basic_data_cleaning.R for DALYs paper - now reproducing figures/results using latest release (2023) of GBD data. 
#
" ------------------------------------------------------------------------------

rm(list = ls())  


# here() works by R knowing where your session is starting - i.e. always have to start within the project

library(here)
here::i_am("code/1. clean_9023.R")
here::dr_here()


library(tidyverse)
library(readxl)
library(janitor)
library(beepr)
library(foreach)
library(doParallel)
library(stringr)

# Cleaning global level 1990 and 2023:


# listing files to clean:
raw_root <- here("raw-data")
years <- c("1990", "2023")
scope <- c("global", "income")

global_files_df <- map_dfr(years, function(y) {
  map_dfr(scope, function(s) {
    tibble(
      year_folder = y,
      scope = s,
      path = list.files(file.path(raw_root, y, s),
                        pattern="\\.csv$", full.names=TRUE)
    )
  })
})

# ------------------------------------------------------------------------------

# Parallelise the data import and cleaning from many files


library(foreach)
library(doParallel)
registerDoParallel(4)  # use multicore, set to the number of our cores


# sets up parallel cleaning over file rows (binding together) - dopar: run in parallel
gbd_df_all <- foreach(i = seq_len(nrow(global_files_df)),
                      .combine = bind_rows,
                      .packages = c("dplyr","readr","stringr")) %dopar% {
                        path <- global_files_df$path[i]
                        year_folder <- global_files_df$year_folder[i]
                        
                        last_mod_date <- file.info(path)$mtime
                        df_in <- readr::read_csv(path, show_col_types = FALSE)
  
  # clean up - renaming
  if ("location" %in% names(df_in)){
    df_in <- df_in %>%
      rename(cause_name = cause, location_name = location, age_name = age, sex_name = sex, 
             measure_name = measure, metric_name = metric)
  }
  df_in <- df_in %>%
    dplyr::select(location_name, year, cause_name, age_name, sex_name, measure_name, metric_name, val, upper, lower) %>%
    filter(!str_detect(cause_name, "Total")) %>%
    mutate(measure_name = str_replace(measure_name, "YLDs \\(Years Lived with Disability\\)", "YLDs")) %>%
    # mutate(measure_name = str_replace(measure_name, "YLLs \\(Years of Life Lost\\)", "YLLs")) %>%
    mutate(measure_name = str_replace(measure_name, "DALYs \\(Disability-Adjusted Life Years\\)", "DALYs")) %>%
    mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
    mutate(age_name = str_replace(age_name, "12-23 months", "1-2 years")) %>%
    mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
    mutate(age_name = case_when(str_detect(age_name, "years") ~ age_name, TRUE ~ str_c(age_name, " years"))) %>%
    mutate(filename = basename(path), last_modified = last_mod_date)
  
  ### If re-running make sure you add in the disease name changes described in the txt file. 
  
  # Append
  df_in
}

# now has all the datasets stacked on top of each other
gbd_df_all


# If there are duplicates, keep only the latest file. 
gbd_df_all <- gbd_df_all %>%
  arrange(location_name, year, cause_name, age_name, sex_name, measure_name, metric_name, desc(last_modified)) %>% 
  distinct(location_name, year, cause_name, age_name, sex_name, measure_name, metric_name, val, upper, lower, .keep_all = TRUE)
beep()



gbd_df_all <- gbd_df_all %>%
  arrange(location_name, year, cause_name, age_name, sex_name, measure_name, metric_name, desc(last_modified))


gbd_df_all %>%
  saveRDS(here("output-data", "gbd_global_all.rds"))
gbd_df_all <- readRDS(here("output-data", "gbd_global_all.rds"))


# saving separate GBD causes
gbd_df_all %>%
  distinct(cause_name) %>%
  saveRDS(here("output-data", "gbd_data_causes.rds"))


# save different GBD data separately

# DALYs
gbd_dalys <- gbd_df_all %>% filter(measure_name == "DALYs") %>%
  saveRDS(here("output-data", "gbd_dalys.rds"))
gbd_dalys <- readRDS(here("output-data", "gbd_dalys.rds"))

# Deaths
gbd_df_all %>% filter(measure_name == "Deaths") %>%
  saveRDS(here("output-data", "gbd_deaths.rds"))

# Disability - YLDs
gbd_df_all %>% filter(measure_name == "YLDs") %>%
  saveRDS(here("output-data", "gbd_ylds.rds"))

# Distinct ages
gbd_df_all %>% distinct(age_name) %>%
  saveRDS(here("output-data", "gbd_data_ages.rds"))


# countries - do from GBD countries file
# NOTE: Use 2021 country names in GBD for filtering, given wait times for downloading 2023 country data
# But list of countries should be almost identical 
gbd_countries <- readRDS(here("output-data", "gbd_data_countries.rds"))


# But if have country-level data:
#gbd_countries_all %>%
#  distinct(location_name) %>%
#  saveRDS("output-data", "gbd_data_countries.rds")

# ------------------------------------------------------------------------------

"
Cause hierarchy
"

gbd_causes <- readRDS(here("output-data", "gbd_data_causes.rds"))
gbd_ages <- readRDS(here("output-data", "gbd_data_ages.rds"))


# Clean and save the cause hierarchy
gbd_hierarchy_df <- read_xlsx(here("raw-data", "IHME_GBD_2023_HIERARCHIES_Y2025M10D23.xlsx"), sheet = "Cause Hierarchy") %>%
  rename_all(~tolower(str_replace_all(., "\\s+", "_")))

gbd_hierarchy_detailed_df <- gbd_hierarchy_df %>%  
  separate(cause_outline, into = c("level1_outline",  "level2_outline", "level3_outline", "level4_outline"), remove = FALSE) %>%
  mutate(level2_outline = str_c(level1_outline, ".", level2_outline),
         level3_outline = str_c(level2_outline, ".", level3_outline),
         level4_outline = cause_outline) %>%
  right_join(gbd_causes) %>% 
  left_join(gbd_hierarchy_df %>%
              select(cause_name, cause_outline)  %>%
              rename(level1_name = cause_name, level1_outline = cause_outline) %>%
              distinct()) %>%
  left_join(gbd_hierarchy_df %>%
              select(cause_name, cause_outline)  %>%
              rename(level2_name = cause_name, level2_outline = cause_outline) %>%
              distinct()) %>%
  left_join(gbd_hierarchy_df %>%
              select(cause_name, cause_outline)  %>%
              rename(level3_name = cause_name, level3_outline = cause_outline) %>%
              distinct()) %>%
  relocate(level1_name, .after = level1_outline) %>%
  relocate(level2_name, .after = level2_outline) %>%
  relocate(level3_name, .after = level3_outline) %>%
  mutate(level2_name = case_when(is.na(level2_name) ~ level1_name,
                                 TRUE ~ level2_name))

gbd_hierarchy_detailed_df %>%  
  saveRDS(here("output-data", "gbd_data_cause_hierarchy.rds"))



tabyl(gbd_hierarchy_detailed_df$cause_name %in% gbd_causes$cause_name)
tabyl(gbd_causes$cause_name %in% gbd_hierarchy_detailed_df$cause_name)


# ------------------------------------------------------------------------------

# Cleaning macro data - GNI and other classifications

classification_data <- read_xlsx(here("raw-data", "WDIEXCEL2024_10_24.xlsx"), sheet = "Country") %>%
  filter(!is.na(`Income Group`)) %>%
  select(`Country Code`, `Table Name`, `Income Group`) %>%
  rename(name = `Table Name`, code = `Country Code`, income_group = `Income Group`) %>%
  mutate(income_group = case_when(income_group == "High income" ~ "World Bank High Income",
                                  income_group == "Upper middle income" ~ "World Bank Upper Middle Income",
                                  income_group == "Lower middle income" ~ "World Bank Lower Middle Income",
                                  income_group == "Low income" ~ "World Bank Low Income")) %>%
  mutate(location_name = case_when(name == "Korea, Dem. People's Rep." ~ "Democratic People's Republic of Korea",
                                   name == "Micronesia, Fed. Sts." ~ "Micronesia (Federated States of)",
                                   name == "Lao PDR" ~ "Lao People's Democratic Republic",
                                   name == "Vietnam" ~ "Viet Nam",
                                   name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                   name == "Slovak Republic" ~ "Slovakia",
                                   name == "West Bank and Gaza" ~ "Palestine",
                                   name == "Moldova" ~ "Republic of Moldova",
                                   name == "Korea, Rep." ~ "Republic of Korea",
                                   name == "United States" ~ "United States of America",
                                   name == "Bahamas, The" ~ "Bahamas",
                                   name == "St. Lucia" ~ "Saint Lucia", 
                                   name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines", 
                                   name == "Bolivia" ~ "Bolivia (Plurinational State of)", 
                                   name == "Egypt, Arab Rep." ~ "Egypt", 
                                   name == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)", 
                                   name == "Türkiye" ~ "Turkey", 
                                   name == "Yemen, Rep." ~ "Yemen", 
                                   name == "Congo, Rep." ~ "Congo", 
                                   name == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo", 
                                   name == "Tanzania" ~ "United Republic of Tanzania", 
                                   name == "Gambia, The" ~ "Gambia", 
                                   name == "São Tomé and Principe" ~ "Sao Tome and Principe", 
                                   name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis", 
                                   name == "Virgin Islands (U.S.)" ~ "United States Virgin Islands",
                                   TRUE ~ name)) %>%
filter(location_name %in% as.vector(gbd_countries$location_name)) %>%
dplyr::select(location_name, income_group)

wb_country_names <- unique(classification_data$location_name)
gbd_country_names <- unique(gbd_countries$location_name)
# Some non-matches, but this is because the datasets have slightly different coverage of "countries"
wb_country_names[!(wb_country_names %in% gbd_country_names)]
gbd_country_names[!(gbd_country_names %in% wb_country_names)]
gbd_country_names[gbd_country_names %in% wb_country_names]

pop_df_in <- read_xlsx(here("raw-data", "WDIEXCEL2024_10_24.xlsx"), sheet = "Data")

macro_df <- pop_df_in %>%
  filter(`Indicator Code` %in% c("NY.GNP.PCAP.CD", "SP.POP.TOTL")) %>%
  select(-`Indicator Name`) %>%
  rename(name = `Country Name`, code = `Country Code`, ind_code = `Indicator Code`) %>%
  pivot_longer(cols = -c(name, code, ind_code), names_to = "year", values_to = "val") %>% 
  mutate(series = case_when(ind_code == "SP.POP.TOTL"  ~ "WDI_population",
                            ind_code == "NY.GNP.PCAP.CD" ~ "GNI_pc")) %>%
  pivot_wider(id_cols = c(name, code, year), names_from = series, values_from = val) %>%
  mutate(time = as.numeric(as.factor(year))) %>%
  mutate(year = as.numeric(year)) %>%
  mutate(location_name = case_when(name == "Korea, Dem. People's Rep." ~ "Democratic People's Republic of Korea",
                                   name == "Micronesia, Fed. Sts." ~ "Micronesia (Federated States of)",
                                   name == "Lao PDR" ~ "Lao People's Democratic Republic",
                                   name == "Vietnam" ~ "Viet Nam",
                                   name == "Kyrgyz Republic" ~ "Kyrgyzstan",
                                   name == "Slovak Republic" ~ "Slovakia",
                                   name == "West Bank and Gaza" ~ "Palestine",
                                   name == "Moldova" ~ "Republic of Moldova",
                                   name == "Korea, Rep." ~ "Republic of Korea",
                                   name == "United States" ~ "United States of America",
                                   name == "Bahamas, The" ~ "Bahamas",
                                   name == "St. Lucia" ~ "Saint Lucia", 
                                   name == "St. Vincent and the Grenadines" ~ "Saint Vincent and the Grenadines", 
                                   name == "Bolivia" ~ "Bolivia (Plurinational State of)", 
                                   name == "Egypt, Arab Rep." ~ "Egypt", 
                                   name == "Iran, Islamic Rep." ~ "Iran (Islamic Republic of)", 
                                   name == "Türkiye" ~ "Turkey", 
                                   name == "Yemen, Rep." ~ "Yemen", 
                                   name == "Congo, Rep." ~ "Congo", 
                                   name == "Congo, Dem. Rep." ~ "Democratic Republic of the Congo", 
                                   name == "Tanzania" ~ "United Republic of Tanzania", 
                                   name == "Gambia, The" ~ "Gambia", 
                                   name == "São Tomé and Principe" ~ "Sao Tome and Principe", 
                                   name == "St. Kitts and Nevis" ~ "Saint Kitts and Nevis", 
                                   name == "Virgin Islands (U.S.)" ~ "United States Virgin Islands",
                                   TRUE ~ name)) %>%
  mutate(location = case_when(name == "World"  ~ "Global",
                              name == "High income" ~ "World Bank High Income",
                              name == "Upper middle income" ~ "World Bank Upper Middle Income",
                              name == "Lower middle income" ~ "World Bank Lower Middle Income",
                              name == "Low income" ~ "World Bank Low Income",
                              location_name %in% c(gbd_country_names, wb_country_names) ~ "Country",
                              TRUE ~ "Drop")) %>%
  filter(location != "Drop" & year <= 2024) %>% 
  full_join(select(classification_data, location_name, income_group)) %>%
  select(location, location_name, year, income_group, GNI_pc, WDI_population) %>%
filter(!is.na(income_group)) %>%
filter(location_name %in% gbd_countries$location_name)

macro_df %>%
  write_csv(here("output-data", "country_wdi_data.csv"))


# ------------------------------------------------------------------------------

# WPP population/lifetab data --> in actual files, since this includes 2023, take directly from prior


"
WPP life tables and population
"
income_groups <- c("Global", "High Income", "Upper Middle Income", "Lower Middle Income", "Low Income")
## Import and clean population data 
pop_df <- read_xlsx(
  here("raw-data", "WPP2024_POP_F01_1_POPULATION_SINGLE_AGE_BOTH_SEXES.xlsx"),
  skip = 16, sheet = "Estimates") %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>%
  filter(!is.na(year), !is.na(location)) %>%
  filter(type %in% c("Country/Area", "Income Group", "World")) %>%
  dplyr::select(c(location, type, year, `0`:`100+`)) %>% 
  pivot_longer(cols = -c(location, type, year), names_to = "age", values_to = "population") %>%
  mutate(population = as.numeric(population)*1e3) %>%
  mutate(location_name = case_when(location == "State of Palestine" ~ "Palestine", 
                                   location == "Türkiye" ~ "Turkey", 
                                   location == "China, Taiwan Province of China" ~ "Taiwan (Province of China)", 
                                   location == "Dem. People's Republic of Korea" ~ "Democratic People's Republic of Korea",
                                   location == "Micronesia (Fed. States of)" ~ "Micronesia (Federated States of)",
                                   TRUE ~ location)) %>%
  mutate(location_name = case_when(location == "World"  ~ "Global",
                                   location == "High-income countries" ~ "High Income",
                                   location == "Upper-middle-income countries" ~ "Upper Middle Income",
                                   location == "Lower-middle-income countries" ~ "Lower Middle Income",
                                   location == "Low-income countries" ~ "Low Income",
                                   TRUE ~ location_name)) %>%
  #filter(location_name %in% c(gbd_countries$location_name, income_groups)) %>%
  mutate(age = as.numeric(str_replace_all(age, "100\\+", "100"))) %>%
  dplyr::select(location_name, type, year, age, population) 


wpp_country_names <- unique(pop_df$location_name)



## Import and clean life tables
lifetab_file <- here("raw-data", "WPP2024_MORT_F06_1_SINGLE_AGE_LIFE_TABLE_ESTIMATES_BOTH_SEXES.xlsx")

lifetab_sheets <- excel_sheets(lifetab_file)
lifetab_df_raw <- tibble()

for (ss in lifetab_sheets) {
  print(ss)
  if (ss != "NOTES") {
    df_temp <- read_xlsx(lifetab_file, skip = 16, sheet = ss)
    lifetab_df_raw <- rbind(lifetab_df_raw, df_temp)
  }
}
beep()


lifetab_df <- lifetab_df_raw %>%
  rename(location = `Region, subregion, country or area *`, 
         type = Type, year = Year, age = `Age (x)`, 
         mortality = `Central death rate m(x,n)`,
         survival = `Number of survivors l(x)`,
         remaining_le = `Expectation of life e(x)`) %>%
  filter(!is.na(year), !is.na(location)) %>%
  filter(type %in% c("Country/Area", "Income Group", "World")) %>%
  mutate(location_name = case_when(location == "State of Palestine" ~ "Palestine", 
                                   location == "Türkiye" ~ "Turkey", 
                                   location == "China, Taiwan Province of China" ~ "Taiwan (Province of China)", 
                                   location == "Dem. People's Republic of Korea" ~ "Democratic People's Republic of Korea",
                                   location == "Micronesia (Fed. States of)" ~ "Micronesia (Federated States of)",
                                   TRUE ~ location)) %>%
  mutate(location_name = case_when(location == "World"  ~ "Global",
                                   location == "High-income countries" ~ "High Income",
                                   location == "Upper-middle-income countries" ~ "Upper Middle Income",
                                   location == "Lower-middle-income countries" ~ "Lower Middle Income",
                                   location == "Low-income countries" ~ "Low Income",
                                   TRUE ~ location_name)) %>%
  mutate(age = as.numeric(age),
         mortality = as.numeric(mortality),
         survival = as.numeric(survival),
         remaining_le = as.numeric(remaining_le)) %>%
  dplyr::select(location_name, type, year, age, mortality, survival, remaining_le) %>%
  mutate(survival = survival/1e5)

tabyl(wpp_country_names == unique(lifetab_df$location_name))



pop_df %>%
  left_join(lifetab_df) %>%
  filter(year %% 10 == 0) %>%
  ggplot() + theme_bw() + facet_wrap(~year) +
  geom_line(aes(x = age, y = survival, group = location_name, alpha = population))

country_lifetab_df <- pop_df %>%
  left_join(lifetab_df) %>%
  mutate(age_name = str_c(plyr::round_any(age,5, f = floor), "-", plyr::round_any(age,5, f = floor)+4, " years"),
         age_name = case_when(age == 0 ~ "0-1 years", age >=1 & age < 2 ~  "1-2 years", 
                              age >=2 & age <= 4 ~  "2-4 years", 
                              age >= 100 ~ "100+ years", TRUE ~ age_name)) 


country_lifetab_df %>%
  saveRDS(here("output-data", "country_lifetab_data.rds"))


# ------------------------------------------------------------------------------


"
WPP Fertility rates
"

# we don't use GBD fertility since data since not available at aggregate location levels
# and not available for single ages


## Fertility estimates
fertility_est_df <- read_xlsx(here("raw-data", "WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx"),
                              sheet = "Estimates", skip = 16) %>%
  rename(location_name = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>%
  filter(!is.na(year)) %>%
  select(c(location_name, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location_name, -type), as.numeric) %>%
  pivot_longer(cols = -c(location_name, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_est = fertility) %>%
  mutate(location_name = case_when(location_name == "World"  ~ "Global",
                                   location_name == "High-income countries" ~ "High Income",
                                   location_name == "Upper-middle-income countries" ~ "Upper Middle Income",
                                   location_name == "Lower-middle-income countries" ~ "Lower Middle Income",
                                   location_name == "Low-income countries" ~ "Low Income",
                                   TRUE ~ location_name))

locs_2023 <- unique(fertility_est_df$location_name[which(fertility_est_df$year == 2023)])

fertility_est_df <- fertility_est_df %>%
  full_join(crossing(year = 2023:2100, age = 15:49, location_name = locs_2023),
            by = c("location_name", "year", "age")) %>%
  select(-type) %>% arrange(location_name, age, year) %>%
  fill(fertility_est, .direction = "down") %>%
  arrange(location_name, year, age) %>%
  distinct()


## Fertility projections
# Low
fertility_low_df <- read_xlsx(here("raw-data", "WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx"),
                              sheet = "Low variant", skip = 16) %>%
  rename(location_name = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>% filter(!is.na(year)) %>%
  select(c(location_name, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location_name, -type), as.numeric) %>%
  pivot_longer(cols = -c(location_name, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_low = fertility) %>%
  mutate(location_name = case_when(location_name == "World"  ~ "Global",
                                   location_name == "High-income countries" ~ "High Income",
                                   location_name == "Upper-middle-income countries" ~ "Upper Middle Income",
                                   location_name == "Lower-middle-income countries" ~ "Lower Middle Income",
                                   location_name == "Low-income countries" ~ "Low Income", TRUE ~ location_name)) %>%
  select(-type) %>% arrange(location_name, year, age) %>%
  distinct()

# Medium
fertility_med_df <- read_xlsx(here("raw-data", "WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx"),
                              sheet = "Medium variant", skip = 16) %>%
  rename(location_name = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>% filter(!is.na(year)) %>%
  select(c(location_name, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location_name, -type), as.numeric) %>%
  pivot_longer(cols = -c(location_name, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_med = fertility) %>%
  mutate(location_name = case_when(location_name == "World"  ~ "Global",
                                   location_name == "High-income countries" ~ "High Income",
                                   location_name == "Upper-middle-income countries" ~ "Upper Middle Income",
                                   location_name == "Lower-middle-income countries" ~ "Lower Middle Income",
                                   location_name == "Low-income countries" ~ "Low Income", TRUE ~ location_name))  %>%
  select(-type) %>% arrange(location_name, year, age) %>%
  distinct()


# High
fertility_high_df <- read_xlsx(here("raw-data", "WPP2024_FERT_F01_FERTILITY_RATES_BY_SINGLE_AGE_OF_MOTHER.xlsx"),
                               sheet = "High variant", skip = 16) %>%
  rename(location_name = `Region, subregion, country or area *`, 
         type = Type, year = Year) %>% filter(!is.na(year)) %>%
  select(c(location_name, type, year, `15`, `16`, `17`, `18`, `19`, `20`, `21`, `22`, `23`, `24`, `25`,
           `26`, `27`, `28`, `29`, `30`, `31`, `32`, `33`, `34`, `35`, `36`, `37`, `38`, `39`,
           `40`, `41`, `42`, `43`, `44`, `45`, `46`, `47`, `48`, `49`)) %>%
  mutate_at(vars(-location_name, -type), as.numeric) %>%
  pivot_longer(cols = -c(location_name, type, year), names_to = "age", values_to = "fertility") %>%
  mutate(fertility = fertility/2000, age = as.numeric(age)) %>% filter(!is.na(fertility)) %>%
  rename(fertility_high = fertility) %>%
  mutate(location_name = case_when(location_name == "World"  ~ "Global",
                                   location_name == "High-income countries" ~ "High Income",
                                   location_name == "Upper-middle-income countries" ~ "Upper Middle Income",
                                   location_name == "Lower-middle-income countries" ~ "Lower Middle Income",
                                   location_name == "Low-income countries" ~ "Low Income", TRUE ~ location_name))  %>%
  select(-type) %>% arrange(location_name, year, age) %>%
  distinct()


fertility_df <- fertility_est_df %>%
  full_join(fertility_low_df) %>%
  full_join(fertility_med_df) %>%
  full_join(fertility_high_df) %>%
  arrange(location_name, year, age) %>%
  mutate(fertility_low = case_when(year <= 2023 ~ fertility_est, year > 2023 ~ fertility_low)) %>%
  mutate(fertility_med = case_when(year <= 2023 ~ fertility_est, year > 2023 ~ fertility_med)) %>%
  mutate(fertility_high = case_when(year <= 2023 ~ fertility_est, year > 2023 ~ fertility_high)) %>%
  mutate(location_name = case_when(location_name == "State of Palestine" ~ "Palestine", 
                                   location_name == "Türkiye" ~ "Turkey", 
                                   location_name == "China, Taiwan Province of China" ~ "Taiwan (Province of China)", 
                                   location_name == "Dem. People's Republic of Korea" ~ "Democratic People's Republic of Korea",
                                   location_name == "Micronesia (Fed. States of)" ~ "Micronesia (Federated States of)",
                                   TRUE ~ location_name))

beep()


tabyl(gbd_country_names %in% unique(fertility_df$location_name))
gbd_country_names[!(gbd_country_names %in% unique(fertility_df$location_name))]



fertility_df %>%
  saveRDS(here("output-data", "wpp_fertility_rates.rds"))


ggplot(filter(fertility_df, location_name == "Global"), aes(x= age, y = fertility_est)) +
  geom_line(aes(color = year, group = year))
# Note that we divide by 2000 here to get the rate per person (rather than per female)
ggplot(filter(fertility_df, location_name == "Global")) + theme_bw() + xlab("Age") + ylab("Fertility rate") +
  geom_line(aes(x = age, y = fertility_est, group = as.factor(year), color = year)) + 
  geom_line(aes(x = age, y = fertility_med, group = as.factor(year), color = year), linetype = "dashed")
ggplot(filter(fertility_df, location_name == "Low Income")) + theme_bw() + xlab("Age") + ylab("Fertility rate") +
  geom_line(aes(x = age, y = fertility_est, group = as.factor(year), color = year)) + 
  geom_line(aes(x = age, y = fertility_med, group = as.factor(year), color = year), linetype = "dashed")


















