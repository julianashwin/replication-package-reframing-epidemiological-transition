# Title: appendix_cause_hierarchy.R
# Description: 
#              
#
# Date: 13/02/25
# Author: @msabberwal
# ------------------------------------------------------------------------------

rm(list=ls())


setwd("C:/Users/MahimaSabberwal/Documents/dalys_rr_2023/")
getwd()


library(here)

library(tidyverse)
library(readxl)
library(janitor)
library(stargazer)

gbd_df_all <- readRDS("data/gbd_global_all.rds")
gbd_dalys <- readRDS("data/gbd_dalys.rds")
all_causes <- readRDS("raw-data/gbd_data_causes.rds")
gbd_cause_hierarchy <- readRDS("data/gbd_data_cause_hierarchy.rds")

# Life table data (age-specific population, mortality, survival, remaining LE)
jlifetab_df <- readRDS("data/country_lifetab_data.rds")

clusters_df_2023 <- readRDS("data/cluster4_assignment_2023.rds") 


# ------------------------------------------------------------------------------


# Appendix table: assignment of diseases to clusters/account of disease burden (DALYs)
# for 2023 disease burden data

cluster_map23 <- clusters_df_2023 %>%
  distinct(cause_name, cluster_4)

total_dalys_clusters <- gbd_dalys %>%
  filter(location_name == "Global") %>%
  filter(metric_name == "Number") %>%
  filter(year == "2023") %>%
  group_by(cause_name) %>%
  summarise(total_daly_no = sum(val, na.rm = TRUE), .groups = "drop") %>%
  left_join(cluster_map23, by = "cause_name") %>%
  # generating total DALYs in the year, then total DALYs by cluster
  mutate(total_daly_year = sum(total_daly_no)) %>%
  group_by(cluster_4) %>% 
  mutate(total_daly_cluster = sum(total_daly_no, na.rm = TRUE)) %>%
  ungroup() %>%
  # generating percentage of DALYs by condition and cluster
  mutate(daly_percent_condition = (total_daly_no / total_daly_year) * 100) %>%
  mutate(daly_percent_cluster = (total_daly_cluster / total_daly_year) * 100) %>%
  # percentage of cluster explained by individual disease
  mutate(daly_percent_condcluster = (total_daly_no / total_daly_cluster) * 100) %>%
  left_join(gbd_cause_hierarchy, by = "cause_name") %>%
  select(cause_name, cluster_4, daly_percent_condition, daly_percent_condcluster, daly_percent_cluster, level1_name)

write.csv(total_dalys_clusters, "perc_dalys_cluster.csv")


# -----------------------------------------------------------

# Then need to make note of any major classification changes from 2021


cluster21_df <- read_csv("C:/Users/MahimaSabberwal/Documents/dalys_gbd/DALYs-paper/clean_data/cluster_membership_data.csv")

# comparing cluster assignment


# helper to standardise cause names (avoids false mismatches)
norm_name <- function(x) {
  x %>%
    as.character() %>%
    str_trim() %>%
    str_squish() %>%
    str_to_lower()
}

c21 <- cluster21_df %>%
  mutate(cause_key = norm_name(cause_name)) %>%
  distinct(cause_key, .keep_all = TRUE) %>%
  rename(cluster_4_2021 = cluster_4)

c23 <- cluster_map23 %>%
  mutate(cause_key = norm_name(cause_name)) %>%
  distinct(cause_key, .keep_all = TRUE) %>%
  rename(cluster_4_2023 = cluster_4,
         cause_name_2023 = cause_name)

cmp <- full_join(
  c21 %>% select(cause_key, cause_name, cluster_4_2021),
  c23 %>% select(cause_key, cause_name_2023, cluster_4_2023),
  by = "cause_key"
) %>%
  mutate(
    cause_name_display = coalesce(cause_name, cause_name_2023),
    status = case_when(
      is.na(cluster_4_2021) & !is.na(cluster_4_2023) ~ "new_in_2023",
      !is.na(cluster_4_2021) & is.na(cluster_4_2023) ~ "missing_in_2023",
      !is.na(cluster_4_2021) & !is.na(cluster_4_2023) & cluster_4_2021 != cluster_4_2023 ~ "changed",
      TRUE ~ "unchanged"
    )
  )

# 1) Diseases whose cluster assignment changed
changed <- cmp %>%
  filter(status == "changed") %>%
  select(cause_name_display, cluster_4_2021, cluster_4_2023) %>%
  arrange(cause_name_display)

changed


# some diseases no longer present, vice versa - were present

only_2021 <- c21 %>%
  anti_join(c23, by = "cause_key") %>%
  distinct(cause_name) %>%
  arrange(cause_name)

only_2021


only_2023 <- c23 %>%
  anti_join(c21, by = "cause_key") %>%
  distinct(cause_key)

only_2023






# --------------------------------

# cause hierarchy text descriptives 

gbd_cause_hierarchy <- gbd_cause_hierarchy %>%
  select(cause_name, level1_name)

total_dalys_cause_hierarchy <- gbd_dalys %>%
  filter(location_name == "Global") %>%
  filter(metric_name == "Number") %>%
  group_by(cause_name, year) %>%
  mutate(total_daly_no = sum(val, na.rm = TRUE), .groups = "drop") %>% # now have sum of DALYs lost across all ages for that cause globally in a year
  filter(age_name == "0-1 years") %>%
  select(-age_name, -upper, -lower, -last_modified, -val, -filename) %>%
  left_join(cluster_map23, by = "cause_name") %>%
  left_join(gbd_cause_hierarchy, by = "cause_name")

# want percentage of non-communicable in 1990 and 2021
ncd_share <- total_dalys_cause_hierarchy %>%
  # total DALYs lost in that year
  group_by(year) %>%
  mutate(total_daly_year = sum(total_daly_no), .groups = "drop") %>%
  # by level 1 category
  group_by(year, level1_name) %>%
  mutate(total_level1_daly = sum(total_daly_no), .groups = "drop") %>%
  # percent of total
  mutate(level1_percent_total = (total_level1_daly / total_daly_year) * 100)


# percentage of number of non-communicable diseases that are classified as ageing
no_ageing <- total_dalys_cause_hierarchy %>%
  filter(year == 2023) %>%
  filter(cluster_4 == "Ageing-related") %>% # 103 
  filter(level1_name == "Non-communicable diseases") # 94


# percentage of ageing-related disease burden that is NCD (2023)
ncd_ageing_share <- total_dalys_cause_hierarchy %>%
  filter(year == 2023) %>%
  filter(cluster_4 == "Ageing-related") %>%
  group_by(year) %>%
  mutate(sum_ageing_daly = sum(total_daly_no), .groups = "drop") %>%
  group_by(level1_name) %>%
  mutate(sum_level1_dalys = sum(total_daly_no), .groups = "drop") %>%
  mutate(level1_perc_dalys = (sum_level1_dalys / sum_ageing_daly) * 100)


# no of non-communicable diseaes in each cluster
no_ncd <- total_dalys_cause_hierarchy %>%
  filter(year == 2023) %>%
  filter(level1_name == "Non-communicable diseases") # 195

infant <- no_ncd %>%
  filter(cluster_4 == "Infant") # 30

adult_early <- no_ncd %>%
  filter(cluster_4 == "Adult (early)") # 31

adult_late <- no_ncd %>%
  filter(cluster_4 == "Adult (late)") # 40

ageing_related <- no_ncd %>%
  filter(cluster_4 == "Ageing-related") # 94

rm(infant, adult_early, adult_late, ageing_related)





















