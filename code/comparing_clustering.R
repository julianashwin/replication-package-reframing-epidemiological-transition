
rm(list=ls())


setwd("C:/Users/MahimaSabberwal/Documents/dalys_rr_2023/")
getwd()


library(here)
setwd(here())

library(tidyverse)
library(readxl)
library(janitor)
library(stargazer)


clusters_df_2023 <- readRDS("C:/Users/MahimaSabberwal/Documents/dalys_rr_2023/data/cluster4_assignment_2023.rds")  

cluster_df <- read_csv("C:/Users/MahimaSabberwal/Documents/dalys_gbd/DALYs-paper/clean_data/cluster_membership_data.csv")


cluster_map23 <- clusters_df_2023 %>%
  distinct(cause_name, cluster_4)


# comparing which causes have been clustered differently 
diffs <- cluster_df %>%
  inner_join(cluster_map23, by = "cause_name", suffix = c("_df", "_map")) %>%
  filter(cluster_4_df != cluster_4_map | xor(is.na(cluster_4_df), is.na(cluster_4_map))) %>%
  select(cause_name, cluster_4_df, cluster_4_map)

diffs




























