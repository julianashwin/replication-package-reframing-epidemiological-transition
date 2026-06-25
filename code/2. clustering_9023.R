# Title: 2.clustering_9023.R
# Description: Assigns all diseases/causes in GBD to one of 4 life-cycle clusters through K-means clustering
#              Creates plots of individual diseases' age-profile burden and centroid's burden of each cluster 
#              Plots clusters' centroids age-profile over time+
#
# ------------------------------------------------------------------------------
  
rm(list=ls())

library(here)
here::i_am("code/2. clustering_9023.R")
here::dr_here()

library(tidyverse)
library(readxl)
library(janitor)
library(ClusterR)
library(stargazer)
library(gt)
library(factoextra)
library(scales)  
  
dir.create(here("output", "figures_pdfs"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("output", "figure_underlying_data"), recursive = TRUE, showWarnings = FALSE)


# Importing data 
gbd_dalys <- readRDS(here("output-data", "gbd_dalys.rds"))
all_causes <- readRDS(here("output-data", "gbd_data_causes.rds"))
gbd_dalys21 <- readRDS(here("output-data", "gbd_data_dalys21.rds")) # has all years from 1990 - 2021
  

# Process data ------------------------------------------------------------

# Ordered vector of age groups to enforce consistent factor ordering
age_groups <- c("0-1 years", "1-2 years", "2-4 years", "5-9 years", "10-14 years", "15-19 years", 
                "20-24 years", "25-29 years", "30-34 years", "35-39 years", "40-44 years", "45-49 years", 
                "50-54 years", "55-59 years", "60-64 years", "65-69 years", "70-74 years", "75-79 years", 
                "80-84 years", "85-89 years", "90-94 years", "95-99 years")

# Prepare global DALY *rates* by age/cause/year and standardize within year
global_daly_df <- gbd_dalys %>%
  filter(location_name == "Global",          # use global estimates
         metric_name == "Rate") %>%          # work with rates for shape-by-age
  complete(year, age_name, cause_name) %>%   # ensure a full grid; fill missing triples
  # this generates rows for all possible combinations of year, age, cause
  # by default fills missing values with NA
  mutate(
    val   = replace_na(val, 0),              # replace missing values with 0
    upper = replace_na(upper, 0),
    lower = replace_na(lower, 0)
  ) %>% 
  # Carry forward shared metadata after completing the grid
  fill(location_name, year, sex_name, measure_name, metric_name, .direction = "down") %>%
  # Harmonize age labels to the age_groups vector used throughout
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "12-23 months", "1-2 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age_name = factor(age_name, levels = age_groups, ordered = TRUE)) %>%
  # Standardize val within each (year, cause) age profile to mean 0, sd 1
  group_by(year, cause_name) %>%
  mutate(val_std = (val - mean(val)) / sd(val)) %>%
  mutate(val_std = replace_na(val_std, 0)) %>%   # handle degenerate sd=0 cases
  ungroup() 

# Visual sanity check: shape of standardized age profiles each decade
global_daly_df %>%  
  filter(year %% 10 == 0) %>%
  ggplot() + theme_bw() + 
  facet_wrap(~year) + 
  geom_line(aes(x = age_name, y = val_std, group = cause_name))

# Check that all causes are covered - true if cause name appears in global_daly_df, false if not
# then subsets to causes in all_causes which are not in global_daly_df
tabyl(all_causes$cause_name %in% unique(global_daly_df$cause_name))
all_causes$cause_name[!(all_causes$cause_name %in% unique(global_daly_df$cause_name))]

  

# -----------------------------------------

# clustering 2023

# Convert to wide matrix (rows = causes, columns = age groups) for clustering (2023 only)
cluster_matrix_2023 <- global_daly_df %>%
  group_by(cause_name, age_name) %>%
  
  filter(year == 2023) %>%
  pivot_wider(id_cols = cause_name, names_from = age_name, values_from = val_std)
head(cluster_matrix_2023)
# Check column names
names(cluster_matrix_2023)



# K-means - 4 clusters --------------------

# Run kmeans++ on 2023 data 
# ignore the cause_name column
# The dimensions of the space in which the clustering will be made are the age groups
# Each disease is a vector in this space, 
# with coordinates given by the standardized DALY rate for that age group
kmeans4 <- KMeans_rcpp(as.matrix(cluster_matrix_2023[,2:ncol(cluster_matrix_2023)]), 
                      clusters = 4, num_init = 5, 
                      max_iters = 100, initializer = 'kmeans++', seed = 123)

# Attach cluster assignments back to the long table and label clusters semantically
clusters_df_2023 <- global_daly_df %>% filter(year == 2023) %>% 
  left_join(tibble(cause_name = cluster_matrix_2023$cause_name, cluster_4 = kmeans4$clusters)) %>%
  relocate(cluster_4, .after = cause_name) %>%
  mutate(
    # Manual naming to reflect typical age-at-peak pattern observed
    cluster_4 = case_when(
      cluster_4 == 4  ~ "Infant",
      cluster_4 == 3  ~ "Adult (early)",
      cluster_4 == 2  ~ "Ageing-related",
      cluster_4 == 1  ~ "Adult (late)"
    ),
    cluster_4 = factor(cluster_4,
                       levels = c("Infant", "Adult (early)", "Adult (late)", "Ageing-related"),
                       ordered = TRUE),
    # Numeric x-axis for plotting (take left bound of "X-Y years")
    age_no = as.character(age_name),
    age_no = as.numeric(str_split_i(age_no, "-", 1))
  )

clusters_df_2023 %>%
  saveRDS(here("output-data", "cluster4_assignment_2023.rds"))
clusters_df_2023 <- readRDS(here("output-data", "cluster4_assignment_2023.rds"))  


# Plot
f1_clusters4_data <- clusters_df_2023 %>%
  select(year, age_name, age_no, cause_name, cluster_4, val, val_std)

write_csv(f1_clusters4_data, here("output", "figure_underlying_data", "f1_clusters4.csv"))

f1_clusters4_plot <- f1_clusters4_data %>%  
  ggplot(aes(x = age_no, y = val_std))  + theme_bw() +
  geom_line(aes(group = cause_name, color = cluster_4), alpha = 0.2, size = 0.2) +
  geom_line(aes(group = cluster_4, color = cluster_4), stat = "summary", fun = "mean", size = 0.5) + 
  scale_color_manual("Disease cluster", values = c("Ageing-related" = "firebrick1", "Adult (late)" = "blue3", 
                                                   "Adult (early)" = "cornflowerblue", "Infant" = "forestgreen")) + 
  xlab("Age") + ylab("Disease burden (standardised)") + 
  scale_y_continuous(limits = c(-2.5,4.5), expand = c(0, 0)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave( #
  filename = here("output", "clusters4_global2023.jpg"),          
  plot = f1_clusters4_plot,
  width = 9, height = 4, units = "in",     
  dpi = 300                                     
)

ggsave(
  filename = here("output", "figures_pdfs", "f1_clusters4.pdf"),
  plot = f1_clusters4_plot +
    labs(title = "Figure 1: Disease Clusters 2023: Centroids and Individual Diseases") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")),
  width = 8.27, height = 11.69, units = "in"
)



# ------------------------------------------------------------------------------

# Reproducing Figure 7 


# plotting centroids over time -- 2001 - 2023

# attach previously defined 2001-21 centroids to 2023-defined
# NOTE: we use previous years' data 

# Stability analysis ------------------------------------------------------
# re-fit clustering separately for each year
#   - Re-cluster per year
#   - Align cluster labels across years by the age at which the centroid peaks
#   - Track centroids and membership stability over time


# Join previous DALY data to 2023 DALY - will plot for 2000 - 2023
global_daly0123_df <- gbd_dalys21 %>%
  rbind(gbd_dalys) %>%
  filter(location_name == "Global",          # use global estimates
         metric_name == "Rate",
         year > 1999) %>%          # work with rates for shape-by-age
  complete(year, age_name, cause_name) %>%   # ensure a full grid; fill missing triples
  # this generates rows for all possible combinations of year, age, cause
  # by default fills missing values with NA
  mutate(
    val   = replace_na(val, 0),              # replace missing values with 0
    upper = replace_na(upper, 0),
    lower = replace_na(lower, 0)
  ) %>% 
  # Carry forward shared metadata after completing the grid
  fill(location_name, year, sex_name, measure_name, metric_name, .direction = "down") %>%
  # Harmonize age labels to the age_groups vector used throughout
  mutate(age_name = str_replace(age_name, "<1 year", "0-1 years")) %>%
  mutate(age_name = str_replace(age_name, "12-23 months", "1-2 years")) %>%
  mutate(age_name = str_replace(age_name, "95\\+ years", "95-99 years")) %>%
  mutate(age_name = factor(age_name, levels = age_groups, ordered = TRUE)) %>%
  # Standardize val within each (year, cause) age profile to mean 0, sd 1
  group_by(year, cause_name) %>%
  mutate(val_std = (val - mean(val)) / sd(val)) %>%
  mutate(val_std = replace_na(val_std, 0)) %>%   # handle degenerate sd=0 cases
  ungroup() 


clusters_years_df <- tibble()

for (yy in unique(global_daly0123_df$year)){
  
  # Build year-specific wide matrix
  cluster_matrix <- global_daly0123_df %>%
    filter(year == yy) %>%
    group_by(year, cause_name, age_name) %>%
    arrange(year, cause_name, age_name) %>%
    pivot_wider(id_cols = c(year, cause_name), names_from = age_name, values_from = val_std) 
  
  names(cluster_matrix)
  
  # Re-cluster for that year
  kmeans <- KMeans_rcpp(as.matrix(cluster_matrix[,3:ncol(cluster_matrix)]), clusters = 4, num_init = 5, 
                        max_iters = 100, initializer = 'kmeans++', seed = 123)
  
  # Derive a consistent naming scheme by where the centroid peaks (highest age bin index)
  cluster_ids <- tibble(cluster_4 = 1:4, 
                        max_position = apply(kmeans$centroids, 1, which.max)) %>%
    arrange(-max_position) %>%
    mutate(cluster_name = c("Ageing-related", "Adult (late)", 
                            "Adult (early)", "Infant"))
  
  # Attach clusters and aligned names back to long data for that year
  clusters_year_df <- global_daly0123_df %>%
    filter(year == yy) %>%
    left_join(tibble(
      year = cluster_matrix$year, 
      cause_name = cluster_matrix$cause_name, 
      cluster_4 = kmeans$clusters
    )) %>%
    left_join(cluster_ids) %>%
    relocate(cluster_4, cluster_name, .after = cause_name) %>%
    group_by(cluster_name, age_name) %>%
    mutate(
      cluster_name = factor(cluster_name,
                            levels = c("Infant", "Adult (early)", 
                                       "Adult (late)", "Ageing-related"),
                            ordered = TRUE),
      age_no = as.character(age_name),
      age_no = as.numeric(str_split_i(age_no, "-", 1))
    )
  
  # Accumulate across years
  clusters_years_df <- rbind(clusters_years_df, clusters_year_df)
}

# Show df
head(clusters_years_df)

# Compute per-(year, cluster, age) cluster centroids (mean DALYs) for plotting and correlation
clusters_years_df <- clusters_years_df %>%
  group_by(year, cluster_name, age_name) %>%
  mutate(centroid = mean(val_std)) %>%
  select(year, age_name, age_no, cause_name, cluster_4, cluster_name, val, val_std, centroid)


# Centroid correlations
# Obtaining mean for cluster-age-year
centroids_df <- clusters_years_df %>%
  group_by(year, cluster_name, age_name, age_no) %>%
  summarise(centroid = mean(val_std), .groups = "drop")

# Pull out the 2023 centroid for each cluster and age
centroids_2023 <- centroids_df %>%
  filter(year == 2023) %>%
  select(cluster_name, age_name, centroid_2023 = centroid)

# Correlate each year's centroid with the 2023 centroid, within cluster
centroid_corrs <- centroids_df %>%
  filter(year >= 2001) %>%
  left_join(centroids_2023, by = c("cluster_name", "age_name")) %>%
  group_by(cluster_name, year) %>%
  summarise(
    corr_with_2023 = cor(centroid, centroid_2023, use = "complete.obs"),
    .groups = "drop"
  )

centroid_corrs



# Diagnostics: fiddle around with the years --> percent of ageing-related diseases in 2015 that are in 2023
ageing_2023 <- clusters_years_df %>%
  filter(year == 2023, cluster_name == "Ageing-related") %>%
  distinct(cause_name)

ageing_2015 <- clusters_years_df %>%
  filter(year == 2015, cluster_name == "Ageing-related") %>%
  distinct(cause_name)

pct_same <- ageing_2015 %>%
  summarise(
    n_2023 = n(),
    n_also_2015 = sum(cause_name %in% ageing_2015$cause_name),
    pct_also_2015 = 100 * n_also_2015 / n_2023
  )

pct_same


# Plot the centroids over age, per cluster, across years (from 2001 onward)
clusters_years_df %>% 
  filter(year > 2000) %>%
  ggplot(aes(x = age_no, y = centroid))  + theme_bw() +
  facet_wrap(~cluster_name) + 
  #geom_line(aes(group = cause_name, color = cluster_name), alpha = 0.2, size = 0.2) +
  geom_line(aes(group = year, color = year), size = 0.5) + 
  xlab("Age") + ylab("Disease burden (standardised)") + 
  scale_y_continuous(limits = c(-2.5,4.5), expand = c(0, 0)) +
  theme(axis.ticks.y = element_blank(), axis.text.y = element_blank())

ggsave(
  filename = here("output", "global_centroid_plots_0123.jpg"),          
  width = 10, height = 5, units = "in",     
  dpi = 300                                     
)


  
  
  
  
  
  
  
  
  
  
