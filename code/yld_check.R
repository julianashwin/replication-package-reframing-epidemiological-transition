

# comparing YLDs

gbd_ylds_23 <- readRDS("C:/Users/MahimaSabberwal/Documents/dalys_rr_2023/data/gbd_ylds.rds") %>%
  filter(year == 2023)

gbd_ylds_21 <- readRDS("C:/Users/MahimaSabberwal/Documents/dalys_gbd/DALYs-paper/raw_data/GBD/gbd_data_ylds.rds") %>%
  filter(year == 2021)




all_causes <- gbd_ylds_23 %>% distinct(cause_name)

loc_cause <- gbd_ylds_23 %>% distinct(location_name, cause_name)

missing <- expand_grid(
  location_name = sort(unique(gbd_ylds_23$location_name)),
  cause_name    = all_causes$cause_name
) %>%
  anti_join(loc_cause, by = c("location_name", "cause_name"))

# summary: how many causes missing per location
missing %>%
  count(location_name, name = "n_missing") %>%
  arrange(desc(n_missing))

missing %>% arrange(location_name, cause_name)



#-------------------------------------------------------------------------------

# Double checking a similar thing has not happened to DALYs or Deaths


# DALYs:

gbd_dalys_23 <- readRDS("C:/Users/MahimaSabberwal/Documents/dalys_rr_2023/data/gbd_dalys.rds") %>%
  filter(year == 2023)

all_causes <- gbd_dalys_23 %>% distinct(cause_name)

loc_cause <- gbd_dalys_23 %>% distinct(location_name, cause_name)

missing <- expand_grid(
  location_name = sort(unique(gbd_dalys_23$location_name)),
  cause_name    = all_causes$cause_name
) %>%
  anti_join(loc_cause, by = c("location_name", "cause_name"))

# summary: how many causes missing per location
missing %>%
  count(location_name, name = "n_missing") %>%
  arrange(desc(n_missing))

missing %>% arrange(location_name, cause_name)


# 0 - have all DALYs data needed


# ------------------------------------------------------------------

# Deaths:

gbd_deaths_23 <- readRDS("C:/Users/MahimaSabberwal/Documents/dalys_rr_2023/data/gbd_deaths.rds") %>%
  filter(year == 2023)

all_causes <- gbd_deaths_23 %>% distinct(cause_name)

loc_cause <- gbd_deaths_23 %>% distinct(location_name, cause_name)

missing <- expand_grid(
  location_name = sort(unique(gbd_deaths_23$location_name)),
  cause_name    = all_causes$cause_name
) %>%
  anti_join(loc_cause, by = c("location_name", "cause_name"))

# summary: how many causes missing per location
missing %>%
  count(location_name, name = "n_missing") %>%
  arrange(desc(n_missing))

missing %>% arrange(location_name, cause_name)

# 0 - have all deaths data needed



julian_gbd_death <- readRDS("C:/Users/MahimaSabberwal/Documents/dalys_gbd/DALYs-paper/raw_data/GBD/2021/gbd_data_deaths.rds")












