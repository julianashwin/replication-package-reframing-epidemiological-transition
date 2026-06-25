# =============================================================================
# DEMOGRAPHIC HEALTH MODELING FUNCTIONS
# =============================================================================
# This code provides functions for modeling population demographics, mortality,
# disability, and health outcomes over time, with capabilities for scenario
# analysis and aging intervention modeling.


# Create dataframes for mortality and disability --------------------------

# Create dataframe with mortality rates for each year - complete mortality panel (all years * ages).
# optionally projects future years using income-transition weighting scheme
# if growth transitions are true, overwrites projected-year rates using income_transition

create_mortality_df <- function(all_mortality_df, loc_name = "Global", start_year = 2023, 
                                end_year = 2100, end_age = 100, growth_transitions = FALSE, 
                                income_transition_df = NULL){
  # Handle cases where start year is beyond available data
  # Use the most recent available data as the starting point
  latest_year <- max(all_mortality_df$year)
  if (start_year > latest_year){
    all_mortality_df <- all_mortality_df %>%
      filter(year == latest_year) %>%
      mutate(year = start_year)
  } else {
    #all_mortality_df <- all_mortality_df %>%
    #  filter(year == start_year)
  }
  
  # Create mortality data structure based on location type
  if (loc_name == "Regions"){
    mortality_df <- all_mortality_df %>%
      # Handle multiple income-based regions
      filter(location %in% c("High Income", "Low Income", 
                             "Lower Middle Income", "Upper Middle Income")) %>%
      relocate(year, .after = location) %>%
      
      # Create complete grid of location x year x age combinations
      full_join(crossing(location = c("High Income", "Low Income", 
                                      "Lower Middle Income", "Upper Middle Income"), 
                         year = start_year:end_year, age = 0:end_age)) %>%
      arrange(location, year, age) %>%
      
      # Forward fill missing values within each location-age group
      fill(location, .direction = "down") %>%
      group_by(location, age) %>% 
      fill(names(all_mortality_df)) %>% 
      
      ungroup()
    
  } else {
    # Handle single location (global)
    mortality_df <- all_mortality_df %>%
      filter(location == loc_name) %>%
      relocate(year, .after = location) %>%
      
      # Create complete grid
      full_join(crossing(year = start_year:end_year, age = 0:end_age)) %>%
      arrange(location, year, age) %>%
      
      # Forward fill missing values
      fill(location, .direction = "down") %>%
      group_by(location, age) %>% 
      fill(names(all_mortality_df)) %>% 
      
      ungroup()
  }
  
  # Apply growth transitions if specified
  # This models changing mortality patterns as regions transition between income levels
  if (growth_transitions){
    # Make sure location is valid
    stopifnot(loc_name %in% c("Regions", unique(income_transition_df$income_group)))
    region_names <- c("High Income", "Low Income", 
                      "Lower Middle Income", "Upper Middle Income")
    
    # Identify the transition proportions, groups and causes
    cause_names <- names(select(all_mortality_df, mortality:last_col()))
    
    # Process each region
    for (region in region_names){
      print(str_c("Projecting forwards mortality for ", region))
      transitions_df <- filter(income_transition_df, income_group == region) %>%
        arrange(year)
      
      # Loop through years (with progress bar)
      pb <- txtProgressBar(min = max(start_year, 2023)+1, max = end_year)
      for (yy in (max(start_year, latest_year)+1):end_year){
        # Find rows for this year and region
        yy_obs <- which(mortality_df$year == yy & mortality_df$location == region)
        # Reset mortality rates to zero for recalculation
        mortality_df[yy_obs,cause_names] <- 0
        # Get transition proportions for this year
        trans_props <- transitions_df[which(transitions_df$year == yy),region_names]
        
        # Add contribution from each income groups rates
        # Calculate weighted average mortality from all income groups
        for (gg in region_names){
          # Get base mortality rates for donor group
          add_rates <- as.numeric(trans_props[1,gg])*as.matrix(
            all_mortality_df[which(
              all_mortality_df$location == gg & 
                all_mortality_df$year == max(all_mortality_df$year)
            ), cause_names]
          )
          
          # Add weighted contribution to target region
          mortality_df[yy_obs,cause_names] <- mortality_df[yy_obs,cause_names] + as.matrix(add_rates)
        }
        setTxtProgressBar(pb, yy)
      }
      
    }
  } 
  
  return(mortality_df)
}


# Create dataframe with disability rates for each year 

# Create disability rate dataframe - parallel function to create_mortality_df
# This function has identical structure as the previous one but handles 
# disability rates instead of mortality

create_disability_df <- function(all_disability_df, loc_name = "Global", start_year = 2023, 
                                 end_year = 2100, end_age = 100, growth_transitions = FALSE, 
                                 income_transition_df = NULL){
  # Handle cases where start year is beyond available data
  latest_year <- max(all_disability_df$year)
  if (start_year > latest_year){
    all_disability_df <- all_disability_df %>%
      filter(year == latest_year) %>%
      mutate(year = start_year)
  }else {
    #all_disability_df <- all_disability_df %>%
    #  filter(year == start_year)
  }
  
  # Create disability data structure (same logic as mortality function)
  if (loc_name == "Regions"){
    disability_df <- all_disability_df %>%
      filter(location %in% c("High Income", "Low Income", 
                             "Lower Middle Income", "Upper Middle Income")) %>%
      relocate(year, .after = location) %>%
      full_join(crossing(location = c("High Income", "Low Income", 
                                      "Lower Middle Income", "Upper Middle Income"), 
                         year = start_year:end_year, age = 0:end_age)) %>%
      arrange(location, year, age) %>%
      fill(location, .direction = "down") %>%
      group_by(location, age) %>% 
      fill(names(all_disability_df)) %>% 
      ungroup()
  } else {
    disability_df <- all_disability_df %>%
      filter(location == loc_name) %>%
      relocate(year, .after = location) %>%
      full_join(crossing(year = start_year:end_year, age = 0:end_age)) %>%
      arrange(location, year, age) %>%
      fill(location, .direction = "down") %>%
      group_by(location, age) %>% 
      fill(names(all_disability_df)) %>% 
      ungroup()
  }
  
  # Apply growth transitions for disability (same logic as mortality)
  if (growth_transitions){
    stopifnot(loc_name %in% c("Regions", unique(income_transition_df$income_group)))
    region_names <- c("High Income", "Low Income", 
                      "Lower Middle Income", "Upper Middle Income")
    # Identify the transition proportions, groups and causes
    cause_names <- names(select(all_disability_df, disability:last_col()))
    for (region in region_names){
      print(str_c("Projecting forwards disability for ", region))
      transitions_df <- filter(income_transition_df, income_group == region) %>%
        arrange(year)
      # Loop through years
      pb <- txtProgressBar(min = max(start_year, 2023)+1, max = end_year)
      for (yy in (max(start_year, latest_year)+1):end_year){
        yy_obs <- which(disability_df$year == yy & disability_df$location == region)
        disability_df[yy_obs,cause_names] <- 0
        trans_props <- transitions_df[which(transitions_df$year == yy),region_names]
        # Add contribution from each income groups rates
        for (gg in region_names){
          add_rates <- as.numeric(trans_props[1,gg])*as.matrix(all_disability_df[which(
            all_disability_df$location == gg & all_disability_df$year == max(all_disability_df$year)),cause_names])
          disability_df[yy_obs,cause_names] <- disability_df[yy_obs,cause_names] + as.matrix(add_rates)
        }
        setTxtProgressBar(pb, yy)
      }
      
    }
  }
  
  return(disability_df)
}



# Define scenarios --------------------------------------------------------

# Define mortality_new by removing a proportion of causes in disease_list
def_mortality_new <- function(mortality_df, disease_list, remove_prop = 1){
  # Calculate new mortality = original - (proportion * sum of targeted diseases)
  mortality_df$mortality_new <- mortality_df$mortality - 
    remove_prop*rowSums(mortality_df[,which(names(mortality_df) %in% disease_list)])
  return(mortality_df)
}


# Define disability_new by removing a proportion of causes in disease_list
def_disability_new <- function(disability_df, disease_list, remove_prop = 1){
  # Calculate new disability = original - (proportion * sum of targeted diseases)
  disability_df$disability_new <- disability_df$disability - 
    remove_prop*rowSums(disability_df[,which(names(disability_df) %in% disease_list)])
  return(disability_df)
}



# Population projections --------------------------------------------------



# Forecast dalys from start_year to end_year given population, fertility, mortality and disability
forecast_dalys <- function(population_df, fertility_df, mortality_df, disability_df, 
                           new_vars = "none", start_year = 2023, end_year = 2100, end_age = 150, 
                           no_births = FALSE, fertility_type = "fertility_est", loc_name = "Regions",
                           growth_transitions = TRUE, project_100plus = TRUE){
  
  # Convert to data frames to ensure consistent structure
  population_df1 <- data.frame(population_df)
  fertility_df1 <- data.frame(fertility_df)
  mortality_df1 <- data.frame(mortality_df)
  disability_df1 <- data.frame(disability_df)
  
  # Handle projections for ages 100+ with rate capping
  # ALSO: here mortality and disability rates are set to projected values
  if (project_100plus){
    # Make sure the mortality rates don't exceed 1 (100,000 per 100,000 (i.e., 100%))
    mortality_df1$mortality <- pmin(mortality_df1$mortality_proj, 1e5)
    # Do it for projected_new morality as well if it exists in the df
    if ("mortality_proj_new" %in% names(mortality_df1)){
      mortality_df1$mortality_new <- pmin(mortality_df1$mortality_proj_new, 1e5)
    }
    # Same for disability rates
    disability_df1$disability <- pmin(disability_df1$disability_proj, 1e5)
    if ("disability_proj_new" %in% names(disability_df1)){
      disability_df1$disability_new <- pmin(disability_df1$disability_proj_new, 1e5)
    }
  }
  
  # Set fertility rates according to fertility_type
  fertility_df1$fertility <- fertility_df1[,fertility_type]
  
  # If not using growth transitions, just use start_year data
  if (!growth_transitions){
    # Static rates: use start_year rates for entire projection period
    if (loc_name == "Regions"){
      # Multi-region setup (mortality and disability)
      
      mortality_df1 <- filter(mortality_df1, year == start_year) %>%
        full_join(crossing(location = c("High Income", "Low Income", 
                                        "Lower Middle Income", "Upper Middle Income"), 
                           year = start_year:end_year, age = 0:end_age)) %>%
        arrange(location, year, age) %>% fill(location, .direction = "down") %>% 
        group_by(location, age) %>% fill(names(mortality_df1)) %>% 
        ungroup() %>% data.frame()
      
      disability_df1 <- filter(disability_df1, year == start_year) %>%
        full_join(crossing(location = c("High Income", "Low Income", 
                                        "Lower Middle Income", "Upper Middle Income"), 
                           year = start_year:end_year, age = 0:end_age)) %>%
        arrange(location, year, age) %>% fill(location, .direction = "down") %>% 
        group_by(location, age) %>% fill(names(disability_df1)) %>% 
        ungroup() %>% data.frame()
      
    } else {
      # Mortality and disability without any growth transitions, 
      # so constant mortality and disability
      
      mortality_df1 <- filter(mortality_df1, year == start_year) %>%
        full_join(crossing(year = start_year:end_year, age = 0:end_age), by = c("year", "age")) %>%
        arrange(location, year, age) %>% fill(location, .direction = "down") %>% 
        group_by(location, age) %>% fill(names(mortality_df1)) %>% 
        ungroup() %>% data.frame()
      
      disability_df1 <- filter(disability_df1, year == start_year) %>%
        full_join(crossing(year = start_year:end_year, age = 0:end_age), by = c("year", "age")) %>%
        arrange(location, year, age) %>% fill(location, .direction = "down") %>% 
        group_by(location, age) %>% fill(names(disability_df1)) %>% 
        ungroup() %>% data.frame()
    }
  }
  # Set mortality and/or disability to new version according to new
  # Apply scenario-specific mortality and disability rates
  stopifnot(new_vars %in% c("none", "both", "mortality", "disability"))
  if (new_vars == "both"){
    # Use modified rates for both mortality and disability
    mortality_df1$mortality <- mortality_df1$mortality_new
    disability_df1$disability <- disability_df1$disability_new
    
  } else if (new_vars == "mortality"){
    # Use modified mortality rates only
    mortality_df1$mortality <- mortality_df1$mortality_new
    
  } else if (new_vars == "disability"){
    # Use modified disability rates only
    disability_df1$disability <- disability_df1$disability_new
    
  }
  
  # Option to simulate zero fertility (no new births)
  if (no_births){
    fertility_df1$fertility <- 0
  }
  # Create a population dataframe to fill
  # Create the main population projection dataframe
  if (loc_name == "Regions"){
    # Create the main population projection dataframe
    pop_data_long <- population_df1 %>% 
      filter(year == start_year) %>%
      select(location, year, age, population) %>%
      
      # Create complete grid of locations, years, and ages
      full_join(crossing(location = c("High Income", "Low Income", 
                                      "Lower Middle Income", "Upper Middle Income"), 
                         year = start_year:end_year, age = 0:end_age)) %>%
      arrange(location, year, age) %>%
      mutate(population = replace_na(population, 0)) %>%
      
      # Merge in the rates
      left_join(select(mortality_df1, c(location, year, age, mortality))) %>%
      left_join(select(disability_df1, c(location, year, age, disability))) %>%
      left_join(select(fertility_df1, c(location, year, age, fertility))) %>%
      
      # Forward fill fertility rates and handle missing values
      arrange(location, age, year) %>% 
      group_by(location, age) %>% fill(fertility, .direction = "down") %>% ungroup() %>%
      mutate(fertility = replace_na(fertility, 0)) %>%
      
      # Convert rates from per 100,000 to proportions
      mutate(mortality = mortality/100000, disability = disability/100000) %>%
      arrange(location, year, age) %>%
      data.frame()
  } else {
    # Single location (global)
    pop_data_long <- population_df1 %>% 
      filter(year == start_year) %>%
      select(location, year, age, population) %>%
      full_join(crossing(location = loc_name, year = start_year:end_year, age = 0:end_age)) %>%
      arrange(location, year, age) %>%
      left_join(select(mortality_df1, c(location, year, age, mortality))) %>%
      left_join(select(disability_df1, c(location, year, age, disability))) %>%
      left_join(select(fertility_df1, c(location, year, age, fertility))) %>%
      arrange(location, age, year) %>% 
      group_by(location, age) %>% fill(fertility, .direction = "down") %>% ungroup() %>%
      mutate(fertility = replace_na(fertility, 0)) %>%
      mutate(mortality = mortality/100000, disability = disability/100000) %>%
      arrange(location, year, age) %>%
      data.frame()
  }
  
  # MAIN POPULATION PROJECTION LOOP
  # Project population forward year by year using cohort-component method
  years <- start_year:end_year
  region_names <- unique(pop_data_long$location)
  
  for (region in region_names){
    for (yy in years[2:length(years)]){
      # Isolate last year's pop and combine with rates
      pop_old <- pop_data_long[which(pop_data_long$location == region & pop_data_long$year == yy-1),]
      
      # Calculate the new births (assume half of population are women)
      # Calculate births: sum of (population * fertility) across all ages
      population_new <- rep(0, nrow(pop_old))
      population_new[1] <- sum(pop_old$population*pop_old$fertility)
      
      # Use mortality to calculate survivors from previous period
      # Population[age+1] = Population[age] * (1 - mortality[age])
      population_new[2:(end_age+1)] <- pop_old$population[1:end_age]*(1-pop_old$mortality[1:end_age])
      
      # Add the new population numbers back into pop_data_long
      # Update the population data
      pop_data_long$population[which(pop_data_long$location == region & pop_data_long$year == yy)] <- population_new
    }
  }
  
  # Calculate DALYs: population weighted by health status
  # DALY = (1 - disability) * population
  # Higher values indicate better population health
  pop_data_long <- pop_data_long %>%
    mutate(daly = (1 - disability)*population)
  #pop_data_long %>% ggplot() + facet_wrap(~location) + geom_line(aes(x = age, y = daly, group = year, color = year))
  
  return(pop_data_long)
}


# Scenario comparison -----------------------------------------------------

# Compare forecasts under mortality/disability and mortality_new/disability_new
# This function runs baseline and intervention scenarios and compares outcomes
compare_forecasts <- function(population_df, fertility_df, mortality_df, disability_df, loc_name = "Regions",
                              start_year = 2023, end_year = 2100, end_age = 150, no_births = FALSE,
                              fertility_type = "fertility_est", growth_transitions = TRUE, 
                              project_100plus = TRUE){
  
  # Calculate the forecast dalys for baseline and new
  # Run four different scenarios:
  
  # 1. Baseline scenario (no interventions)
  pop_baseline <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                 start_year = start_year, end_year = end_year, end_age = end_age, 
                                 no_births = no_births, new = "none", fertility_type = fertility_type, loc_name = loc_name, 
                                 growth_transitions = growth_transitions, project_100plus = project_100plus)
  # 2. Full intervention (both mortality and disability improvements)
  pop_new <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                            start_year = start_year, end_year = end_year, end_age = end_age, 
                            no_births = no_births, new = "both", fertility_type = fertility_type, loc_name = loc_name, 
                            growth_transitions = growth_transitions, project_100plus = project_100plus)
  
  # 3. Mortality intervention only
  pop_mortonly <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                 start_year = start_year, end_year = end_year, end_age = end_age, 
                                 no_births = no_births, new = "mortality", fertility_type = fertility_type, loc_name = loc_name, 
                                 growth_transitions = growth_transitions, project_100plus = project_100plus)
  
  # 4. Disability intervention only
  pop_disonly <- forecast_dalys(population_df, fertility_df, mortality_df, disability_df, 
                                start_year = start_year, end_year = end_year, end_age = end_age, 
                                no_births = no_births, new = "disability", fertility_type = fertility_type, loc_name = loc_name, 
                                growth_transitions = growth_transitions, project_100plus = project_100plus)
  
  # Merge in the baseline and new
  # Merge results and calculate differences
  dalys_df <- pop_baseline %>%
    select(location, year, age, population, daly, mortality, disability) %>%
    rename(population_base = population, daly_base = daly, mort_base = mortality, dis_base = disability) %>% 
    
    # Join full intervention results
    full_join(select(pop_new, c(location, year, age, population, daly, mortality, disability))) %>%
    rename(population_new = population, daly_new = daly, mort_new = mortality, dis_new = disability) %>%
    
    # Join mortality-only intervention results
    full_join(select(pop_mortonly, c(location, year, age, population, daly, mortality, disability))) %>%
    rename(population_mort = population, daly_mort = daly, mort_mort = mortality, dis_mort = disability) %>%
    
    # Join disability-only intervention results
    full_join(select(pop_disonly, c(location, year, age, population, daly, mortality, disability))) %>%
    rename(population_dis = population, daly_dis = daly, mort_dis = mortality, dis_dis = disability) %>%
    
    # Calculate differences from baseline
    mutate(pop_diff = population_new - population_base, daly_diff = daly_new - daly_base,
           pop_diff_mort = population_mort - population_base, daly_diff_mort = daly_mort - daly_base,
           pop_diff_dis = population_dis - population_base, daly_diff_dis = daly_dis - daly_base) %>%
    arrange(location, year, age)
  
  # Aggregate to annual
  # Aggregate results by year and calculate life expectancy measures
  dalys_yly <- dalys_df %>%
    group_by(location, year) %>%
    
    # Calculate life expectancy (LE) as cumulative survival probability
    mutate(
      LE_base = cumprod(1 - mort_base),
      LE_new = cumprod(1 - mort_new),
      LE_mort = cumprod(1 - mort_mort),
      LE_dis = cumprod(1 - mort_dis)
    ) %>%
    
    # Calculate healthy life expectancy (HLE) adjusting for disability
    mutate(
      HLE_base = (1-dis_base)*cumprod(1 - mort_base),
      HLE_new = (1-dis_new)*cumprod(1 - mort_new),
      HLE_mort = (1-dis_mort)*cumprod(1 - mort_mort),
      HLE_dis = (1-dis_dis)*cumprod(1 - mort_dis)
    ) %>%
    
    # Sum across ages to get annual totals
    summarise(across(c(population_base, population_new, population_mort, population_dis, 
                       daly_base, daly_new, daly_mort, daly_dis, 
                       pop_diff, pop_diff_mort, pop_diff_dis, daly_diff, daly_diff_mort, daly_diff_dis,
                       LE_base, LE_new, LE_mort, LE_dis, HLE_dis, HLE_new, HLE_mort, HLE_dis), sum))
  
  return(dalys_yly)
}



# Aging intervention modeling ---------------------------------------------


# Function to change mortality through slowing aging
# This simulates interventions that slow the aging process itself
slow_mortality <- function(mortality_df, slow_by = 0.01, start_slowing = 30, 
                           end_age = 100){
  
  mortality_df$mortality_new <- mortality_df$mortality
  
  # Apply aging slowdown for each year in the data
  for (yy in unique(mortality_df$year)){
    
    # Apply to ages from start_slowing to end_age
    for (aa in start_slowing:end_age){
      
      # Find observations for this age and year
      obs <- which(mortality_df$age == aa & mortality_df$year == yy)
      
      # Calculate "biological age" - slower than chronological age
      # If slow_by = 0.01, then aging is 1% slower
      cont_age <- max((1 - slow_by)*aa, start_slowing)
      
      # Interpolate mortality rate at the continuous biological age
      int_part <- cont_age%/%1 # Integer part
      int_obs <- which(mortality_df$age == int_part & mortality_df$year == yy)
      dec_part <- cont_age%%1 # Decimal part
      
      # New mortality should be slowed version of old mortality 
      # Linear interpolation: m_new = m_old[int_part] + dec_part*(m_old[int_part+1] - m_old[in_part])
      mortality_df$mortality_new[obs] <- 
        mortality_df$mortality[int_obs] + dec_part*(mortality_df$mortality[int_obs+1]
                                                    - mortality_df$mortality[int_obs])
    }
  }
  return(mortality_df)
}


# Function to change disability through slowing aging
slow_disability <- function(disability_df, slow_by = 0.01, start_slowing = 30, 
                            end_age = 100){
  
  disability_df$disability_new <- disability_df$disability
  
  # !!! This was the original code:
  # for (yy in unique(mortality_df$year)){
  # This is the corrected code (but I am not sure if the results would differ)
  for (yy in unique(disability_df$year)){
    for (aa in start_slowing:end_age){
      obs <- which(disability_df$age == aa & disability_df$year == yy)
      
      # Calculate biological age (same logic as mortality)
      cont_age <- max((1 - slow_by)*aa, start_slowing)
      int_part <- cont_age%/%1
      int_obs <- which(disability_df$age == int_part & disability_df$year == yy)
      dec_part <- cont_age%%1
      
      # New disability should be slowed version of old disability 
      # Linear interpolation: m_new = m_old[int_part] + dec_part*(m_old[int_part+1] - m_old[in_part])
      disability_df$disability_new[obs] <- 
        disability_df$disability[int_obs] + dec_part*(disability_df$disability[int_obs+1]
                                                      - disability_df$disability[int_obs])
    }
  }
  return(disability_df)
}


# Function to change fertility through slowing aging
# This extends reproductive lifespan proportionally to aging slowdown
slow_fertility <- function(fertility_df, slow_by = 0.01, start_slowing = 30, 
                           end_age = 100, fertility_type = "fertility_est"){
  
  fertility_df <- data.frame(fertility_df)
  fertility_df$fertility <- fertility_df[,fertility_type]
  fertility_df$fertility_new <- fertility_df$fertility
  
  # Apply to all ages (not year-specific like mortality/disability)
  for (aa in start_slowing:end_age){
    obs <- which(fertility_df$age == aa)
    
    # Calculate biological age for fertility
    cont_age <- max((1 - slow_by)*aa, start_slowing)
    int_part <- cont_age%/%1
    int_obs <- which(fertility_df$age == int_part)
    dec_part <- cont_age%%1
    
    # New fertility should be slowed version of old fertility 
    # Linear interpolation: m_new = m_old[int_part] + dec_part*(m_old[int_part+1] - m_old[in_part])
    fertility_df$fertility_new[obs] <- 
      fertility_df$fertility[int_obs] + dec_part*(fertility_df$fertility[int_obs+1]
                                                  - fertility_df$fertility[int_obs])
  }
  fertility_df <- tibble(fertility_df)
  return(fertility_df)
}