# ------------------------------------------------------------------------------
# Title: setup_run.R
#
# ------------------------------------------------------------------------------


# setup.R
# One-time setup for replication package

required_pkgs <- c("renv")

for (p in required_pkgs) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

# restore package versions from renv.lock
renv::restore(prompt = FALSE)


message("Setup complete. Now run: source('code/1. clean_9023.R')")


### Running files

source(here("code", "functions_9023.R"))

#source(here("code", "1.clean_9023.R"))

source(here("code", "2. clustering_9023.R"))

source(here("code", "3. static_analysis_9023.R"))

source(here("code", "4. prep_data_scenarios_9023.R"))

source(here("code", "5. prep_forecast_dalys_9023.R"))

source(here("code", "6. forecast_interventions_9023.R"))








