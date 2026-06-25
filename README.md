## Running code for Reframing the Epidemiological Transition — Increasing Returns to Tackling Ageing-Related Diseases**

R scripts should be run in sequential order. If have downloaded raw GBD and UN data and placed in relevant directory, run 1. clean_9023.R. Else, should skip.


Note: Owing to GBD data allowances, in places, we call aspects from 2021 release of GBD data, i.e. identifying all countries in GBD data. Relevant sections commented in code.



### The Figures in the paper manuscript are generated as follows:

- "Figure 1: Disease Clusters 2023 - Centroids and Individual Diseases" is generated in the file 2. clustering_9023.R as clusters4_global2023.jpg.

- "Figure 2: Mortality and Disability Burden by Cluster, 2023" is generated in the file 3. static_analysis_9023.R

- "Figure 3: Expected Years Lost to Disease and Years Lost to Life for Newborn 2023" is generated in the file 3. static_analysis_9023.R

- "Figure 4: Global Health Gains from Reducing Disease Prevalence" is generated in the file 6. forecasts_interventions_9023.R

- "Figure 5: Increasing Returns from Reducing Disease Prevalence" is generated in the file 6. forecasts_interventions_9023.R


### Instructions for Global Burden of Disease data download:

Accessing Global Burden of Disease data requires an Institute for Health Metrics and Evaluation (IHME) account. Sign in: https://login.healthdata.org/a07655f6-e482-42f3-8b30-6b7d009f813d/b2c_1a_signup_signin/oauth2/v2.0/authorize?client_id=9e66b6a3-5d2e-400f-b812-f60f441d5041&scope=https%3A%2F%2Fihmecsu.onmicrosoft.com%2Fdata-api%2Fdata.read%20openid%20profile%20offline_access&redirect_uri=https%3A%2F%2Fvizhub.healthdata.org%2Fgbd-results%2F&client-request-id=019c473d-7d4b-7fee-bca8-2dd06546124c&response_mode=fragment&response_type=code&x-client-SKU=msal.js.browser&x-client-VER=3.26.1&client_info=1&code_challenge=PIJHkpnYmiT18lVQ2-OjfhFjLWW7KAKaa0ZUug4UTyQ&code_challenge_method=S256&nonce=019c473d-7d4d-7873-a23f-34adee065ced&state=eyJpZCI6IjAxOWM0NzNkLTdkNGItNzU0ZC05N2I1LWY3NGUxNTkwMTU3MSIsIm1ldGEiOnsiaW50ZXJhY3Rpb25UeXBlIjoicmVkaXJlY3QifX0%3D%7C%7B%22permalink%22%3A%22gbd-api-2023-public%2Fe62043ad104baf4dbd044eb4386fecee%22%7D


Will need to download the following for the years 1990 and 2023, at both 'Global' level location and 'World Bank Income Levels' location:
- GBD Estimate: 'Cause of death or injury' 
- Measure: Deaths, DALYs, YLDs
- Cause: Select all most detailed cause
- Age: < 1 year, 12-23 months, 2-4 years, 5-9 years, 10-14 years, 15-19 years, ... (at most detailed level possible) ... 95+ years. Will be 22 groups in total
- Sex: Both

To see breakdown of disease burden by non-communicable/communicable diseases/injuries and other as per Appendix in manuscript, need to download GBD cause hierarchy: https://ghdx.healthdata.org/record/gbd-2023-cause-rei-and-location-hierarchies


### Sources for downloading Demographic/Income data:
- United Nations World Population Prospects (2024 releases): https://population.un.org/dataportal/home?df=7463cc2b-3fed-440c-8f6a-0525e73f4e31
    - F01 Fertility rates by single age of mother
    - F06 Mortality: Single age life tables both sexes
    - F01 Population: Single age both sexes

- World Bank WDI Archives: https://datatopics.worldbank.org/world-development-indicators/wdi-archives.html 10/2024

