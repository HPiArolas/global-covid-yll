# Global years lost of life lost to COVID-19

This is the data repository for the project "Global years of life lost to COVID-19".

- Héctor Pifarré i Arolas, Centre for Research in Health Economics, Universitat Pompeu Fabra. For questions regarding the repo, please email hector.pifarre@upf.edu.
- Enrique Acosta, Max Planck Institute for Demographic Research. 
- Guillem López Casasnovas, Centre for Research in Health Economics, Universitat Pompeu Fabra. 
- Adeline Lo, University of Wisconsin-Madison.
- Catia Nicodemo: Centre of Organisation, Department of Primary Economics, University of Oxford. E
- Tim Riffe, Max Planck Institute for Demographic Research. 
- Mikko Myrskylä, Center for Social Data Science, University of Helsinki and Max Planck Institute for Demographic Research. 

All code is included in the `Code` folder and data in `Data` folder, which reproduce analysis in the project.

# References
Data are drawn from several resources.
- Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2017 (GBD 2017) Results.
Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2018. Available from http://ghdx.healthdata.org/gbd-results-tool.
- P. G. T. Walker, et al., The impact of COVID-19 and strategies for mitigation and suppressionin low- and middle-income countries, Science (2020).
- T. Riffe, E. Acosta, and the COVerAGE-DB team, “COVerAGE-DB: A database of COVID-19 Cases and Deaths by Age.” OSF, 15 June 2020. doi:10.17605/OSF.IO/MPWJQ.
- United Nations, Department of Economic and Social Affairs & Population Division. World population prospects Highlights, 2019 revision Highlights, 2019 revision - WPP (2019). OCLC: 1142478963.
- Human Mortality Database - HMD (2020). Available at www.mortality.org or www.humanmortality.de publisher: University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany).



# Code file descriptions
- `A0 - Functions.R`: declares functions for age harmonization, population interpolation, and excess mortality estimation.
- `A1 - Weekly_deaths_ages_harmonization`: harmonizes STMF weekly mortality data into 5-year age groups.
- `A2 - Weekly_exposures_interpolation`: interpolates annual population estimates into weekly.
- `A3 - Baseline_mortality`: estimates baseline mortality.
- `A4 - Excess_mortality_ages_0_95`: summarizes excess mortality.
- `A5 - Excess_and_COVerAGE_deaths`: prepares final excess deaths output.
- `B - Excess YLL.R`: calculations of YLL for excess mortality.
- `B - Other Causes YLL.R`: calculations of YLL for other causes of mortality.
- `B - Projected YLL.R`: calculations of YLL for projected scenarios.
- `C - Compilation main article measures.R`: creates measures cited throughout manuscript.
- `C - Compilation final measures.R`: prepares data to create the measures cited throughout manuscript.
- `C - Final tables.R`: creates tables in manuscript, and a few extra.
- `C - Figures.R`: creates plots and some tables used in manuscript and SI. 

# Main data file descriptions
Excess mortality related 
- `STMF.zip`: All-cause weekly mortality by age and sex for 38 countries. Short-Term Mortality Fluctuations database input files (Version Jan15, 2021), from HMD (2020).

COVID-19 related 
- `deathcounts_clean06-01-2021.rds`: Country, gender, and age specific death counts.
- `matched_excess_deaths_delayed_06_01_2021.rds`: Estimated excess death counts.
- `excess_dates_table_06_01_2021.rds`: Figures for estimated excess death counts.
- `NYTExcessDeaths.csv`: Early data on excess death counts first published by the New York Times (not used in the final article).
- `projections_covid_deaths.RDS`: Projected death counts in Walker et al. (2020), extracted from the working paper version of the article.
- `days_since_first_covid_data.csv`: Difference in days between latest day country COVID data is available and the first official COVID case in each country.

Other causes of death
- `other_cause_transport.rds`: Transport related death counts, from IHME (2018).
- `other_cause_heart.rds`: Heart related death counts, from IHME (2018).
- `other_cause_substance.rds`: Substance related death counts, from IHME (2018).
- `other_cause_drug.rds`: Drug related death counts, from IHME (2018).
- `other_cause_flu.rds`: Flu related death counts, from IHME (2018).

Country lists
- `countries_to_include_excess_06_01_2021.rds`: Excess mortality sample.
- `full sample list.cs`: Full sample.
- `gender_sample.csv`: Gender sample.
- `country_codes.csv`: Country names and ISO codes in the STMF database.

Population & life expectancy
- `country_gbd_sle.2016.rds`: life expectancy, from IHME (2018).
- `country_both_sle_un.rds`: Age specific life expectancy, from WPP (2019).
- `country_male_sle_un.rds`: Male age specific life expectancy, from WPP (2019).
- `country_female_sle_un.rds`: Female age specific life expectancy, from WPP (2019).
- `pop_complete.csv`: Population estimates, from WPP (2019).
- `pop_std.gbd.RDS`: Standard population, from IHME (2018).
- `pop_std.esp.RDS`: European standard population.
- `OffsetsHMD.rds`: Exposures for mortality baseline estimation, from HMD (2020)
- `wpp_f.zip`: Annual estimates in single-year of age for females for mortality baseline estimation, from WPP (2019) (stored in OSF).
- `wpp_m.zip`: Annual estimates in single-year of age for males for mortality baseline estimation, from WPP (2019) (stored in OSF).
