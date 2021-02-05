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
- P. G. T. Walker,et al., The impact of COVID-19 and strategies for mitigation and suppressionin low- and middle-income countries, Science (2020).
- T. Riffe, E. Acosta. et. al. “COVerAGE-DB: A database of COVID-19 Cases and Deaths by Age.” OSF, 15 June 2020. doi:10.17605/OSF.IO/MPWJQ.
- United Nations, Department of Economic and Social Affairs & Population Division. World population prospects Highlights, 2019 revision Highlights, 2019 revision (2019). OCLC: 1142478963.
- Human Mortality Database (2020). Available at www.mortality.org or www.humanmortality.de publisher: University of California, Berkeley (USA), and Max Planck Institute for Demographic Research (Germany).



# Code file descriptions

(NEEDS REVIEW ENRIQUE)
- `A - 1_Mortality_baseline.R`: mortality baseline etimation from weekly mortality.
- `A - 2_Excess_mortality_2020`: calculations of 2020 excess mortality.


- `A - Excess deaths`: prepares final deaths output.
- `B - Excess YLL.R`: calculations of YLL for excess mortality.
- `B - Other Causes YLL.R`: calculations of YLL for other causes of mortality.
- `B - Projected YLL.R`: calculations of YLL for projected scenarios.
- `C - Compilation main article measures.R`: creates measures cited throughout manuscript.
- `C - Compilation final measures.R`: prepares data to create the measures cited throughout manuscript.
- `C - Final tables.R`: creates tables in manuscript, and a few extra.
- `C - Figures.R`: creates plots and some tables used in manuscript and SI. 

# Main data file descriptions
- `stmf_5.rds`: Short term mortality fluctuations in 5-years age groups
- `OffsetsHMD.rds`: Exposures for mortality baseline estimation

COVID-19 related 
(NEEDS REVIEW ENRIQUE) --> data to do excess mortality calculations!!!
- `deathcounts_clean06-01-2021.rds`: Country, gender, and age specific death counts.
- `matched_excess_deaths_delayed_06_01_2021.rds`: Estimated excess death counts.
- `excess_dates_table_06_01_2021.rds`: Figures for estimated excess death counts.
- `NYTExcessDeaths.csv`: Early data on excess death counts first published by the New York Times (not used in the final article).
- `projections_covid_deaths.RDS`: Projected death counts in Walker et al. (2020), extracted from the working paper version of the article.
- `days_since_first_covid_data.csv`: Difference in days between latest day country COVID data is available and the first official COVID case in each country.

Other causes of death
- `other_cause_transport.rds`: Transport related death counts from IHME (2018).
- `other_cause_heart.rds`: Heart related death counts from IHME (2018).
- `other_cause_substance.rds`: Substance related death counts from IHME (2018).
- `other_cause_drug.rds`: Drug related death counts from IHME (2018).
- `other_cause_flu.rds`: Flu related death counts from IHME (2018).

Country lists
- `countries_to_include_excess_06_01_2021.rds`: Excess mortality sample.
- `full sample list.cs`: Full sample.
- `gender_sample.csv`: Gender sample.

Population & life expectancy
- `country_gbd_sle.2016.rds`: IMHE-GBD life expectancy.
- `country_both_sle_un.rds`: UN-World Population prospects age specific life expectancy.
- `country_male_sle_un.rds`: UN-World Population prospects male age specific life expectancy.
- `country_female_sle_un.rds`: UN-World Population prospects female age specific life expectancy.
- `pop_complete.csv`: UN-World Population prospects population.
- `pop_std.gbd.RDS`: IMHE-GBD standard population.
- `pop_std.esp.RDS`: European standard population.


