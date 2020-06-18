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



# Code file descriptions
- `A - 1_Mortality_baseline.R`: mortality baseline etimation from weekly mortality.
- `A - 2_Excess_mortality_2020`: calculations of 2020 excess mortality.
- `B - Excess YLL.R`: calculations of YLL for excess mortality.
- `B - Other Causes YLL.R`: calculations of YLL for other causes of mortality.
- `B - Projected YLL.R`: calculations of YLL for projected scenarios.
- `C - Compilation final measures.R`: creates measures cited throughout manuscript.
- `C - Final tables.R`: creates tables in manuscript, and a few extra.
- `C - Figures.R`: creates plots, some used in manuscript.

# Main data file descriptions
- `stmf_5.rds`: Short term mortality fluctuations in 5-years age groups
- `OffsetsHMD.rds`: Exposures for mortality baseline estimation
- `total_deaths_country_b.rds`: Total number of deaths, both genders combined
- `deaths_age_country_b.rds`: Age distribution of deaths per country -- both
- `YLL_covid19_age_cummulative.rds`: Age distribution of deaths per country and YLLs per age
- `YLL_covid19_age_cummulative_countries.rds`:Age distribution of deaths per country and YLLs per age ++ cut offs
- `YLL_covid19_complete.rds`: YLL losts per death, rates, absolutes both gendered at not
- `YLL_lost_by_age_global.rds`: YLL lost per age all countries combined, then at cut offs
- `YLL_lost_by_age_global_cut_offs.rds`
- `Age_death_descriptives_all.rds`: Average age at which people die from covid gendered and not
- `YLL_other_causes_comparison.rds`: Comparisons to other causes
- `YLL_projections.rds`: Projections vs current
- `YLL.comparison_excess.rds`: Excess mortality
- `ages_at_death_global.rds` and `avgsd_age_death_both_global.rds`: Average age at death, age distribution at death
- `results.gender.total.rds`: gender totals

