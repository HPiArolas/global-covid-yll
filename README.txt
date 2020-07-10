# Global years lost of life lost to COVID-19

This is the *anonymized* data repository for the project "Global years of life lost to COVID-19".

All code is included in the `Code` folder and data in `Data` folder, which reproduce analysis in the project.

# References
Data are drawn from several resources.
- Global Burden of Disease Collaborative Network. Global Burden of Disease Study 2017 (GBD 2017) Results.
Seattle, United States: Institute for Health Metrics and Evaluation (IHME), 2018. Available from http://ghdx.healthdata.org/gbd-results-tool.
- P. G. T. Walker,et al., The impact of COVID-19 and strategies for mitigation and suppressionin low- and middle-income countries, Science (2020).
- T. Riffe, E. Acosta. et. al. “COVerAGE-DB: A database of COVID-19 Cases and Deaths by Age.” OSF, 15 June 2020. doi:10.17605/OSF.IO/MPWJQ.

# Instructions to reproduce:
- Download the repository and unzip it if necessary.
- Install [R (free)](https://www.r-project.org/)
- Install [R Studio (free)](https://rstudio.com/products/rstudio/download/)
- Open R Studio
  - click: `File` - `New project` - `Existing directory` - `folder of unzipped repository`
- Now you are in an R project. You can open this file in R Studio now
- open `Code/master.R` source the files in order, or step through / inspect the code in order. 
- this results in calculations necessary to produce the figures and tables of the manuscript.

# Code file descriptions
- `master.R` : A master script to run the analyses in order. This preloads all packages. Some installation may be required.
- `A - 1_Mortality_baseline.R`: mortality baseline etimation from weekly mortality.
- `A - 2_Excess_mortality_2020`: calculations of 2020 excess mortality.
- `B - Excess YLL.R`: calculations of YLL for excess mortality.
- `B - Other Causes YLL.R`: calculations of YLL for other causes of mortality.
- `B - Projected YLL.R`: calculations of YLL for projected scenarios.
- `C - Compilation final measures.R`: creates measures cited throughout manuscript.
- `C - Final tables.R`: creates tables in manuscript, and a few extra.
- `C - Figures.R` and `Example_figures.rmd`: creates plots, some used in manuscript.

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

This code may require some cleaning in places. If a necessary file is missing please inform us (via the editors as it were), and it will be added to the repository ASAP.

