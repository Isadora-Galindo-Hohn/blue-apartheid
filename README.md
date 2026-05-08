# Blue apartheid
This project investigated differenses in water access between different income brackets and groups.

## Data
Data was accuired from the South African census form and quality of life forms by Gauteng manicupality.
The raw data from these forms where processed and cleaned up using excel.
Using the R scripts in this repo we can then process the data also using the borders/wards data from the South African goverment. New [ward boarders](https://spatialhub-mdb-sa.opendata.arcgis.com) were set 2009, 2011, 2016 and 2020, so that had to be taken into account.

![Map showing income brackets compared to ward of the Gauteng region from 2024.](/images/Map_2024_income_bracket.png)

## Graphs
Using R we could clean up the rest of the data to prepare for showing as graphs.
These datapoints where from
- 2009
- 2011
- 2014
- 2016
- 2018
- 2022
- 2024
And graphs where created for these data points that where derived from the form answers:
- Dominant population group
- Average access to water
- Average income brackets
- Average non white population
- Share of households with over 200 meters to nearest water source
- Population density

![Graphs showing differences in the reported income brackets from year to year](/images/combined_income_distribution_by_year.png)

## Dependencies
All dependencies needed to run the project are documented in `renv.lock` you can use renv to quickly install all needed deps.

![Graph depicting mean income for different population groups each year](/images/income_mean_sd_ribbon_by_group_year.png)
