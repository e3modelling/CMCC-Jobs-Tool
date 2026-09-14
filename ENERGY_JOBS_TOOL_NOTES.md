# Energy Jobs Tool: Inputs, Units, and Adding Technologies

This repository estimates energy-sector jobs by multiplying scenario activity data by job-intensity coefficients. The main standalone workflow is in `energy_jobs_iiasadb_clean.R`; the longer `energy_jobs_iiasadb.R` adds extra fallback logic for missing capacity and capacity-addition data.

## 1. What input data is required?

The tool needs IAMC-style scenario data plus job-intensity data.

### A. Scenario input: `iamc_input_data.csv`

The scenario file must follow the IAMC format:

- `Model`
- `Scenario`
- `Region`
- `Variable`
- `Unit`
- year columns such as `2020`, `2030`, `2050`
- `scenario_model`, used internally to identify each model-scenario pair

The tool only uses a specific subset of IAMC variables:

- `Capacity|Electricity|TECHNOLOGY`
  - Used for O&M jobs.
  - Example: `Capacity|Electricity|Solar|PV`

- `Capacity Additions|Electricity|TECHNOLOGY`
  - Used for construction and manufacturing jobs.
  - Example: `Capacity Additions|Electricity|Wind|Onshore`

- `Resource|Extraction|Coal`
- `Resource|Extraction|Gas`
- `Resource|Extraction|Oil`
  - Used for fossil-fuel extraction jobs.

- `Secondary Energy|Liquids|Biomass`
- `Secondary Energy|Liquids|Oil`
  - Used for refinery / liquid-fuel jobs.

Current supported electricity technologies in the standalone script are:

- Biomass
- Coal
- Gas
- Hydro
- Nuclear
- Oil
- Solar CSP
- Solar PV
- Wind Offshore
- Wind Onshore

### B. Job-intensity input: `energy_jobs_intensity.csv`

This file provides the employment coefficient used to convert energy activity into jobs. It has:

- `iso3`: country code
- `category`: job category
- `fuel`: technology / fuel
- `value`: jobs intensity
- `energyquantity`: historical activity quantity used for weighting regional averages

The job categories are:

- `Construction`
- `Manufacturing`
- `O&M`
- `Extraction`
- `Refinery`

The script maps country-level coefficients to IAMC regions using `regmap_r5.csv` and `regmap_r10.csv`.

### C. Production-share input: `global_production_shares.csv`

This file is used for special redistribution cases:

- Solar PV manufacturing is redistributed using current global manufacturing shares.
- Nuclear extraction is redistributed using current uranium extraction shares.

## 2. What units are expected?

Important: the R code drops the `Unit` column before calculation. That means the numeric data must already be in the correct units. The code does not safely convert scenario units for you.

Expected units for scenario input:

- `Capacity|Electricity|...`
  - Expected unit: `GW`
  - Meaning: installed capacity.
  - If your data is in `MW`, divide by `1000` before using it.

- `Capacity Additions|Electricity|...`
  - Expected unit: `GW/yr`
  - Meaning: new installed capacity per year.
  - If your data is in `MW/yr`, divide by `1000`.

- `Resource|Extraction|Coal/Gas/Oil`
  - Expected unit: `EJ/yr`
  - Meaning: annual extracted primary energy.
  - If your data is in `PJ/yr`, divide by `1000`.
  - If your data is in `TWh/yr`, multiply by `0.0036` to get `EJ/yr`.

- `Secondary Energy|Liquids|Biomass/Oil`
  - Expected unit: `EJ/yr`
  - Meaning: annual liquid-fuel production / refinery activity.
  - If your data is in `PJ/yr`, divide by `1000`.
  - If your data is in `TWh/yr`, multiply by `0.0036`.

Expected units for job intensities:

- Construction: jobs per `GW/yr` of capacity additions.
- Manufacturing: jobs per `GW/yr` of capacity additions.
- O&M: jobs per `GW` of installed capacity.
- Extraction: jobs per `EJ/yr` of extracted fuel.
- Refinery: jobs per `EJ/yr` of liquid-fuel output.

The final calculation is essentially:

```text
jobs = activity quantity * jobs per unit
```

So, for example:

- O&M solar jobs = solar PV capacity in `GW` * solar PV O&M jobs per `GW`
- Wind construction jobs = wind capacity additions in `GW/yr` * wind construction jobs per `GW/yr`
- Coal extraction jobs = coal extraction in `EJ/yr` * coal extraction jobs per `EJ/yr`

The tool calculates job levels by year. Any "job fluctuation" must then be calculated by comparing job outputs across years, for example `jobs_2030 - jobs_2020` or percentage change from one year to another.

## 3. How to add a new technology

To add a new technology to the standalone IAMC workflow:

1. Add the technology name to the `tech` vector in `energy_jobs_iiasadb_clean.R`.

   Example for geothermal:

   ```r
   tech <- c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil",
             "Solar|CSP", "Solar|PV", "Wind|Offshore", "Wind|Onshore",
             "Geothermal")
   ```

2. Add matching IAMC scenario variables to `iamc_input_data.csv`.

   For geothermal, the minimum electricity variables would be:

   - `Capacity|Electricity|Geothermal`, in `GW`
   - `Capacity Additions|Electricity|Geothermal`, in `GW/yr`

3. Add job-intensity rows to `energy_jobs_intensity.csv`.

   For each relevant region/country, add rows for:

   - `Construction`
   - `Manufacturing`
   - `O&M`

   Add `Extraction` or `Refinery` only if the technology actually needs those categories.

4. If the technology name contains extra IAMC pipe levels, add a name-cleaning rule.

   The script already converts names like `Solar|PV` into `Solar (PV)` before splitting the variable name. A new nested name may need similar handling.

5. If capacity additions are sometimes missing, add a lifetime assumption in `energy_jobs_iiasadb.R`.

   The longer script can estimate capacity additions from changes in installed capacity. To do that correctly for a new technology, it needs a retirement/lifetime assumption.

6. If manufacturing is not domestic, add production-share logic.

   For technologies whose equipment is manufactured globally rather than where it is installed, add shares to `global_production_shares.csv` and extend the redistribution logic.

For the WITCH/GAMS workflow, also update `witch/mod_energy_jobs.gms` by adding the technology to `jobs_fuel`, mapping it to the WITCH model technology in `map_jobs_fuel_jreal`, and deciding which categories apply.

