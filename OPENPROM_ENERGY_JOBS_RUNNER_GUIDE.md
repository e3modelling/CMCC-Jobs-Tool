# OPEN-PROM Energy Jobs Runner Guide

This guide explains how to run `run_openprom_jobs.R`, what inputs it expects, which modes it supports, and where it saves CSV and PNG outputs.

## Purpose

`run_openprom_jobs.R` is a convenience runner for applying the standalone energy jobs workflow to an OPEN-PROM run folder.

It reads:

```text
reporting.mif
```

from an OPEN-PROM run directory, prepares the data for the existing energy jobs calculation, runs `compute_energy_jobs_iiasadb()` from `energy_jobs_iiasadb_clean.R`, writes an IAMC-style CSV output, and optionally creates PNG visualizations.

## Required Inputs

The OPEN-PROM run folder must contain:

```text
reporting.mif
data/regionmappingOPDEV5.csv
```

The repository must contain:

```text
run_openprom_jobs.R
compare_openprom_scenarios.R
energy_jobs_iiasadb_clean.R
energy_jobs_intensity.csv
daccs_jobs_assumptions.csv
beccs_jobs_assumptions.csv
global_production_shares.csv
regmap_r5.csv
regmap_r10.csv
```

## Required R Packages

The runner checks and initializes these packages automatically:

```text
dplyr
tidyr
readr
stringr
ggplot2
RColorBrewer
readxl
zoo
```

If any package is missing, the script attempts to install it from CRAN using:

```text
https://cloud.r-project.org
```

## Running From VS Code

The repository includes two VS Code tasks:

```text
Run OPEN-PROM Energy Jobs
Compare OPEN-PROM Scenarios
```

To use it:

1. Open the repository in VS Code.
2. Go to `Terminal > Run Task...`.
3. Select `Run OPEN-PROM Energy Jobs`.
4. Paste the OPEN-PROM run folder path.
5. Select the visualization mode.

The comparison task reads the scenario CSV files already available in `energy_jobs_results/`.
It first displays every eligible result as a numbered list with the scenario code, the descriptive
name used in UPTAKE Deliverable D4.3, and the complete result name. Enter at least two numbers
separated by commas, such as `1,3,5`. The task compares 2030, 2050, and 2070, so no year selection
is required. S6 is omitted from the list. At least two completed scenario results are required.

Example run folder:

```text
C:\Users\Plessias\Downloads\OneDrive_1_7-24-2026\DAILY_NPi_2026-07-24_15-06-03
```

## Running From Terminal

Run all commands from the repository root.

Basic run:

```powershell
Rscript run_openprom_jobs.R "C:\path\to\OPEN-PROM-run-folder"
```

Run with all visualizations:

```powershell
Rscript run_openprom_jobs.R "C:\path\to\OPEN-PROM-run-folder" --visualize=all
```

Select a DACCS employment case:

```powershell
Rscript run_openprom_jobs.R "C:\path\to\OPEN-PROM-run-folder" --visualize=all --daccs-case=low
Rscript run_openprom_jobs.R "C:\path\to\OPEN-PROM-run-folder" --visualize=all --daccs-case=reference
Rscript run_openprom_jobs.R "C:\path\to\OPEN-PROM-run-folder" --visualize=all --daccs-case=high
```

The default is `reference`.

Select a BECCS employment case:

```powershell
Rscript run_openprom_jobs.R "C:\path\to\OPEN-PROM-run-folder" --visualize=all --beccs-case=reference
```

The BECCS runner uses `reference` by default. The `low` and `high` cases remain available for
sensitivity analysis by changing the argument to `--beccs-case=low` or `--beccs-case=high`.
Section [BECCS Jobs Methodology](#beccs-jobs-methodology) documents the source and derivation
of all three cases.

Run only the total jobs plot:

```powershell
Rscript run_openprom_jobs.R "C:\path\to\OPEN-PROM-run-folder" --visualize=total
```

Run only breakdown plots:

```powershell
Rscript run_openprom_jobs.R "C:\path\to\OPEN-PROM-run-folder" --visualize=breakdown
```

Run without plots:

```powershell
Rscript run_openprom_jobs.R "C:\path\to\OPEN-PROM-run-folder" --visualize=none
```

Run interactively:

```powershell
Rscript run_openprom_jobs.R
```

Interactive mode asks for:

```text
OPEN-PROM run folder path
Visualization mode [none/total/breakdown/all]
```

## Comparing Scenarios

Run the comparison from the repository root after calculating at least two scenarios:

```powershell
Rscript compare_openprom_scenarios.R "energy_jobs_results"
```

To use the same numbered selection shown by the VS Code task:

```powershell
Rscript compare_openprom_scenarios.R "energy_jobs_results" --interactive
```

Select years or a subset of scenarios with command-line options:

```powershell
Rscript compare_openprom_scenarios.R "energy_jobs_results" --years=2030,2050,2070 --include=S0,S1,S3,S4
```

The runner identifies result files ending in `_energy_jobs_iamc.csv`. The chart legend combines each
scenario code with the descriptive name used in Deliverable D4.3:

```text
S0 - NPI portfolio reference
S1 - 1.5°C full portfolio
S2 - BECCS-only engineered portfolio
S3 - BECCS CAPEX support
S4 - DACCS CAPEX support
S5 - BECCS support + BIO100
```

S6 is excluded from the selection and from all comparison outputs. Its available implementation is
closely aligned with the biomass-constrained S5 case and does not provide a sufficiently distinct
experiment for the employment comparison. The manifest records the descriptive label, complete run
name, and source file for every included scenario.

The comparison produces two bar charts:

```text
scenario_comparison_total_jobs_world.png
scenario_comparison_cdr_jobs.png
```

The first chart compares total World energy employment in millions of jobs. The consolidated CDR
chart compares total DACCS and BECCS employment for the World, EU, China, and the US. Both charts
place 2030, 2050, and 2070 on the horizontal axis and distinguish the selected scenarios by color.
Each geographic row of the CDR chart uses one scale shared by DACCS and BECCS, while scales can
differ among regions whose employment magnitudes differ substantially.

EU employment is calculated as the sum of EU27 country results, preserving the country-specific
labor intensities used by the employment model. China and the US use the `CHA` and `USA` rows.
The runner also writes the aggregated values underlying each chart as CSV files. The World and
regional CDR CSV files retain the Construction, Manufacturing, and O&M categories even though the
consolidated comparison figure reports their total.

## Visualization Modes

`none`

Writes only the CSV results.

`total`

Creates a grouped bar chart of total energy jobs for the largest regions in 2025, 2030, 2050, and
2070.

`breakdown`

Creates World-level stacked bar charts by job category and by fuel for 2025, 2030, 2050, and 2070.
It also creates the DACCS and BECCS charts described in the technology sections below.

`all`

Creates every available visualization.

## Output Location

All generated outputs are written inside the repository under:

```text
energy_jobs_results/
```

If this folder does not exist, the runner creates it automatically.

For a run folder named:

```text
DAILY_NPi_2026-07-24_15-06-03
```

the CSV output is:

```text
energy_jobs_results/DAILY_NPi_2026-07-24_15-06-03_energy_jobs_iamc.csv
```

If visualizations are enabled, PNGs are written to:

```text
energy_jobs_results/DAILY_NPi_2026-07-24_15-06-03_plots/
```

Scenario-comparison outputs are written to:

```text
energy_jobs_results/scenario_comparison/
```

Possible PNG outputs:

```text
energy_jobs_total_top_regions.png
energy_jobs_by_category_world.png
energy_jobs_by_fuel_world.png
energy_jobs_cdr_selected_years.png
energy_jobs_cdr_key_regions_selected_years.png
```

`energy_jobs_results/` is ignored by Git, so generated CSV and PNG files are not committed accidentally.

## Processing Steps

The runner does the following:

1. Checks and loads required R packages.
2. Reads `reporting.mif` from the OPEN-PROM run folder.
3. Adds the `scenario_model` identifier required by the jobs workflow.
4. Loads `data/regionmappingOPDEV5.csv`.
5. Extends the repository's region mappings with the OPEN-PROM region mapping.
6. Applies OPEN-PROM compatibility adjustments and separates bioenergy capacity by CCS status.
7. Prepares DACCS and incremental BECCS CCS activity rows.
8. Calls `compute_energy_jobs_iiasadb()`.
9. Writes the IAMC-style jobs CSV.
10. Optionally writes PNG visualizations.

## OPEN-PROM Compatibility Adjustments

When OPEN-PROM reports both CCS components, the runner uses the disaggregated electricity variables:

```text
Capacity|Electricity|Biofuels|w/o CCS -> Capacity|Electricity|Biomass
Capacity Additions|Electricity|Biofuels|w/o CCS -> Capacity Additions|Electricity|Biomass
Capacity|Electricity|Biofuels|w/ CCS -> Capacity|Electricity|BECCS Base
Capacity Additions|Electricity|Biofuels|w/ CCS -> Capacity Additions|Electricity|BECCS Base
```

The runner verifies that the two components sum to the aggregate `Biofuels` values for every
region and year, allowing for the four-decimal rounding precision of the OPEN-PROM output. It then
removes the aggregate capacity rows from the jobs input. This preserves
the original Biomass activity while assigning the portion equipped with CCS to the BECCS result.
Runs that lack a complete CCS disaggregation retain the aggregate Biomass calculation and issue a warning.

Aggregate solar capacity additions are split into PV and CSP using installed-capacity shares:

```text
Capacity Additions|Electricity|Solar
```

Aggregate wind capacity additions are split into onshore and offshore using installed-capacity shares:

```text
Capacity Additions|Electricity|Wind
```

Fossil extraction is proxied from primary energy:

```text
Primary Energy|Coal -> Resource|Extraction|Coal
Primary Energy|Gas  -> Resource|Extraction|Gas
Primary Energy|Oil  -> Resource|Extraction|Oil
```

The conversion used is:

```text
1 Mtoe = 0.041868 EJ
```

Liquid fuel refinery activity is proxied from:

```text
Secondary Energy|Liquids -> Secondary Energy|Liquids|Oil
```

This proxy is also converted from `Mtoe` to `EJ`.

## Unit Assumptions

The jobs workflow expects the scenario activity data to be in these units:

```text
Capacity|Electricity|...                  GW
Capacity Additions|Electricity|...        GW/yr
Resource|Extraction|Coal/Gas/Oil          EJ/yr
Secondary Energy|Liquids|Oil              EJ/yr
```

The runner converts the OPEN-PROM primary-energy and liquids proxy rows from `Mtoe` to `EJ`. It does not perform general unit conversion for every possible variable.

## Adding a New Technology

To add a new technology, the jobs workflow needs three things:

1. The technology must be listed in the R workflow.
2. The scenario data must contain the matching IAMC activity variables.
3. The jobs-intensity database must contain coefficients for the same technology name.

The examples below use `Geothermal`.

### 1. Add the Technology to the Workflow

Edit `energy_jobs_iiasadb_clean.R`.

For the OPEN-PROM task runner, update only the `tech` vector inside `compute_energy_jobs_iiasadb()` in `energy_jobs_iiasadb_clean.R`.

This is the workflow used by:

```text
run_openprom_jobs.R
```

You may see other `tech <- c(...)` vectors in files such as:

```text
energy_jobs_iiasadb.R
capacity_led.R
cut_scenarios.R
```

Those belong to legacy analysis scripts or helper workflows. They are not used by the OPEN-PROM task runner. Only update them if you explicitly plan to run those scripts too.

Find this vector in `energy_jobs_iiasadb_clean.R`:

```r
tech <- c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil",
          "Solar|CSP", "Solar|PV", "Wind|Offshore", "Wind|Onshore")
```

Add the new technology:

```r
tech <- c("Biomass", "Coal", "Gas", "Hydro", "Nuclear", "Oil",
          "Solar|CSP", "Solar|PV", "Wind|Offshore", "Wind|Onshore",
          "Geothermal")
```

For a normal electricity-generation technology, this makes the workflow look for:

```text
Capacity|Electricity|Geothermal
Capacity Additions|Electricity|Geothermal
```

### 2. Make Sure the Scenario Data Contains the Technology

The OPEN-PROM `reporting.mif` must contain the matching variables:

```text
Capacity|Electricity|Geothermal
Capacity Additions|Electricity|Geothermal
```

Expected units:

```text
Capacity|Electricity|Geothermal             GW
Capacity Additions|Electricity|Geothermal   GW/yr
```

If these variables are not present in `reporting.mif`, the jobs workflow has no activity data for the new technology and cannot calculate jobs for it.

### 3. Add Jobs-Intensity Coefficients

Edit `energy_jobs_intensity.csv`.

For a standard electricity technology, add rows for:

```text
Construction
Manufacturing
O&M
```

The `fuel` name must match the cleaned technology name used by the workflow.

For `Geothermal`, the `fuel` value should be:

```text
Geothermal
```

Example structure:

```text
iso3,category,fuel,value,energyquantity
USA,Construction,Geothermal,...
USA,Manufacturing,Geothermal,...
USA,O&M,Geothermal,...
```

The file already contains country-level coefficients. The workflow aggregates them to model regions using the region mappings.

### 4. Add OPEN-PROM Name Mapping if Needed

Sometimes OPEN-PROM reports a technology with a different name than the jobs workflow expects.

For example, OPEN-PROM may report:

```text
Capacity|Electricity|Geothermal and other renewable sources
Capacity Additions|Electricity|Geothermal and other renewable sources
```

while the jobs workflow expects:

```text
Capacity|Electricity|Geothermal
Capacity Additions|Electricity|Geothermal
```

In that case, edit `run_openprom_jobs.R`, inside:

```r
rename_openprom_variables <- function(data) {
```

and add mapping rules:

```r
data$Variable[data$Variable == "Capacity|Electricity|Geothermal and other renewable sources"] <- "Capacity|Electricity|Geothermal"
data$Variable[data$Variable == "Capacity Additions|Electricity|Geothermal and other renewable sources"] <- "Capacity Additions|Electricity|Geothermal"
```

### 5. Handle Nested Technology Names Carefully

The workflow can handle existing nested names such as:

```text
Solar|PV
Solar|CSP
Wind|Onshore
Wind|Offshore
```

because the script explicitly cleans them before splitting the IAMC variable name.

If the new technology has extra `|` levels, such as:

```text
Capacity|Electricity|Hydrogen|Turbine
```

then add a cleaning rule before the `separate()` step in `energy_jobs_iiasadb_clean.R`.

For example:

```r
mutate(Variable = str_replace(Variable, 'Hydrogen[|]Turbine', 'Hydrogen (Turbine)'))
```

The corresponding `fuel` value in `energy_jobs_intensity.csv` should then be:

```text
Hydrogen (Turbine)
```

### 6. Add Extraction or Refinery Logic Only if Relevant

Most electricity technologies only need:

```text
Construction
Manufacturing
O&M
```

If the new technology is a fuel extraction or refinery activity, also update the relevant vectors in `energy_jobs_iiasadb_clean.R`:

```r
fuelprod <- c("Resource|Extraction|Coal",
              "Resource|Extraction|Gas",
              "Resource|Extraction|Oil")
```

or:

```r
refine <- c("Secondary Energy|Liquids|Biomass",
            "Secondary Energy|Liquids|Oil")
```

Use this only when the technology is genuinely represented by extraction or refinery activity, not by power-sector capacity.

### 7. Add Production-Share Logic Only if Needed

Some jobs are redistributed globally instead of staying in the region where capacity is installed.

The existing workflow does this for cases such as solar PV manufacturing and nuclear extraction using:

```text
global_production_shares.csv
```

If the new technology has globally traded manufacturing or extraction activity, add production-share rows to `global_production_shares.csv` and extend the redistribution logic in `energy_jobs_iiasadb_clean.R`.

If the jobs should stay domestic, no production-share logic is needed.

### 8. Test the New Technology

After editing the files, run:

```powershell
Rscript run_openprom_jobs.R "C:\path\to\OPEN-PROM-run-folder" --visualize=all
```

Then check:

```text
energy_jobs_results/<run_name>_energy_jobs_iamc.csv
```

Look for output variables such as:

```text
Energy Jobs|Construction|Geothermal|Total
Energy Jobs|Manufacturing|Geothermal|Total
Energy Jobs|O&M|Geothermal|Total
```

If these rows are missing, check:

1. Whether the technology is included in the `tech` vector.
2. Whether `reporting.mif` contains matching `Capacity` and `Capacity Additions` rows.
3. Whether `energy_jobs_intensity.csv` contains matching `fuel` and `category` rows.
4. Whether the technology name needs an OPEN-PROM mapping rule.

## DACCS Jobs Methodology

DACCS activity enters the workflow through standardized internal rows. OPEN-PROM models DACCS outside
the electricity technology vector, while the employment calculation still uses the common job-intensity
merge, technical-progress adjustment, regional convergence, jobs equation, and IAMC output.

### OPEN-PROM Activity Variable

The preferred input is:

```text
Carbon Removal|Geological Storage|Direct Air Capture   Mt CO2/yr
```

The runner uses these fallbacks, in order, if the preferred variable is absent:

```text
Carbon Capture|Geological Storage|Direct Air Capture
Carbon Capture|Direct Air Capture
```

The unit must be exactly `Mt CO2/yr`. When multiple variables are available, the runner warns if their values differ and uses the highest-priority variable.

### Operating Capacity

OPEN-PROM variable `V06CapCDR` is the installed annual capture-capacity stock. The emissions module
subtracts it directly from annual emissions, and the IAMC reporting layer expresses it in `Mt CO2/yr`.
The employment calculation therefore uses the reported value directly:

```text
operating_capacity[t] = reported_OPEN-PROM_capacity[t]
```

No additional capacity-factor conversion is applied. Applying one would increase a quantity that already
represents annual capture capacity. The `0.85` factor used in a NETL reference plant remains documented in
`daccs_jobs_assumptions.csv` for analyses based on nameplate equipment capacity or other external datasets:

```text
https://www.netl.doe.gov/projects/files/DirectAirCaptureCaseStudiesSolventSystem_083122.pdf
```

### Capacity Additions

OPEN-PROM accumulates `V06CapCDR` from the previous year's stock and the model's new CDR capacity. It
does not remove retired DACCS capacity from this stock. The runner follows that model boundary and derives
commissioned capacity from the positive annual change:

```text
additions[t] = max(operating_capacity[t] - operating_capacity[t - 1], 0)
```

The first reported year is treated as an existing stock and receives no construction activity. The runner
also verifies that the input has consecutive annual observations before calculating additions.

A 20-year DAC plant life is retained in `daccs_jobs_assumptions.csv` as an optional replacement
sensitivity. WRI uses this lifetime in its assessment of DAC material requirements:

```text
https://publications.wri.org/scaling-dac-in-the-us
```

Introducing replacement cohorts would add construction activity that OPEN-PROM does not represent and
should therefore be reported separately from the central model-consistent result.

### Construction Activity

Rhodium reports average annual employment over an approximately five-year construction period. The runner therefore calculates the capacity simultaneously under construction:

```text
capacity_under_construction[t] = sum(additions commissioned from t to t + T - 1)
```

where `T = 5` years in the reference assumptions. Construction and Manufacturing use this quantity, while O&M uses operating capacity.

Because the OPEN-PROM horizon ends in 2100, construction employment near the endpoint includes only projects commissioned by 2100. It does not assume unreported projects after the model horizon, so Construction and Manufacturing in 2100 should be interpreted as a model-horizon lower bound.

### Employment Intensities

The reference case is based on a Rhodium assessment of a commercial DAC facility with `0.5 Mt CO2/yr` capacity:

```text
https://rhg.com/research/direct-air-capture-workforce-development/
```

The published values and conversions are:

```text
Construction:  765 / 0.5 = 1,530 jobs per (Mt CO2/yr capacity under construction)
Manufacturing: 450 / 0.5 =   900 jobs per (Mt CO2/yr capacity under construction)
O&M:           340 / 0.5 =   680 jobs per (Mt CO2/yr operating capacity)
```

`Manufacturing` represents the source's supply-chain share, which includes equipment manufacturing, freight, and related activities. It is mapped to Manufacturing to preserve the categories already used by the jobs tool.

The `low` and `high` cases use the newer Rhodium ranges for a first-of-a-kind 0.5 Mt CO2/yr DACCS project, including CO2 injection and storage:

```text
https://rhg.com/research/the-benefits-of-innovation-an-assessment-of-the-economic-opportunities-of-highly-durable-carbon-dioxide-removal/
```

The published project-level and supply-chain components are divided directly by `0.5 Mt CO2/yr`; no
additional allocation shares are imposed. These cases are sensitivity scenarios rather than statistical
confidence intervals. Their full values, units, scope, calculations, and source URLs are recorded in
`daccs_jobs_assumptions.csv`.

### Regional Productivity and Convergence

The Rhodium factors are US-based. For each job category, the runner derives a regional multiplier from the existing jobs-intensity database:

```text
relative_intensity[r,f,c] = intensity[r,f,c] / median_region(intensity[f,c])
regional_multiplier[r,c] = median_fuel(relative_intensity[r,f,c])
normalized_multiplier[r,c] = regional_multiplier[r,c] / regional_multiplier[USA,c]
DACCS_intensity[r,c] = US_DACCS_intensity[c] * normalized_multiplier[r,c]
```

This transfers the tool's existing regional labor-productivity pattern to DACCS without inventing country-level DACCS observations. The resulting DACCS coefficients then use the existing convergence-to-minimum logic through 2100.

### Technical Progress

The reference calculation sets DACCS annual technical progress to `0.00`. OPEN-PROM's learning parameter
describes costs, while the available sources provide no matching labor-productivity trajectory.

To run the same sensitivity used by solar and wind, change the DACCS row in the `techprogress` table in `energy_jobs_iiasadb_clean.R` from:

```text
DACCS, 0.00
```

to:

```text
DACCS, 0.0075
```

### DACCS Outputs

The IAMC result contains:

```text
Energy Jobs|Construction|DACCS|Total
Energy Jobs|Manufacturing|DACCS|Total
Energy Jobs|O&M|DACCS|Total
```

With `--visualize=breakdown` or `--visualize=all`, the runner also writes:

```text
energy_jobs_cdr_selected_years.png
```

The chart shows DACCS and BECCS jobs by category in 2030, 2050, 2070, and 2100. Both panels use one
y-axis scale, expressed in thousands of jobs, to support direct comparison.

## BECCS Jobs Methodology

### Scope

The current BECCS calculation covers bioenergy used for electricity generation with carbon capture.
OPEN-PROM also reports biomass capture associated with hydrogen, liquids, heat, and demand sectors.
Those activities require their own technology and employment assumptions and remain outside this calculation.

The power-sector calculation uses:

```text
Capacity|Electricity|Biofuels|w/ CCS                 GW
Capacity Additions|Electricity|Biofuels|w/ CCS       GW/yr
Carbon Capture|Energy|Supply|Electricity|Biofuels    Mt CO2/yr
```

The runner validates each unit before calculating employment.

### Treatment of Existing Biomass Jobs

The previous Biomass result used aggregate `Biofuels` capacity. OPEN-PROM defines that aggregate as the
sum of capacity with and without CCS. The runner now assigns these components to separate internal fuels:

```text
Biomass     = Biofuels|w/o CCS
BECCS Base  = Biofuels|w/ CCS
```

Both components use the existing regional Biomass employment factors. Construction and Manufacturing
continue to use capacity additions, while O&M continues to use installed capacity. Technical progress,
regional convergence, and the final jobs equation remain unchanged.

The disaggregation satisfies the following conservation identity for each region, year, and job category:

```text
jobs_biomass_without_ccs + jobs_beccs_base = jobs_biomass_before_disaggregation
```

`Secondary Energy|Liquids|Biomass` continues to feed the Refinery category under Biomass. The BECCS
calculation described here does not change liquid-biofuel employment.

### Incremental CCS Component

The BECCS result includes a second internal component named `BECCS CCS`. It represents employment
associated with capture equipment, engineering, the investment supply chain, and the source's allocated
CO2 injection and storage activity. The final reported BECCS jobs combine the two components:

```text
total BECCS jobs = BECCS Base jobs + BECCS CCS jobs
```

Consequently, the change in total energy employment equals the incremental `BECCS CCS` component.
The base-plant component was already present in the aggregate Biomass result.

### Employment Source and Cases

The employment factors come from Rhodium Group's *The Benefits of Innovation: An Assessment of the
Economic Opportunities of Highly Durable Carbon Dioxide Removal*:

```text
https://rhg.com/research/the-benefits-of-innovation-an-assessment-of-the-economic-opportunities-of-highly-durable-carbon-dioxide-removal/
```

Rhodium evaluates an initial commercial-scale BECCS project with 0.5 Mt CO2/yr of capture capacity,
a four-year construction period, and an expected operating life of 20-30 years. Its Figure 3 reports:

| Employment component | Low | High | Unit in the source |
| --- | ---: | ---: | --- |
| Project-level investment | 210 | 340 | average annual jobs per 0.5 Mt CO2/yr project |
| Investment supply chain | 210 | 320 | average annual jobs per 0.5 Mt CO2/yr project |
| Project-level O&M | 30 | 90 | ongoing jobs per 0.5 Mt CO2/yr project |
| O&M supply chain | 30 | 70 | ongoing jobs per 0.5 Mt CO2/yr project |

Dividing by 0.5 gives the normalized factors used by the runner:

| Tool category | Low | Reference | High | Model unit |
| --- | ---: | ---: | ---: | --- |
| Construction | 420 | 550 | 680 | jobs per Mt CO2/yr capture capacity under construction |
| Manufacturing | 420 | 530 | 640 | jobs per Mt CO2/yr capture capacity under construction |
| O&M | 60 | 120 | 180 | jobs per Mt CO2/yr operating capture |

The reference values are arithmetic midpoints of the normalized low and high estimates. They are
modelling assumptions derived from the published range. Rhodium does not report a separate reference case.

The main calculation uses project-level O&M. This choice limits overlap with the existing Biomass
coefficient, whose source does not separate plant operations from biomass supply and transport. The
ongoing supply-chain factors are stored in `beccs_jobs_assumptions.csv` as
`optional_ongoing_supply_chain`. They remain excluded from the runner and can support a broader-scope
sensitivity calculation.

Rhodium's BECCS estimates describe complete projects rather than an isolated capture retrofit. The
`BECCS Base` component already represents the host bioenergy plant, so some overlap may remain in the
project-level CCS proxy even after excluding ongoing supply-chain employment. The result retains the two
components separately in `energy_jobs_components` during execution to make this scope transparent. A
future capture-only employment source could replace the CCS proxy without changing the activity pipeline.

Rhodium's totals include employment associated with CO2 injection and storage. The runner therefore
does not add a separate storage factor.

### Capture Capacity Under Construction

OPEN-PROM reports BECCS electrical capacity, capacity additions, and annual power-sector biomass capture.
The runner first derives a region- and year-specific capture rate:

```text
capture_rate[t] = power_sector_biomass_capture[t] / BECCS_electrical_capacity[t]
```

It then converts electrical capacity additions into commissioned annual capture capacity:

```text
commissioned_capture_capacity[t] = BECCS_capacity_additions[t] * capture_rate[t]
```

Rhodium reports average annual investment employment over four construction years. The activity used for
Construction and Manufacturing is therefore:

```text
capture_capacity_under_construction[t]
  = sum(commissioned capture capacity from t through t + 3)
```

O&M uses annual power-sector biomass capture directly:

```text
BECCS_CCS_O&M_jobs[t]
  = power_sector_biomass_capture[t] * O&M_jobs_per_unit[t]
```

Construction and Manufacturing near 2100 include projects commissioned within the OPEN-PROM horizon.
The corresponding employment values are lower-bound estimates because the input contains no commissioning
activity after 2100.

### Regional Intensities and Technical Progress

Rhodium's employment factors represent US projects. The runner applies the same normalized regional
labor-intensity pattern used for DACCS, derived from the existing jobs database and normalized to the US.
The resulting BECCS CCS factors then follow the standard regional convergence calculation through 2100.

`BECCS Base` retains the existing Biomass technical-progress value of `0.00`. `BECCS CCS` also uses `0.00`
in the reference calculation because the source provides no annual labor-productivity trajectory. A code
comment identifies `0.0075` as an optional sensitivity consistent with the existing solar and wind setting.

### BECCS Outputs and Charts

The IAMC result contains:

```text
Energy Jobs|Construction|BECCS|Total
Energy Jobs|Manufacturing|BECCS|Total
Energy Jobs|O&M|BECCS|Total
```

With `--visualize=breakdown` or `--visualize=all`, the runner writes two CDR bar charts:

```text
energy_jobs_cdr_selected_years.png
energy_jobs_cdr_key_regions_selected_years.png
```

The first chart reports World DACCS and BECCS employment in 2030, 2050, 2070, and 2100. The regional
chart reports DACCS and BECCS employment for EU, China, and US in 2030, 2040, 2050, 2060, and 2070.
China and US use the OPEN-PROM `CHA` and `USA` results. EU employment is the sum of the EU27
member-country results, preserving each country's jobs intensity. In the sample OPEN-PROM data, the
same EU27 set reconstructs the reported aggregate `EU` capacity and capture series within their
four-decimal rounding precision.

## Git Notes

Commit the BECCS implementation and its documented assumptions:

```powershell
git add run_openprom_jobs.R energy_jobs_iiasadb_clean.R beccs_jobs_assumptions.csv OPENPROM_ENERGY_JOBS_RUNNER_GUIDE.md
git commit -m "Add BECCS jobs calculation"
git push origin BECCS_add
```
