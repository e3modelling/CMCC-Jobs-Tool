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
energy_jobs_iiasadb_clean.R
energy_jobs_intensity.csv
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

The repository includes a VS Code task:

```text
Run OPEN-PROM Energy Jobs
```

To use it:

1. Open the repository in VS Code.
2. Go to `Terminal > Run Task...`.
3. Select `Run OPEN-PROM Energy Jobs`.
4. Paste the OPEN-PROM run folder path.
5. Select the visualization mode.

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

## Visualization Modes

`none`

Writes only the CSV results.

`total`

Creates a line plot of total energy jobs for the largest regions.

`breakdown`

Creates World-level stacked area plots by job category and by fuel.

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

Possible PNG outputs:

```text
energy_jobs_total_top_regions.png
energy_jobs_by_category_world.png
energy_jobs_by_fuel_world.png
```

`energy_jobs_results/` is ignored by Git, so generated CSV and PNG files are not committed accidentally.

## Processing Steps

The runner does the following:

1. Checks and loads required R packages.
2. Reads `reporting.mif` from the OPEN-PROM run folder.
3. Adds the `scenario_model` identifier required by the jobs workflow.
4. Loads `data/regionmappingOPDEV5.csv`.
5. Extends the repository's region mappings with the OPEN-PROM region mapping.
6. Applies OPEN-PROM compatibility adjustments.
7. Calls `compute_energy_jobs_iiasadb()`.
8. Writes the IAMC-style jobs CSV.
9. Optionally writes PNG visualizations.

## OPEN-PROM Compatibility Adjustments

Biofuels electricity variables are mapped to Biomass:

```text
Capacity|Electricity|Biofuels -> Capacity|Electricity|Biomass
Capacity Additions|Electricity|Biofuels -> Capacity Additions|Electricity|Biomass
```

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

## Git Notes

Commit the runner, VS Code task, ignore rule, and this guide:

```powershell
git add run_openprom_jobs.R .vscode/tasks.json .gitignore OPENPROM_ENERGY_JOBS_RUNNER_GUIDE.md
git commit -m "Add OPEN-PROM energy jobs runner"
git push origin master
```
