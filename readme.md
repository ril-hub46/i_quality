# Quality of Institutions and Economic Development

This repository contains the code and report for a research project from the National Institute of Statistics and Applied Economics (INSEA, Rabat-Morocco) on the link between institutional quality and economic development.

This project was supervised by Mr. TAAMOUTI.

## Project Objective

The main objective of this project is to empirically analyze the impact of institutions on economic development, based on the seminal paper by Acemoglu, Johnson, and Robinson (2001), "The Colonial Origins of Comparative Development".

The project is divided into two main parts:
1.  A summary and synthesis of the AJR (2001) paper.
2.  An empirical replication and extension of their findings.

## Empirical Analysis

Our empirical work focuses on replicating **Table 4** from the original AJR (2001) paper.

### Data
* The primary dataset is from La Porta et al. (1999), "The Quality of Government," which was also used by AJR.
* **Modification:** The original paper excluded three countries (Hong Kong, Tanzania, Venezuela) from some regressions due to missing "log output per worker" data in the Penn World Table (PWT).
* We supplement this by sourcing the "log output per worker" variable from the **World Bank database**. This allows us to include all 64 countries in our analysis.

### Replications
1.  **Replication (1995 Data):** We first replicate the original IV regressions using 1995 data, as in the paper. Our results are highly consistent with the original findings, showing only minor differences.
2.  **Extension (2019 Data):** We then update the analysis by running the same regressions using more recent GDP data from 2019.

### Findings
Our results confirm the original conclusion: the quality of institutions (proxied by protection against expropriation risk) has a robust and significant positive impact on long-term economic performance. This relationship holds even when using updated data from 2019.

## Repository Structure

* **/data**: Contains the raw data used for the analysis.
* **/src**: Contains all R scripts for data cleaning, analysis, and replication.
* **/rapport**: Contains the final PDF report (`Rapport.pdf`) detailing the methodology and findings.
* `renv.lock`: The `renv` lockfile listing all R package dependencies required to reproduce the analysis.
* `README.md`: This file.

## How to Reproduce

1.  Clone this repository.
2.  Open the `.Rproj` file in RStudio.
3.  Run `renv::restore()` in the R console to install all required packages from the `renv.lock` file.
4.  Run the scripts in the `src/` folder to regenerate the analysis.
5.  Consult the final `rapport/Rapport.pdf` for the full results.

## Authors

* MAHAMAN LAWAL Rilwanou
* ABOUBACAR IBRAHIM Ismaila
* TRAORE Sory Ibrahim
* NENE BI Jonathan
