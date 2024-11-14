# Bachelor-s-Thesis
Data and Codes for BA

This repository contains all the necessary data, R scripts, and other materials for my Bachelor's thesis. The focus of the thesis is on the effect of non-citizen enfranchisement on labor market integration in Switzerland.

## Project Structure
The repository is organized into the following sections:

### 1. R Scripts
- **File 1: `table_generation.R`**  
  Contains R code for generating tables used in the thesis, including calculations and derived statistics from the data.

- **File 2: `parallel_trend_assumption_check.R`**  
  Includes the code for testing the Parallel Trend Assumption using the Difference-in-Differences (DiD) methodology.

- **File 3: `DiD_and_SCM_analysis.R`**  
  Contains the code for the main Difference-in-Differences (DiD) and Synthetic Control Method (SCM) analysis, including the generation of figures and tables used in the results section.

### 2. Data Files
- **Excel Files**  
  The dataset includes various variables related to unemployment rates, share of foreigners, and political party voting shares across different cantons in Switzerland. The data is sourced from the Federal Statistical Office (BFS) and other relevant institutions.  
  *Sources:*  
  - [Federal Statistical Office (BFS)](https://www.bfs.admin.ch)

  - **`unemployment_data.xlsx`**  
    Contains monthly unemployment rates for foreigners in Swiss cantons from 1990 to 2010.

  - **`voting_data.xlsx`**  
    Includes voting data on Social Democratic Party (SP) share in cantonal parliaments.

  - **`cultural_distance_data.xlsx`**  
    Contains cultural distance measures between Neuchâtel and other Swiss cantons.

### 3. PDFs
- **RMS Files**  
  PDFs of relevant RMS (Research Management System) reports, including detailed explanations of the methodology, results, and other supporting documents.

### 4. Thesis Topic and Background
This project investigates the impact of non-citizen enfranchisement on labor market integration in Switzerland, with a particular focus on the effects on foreigner unemployment rates following the introduction of voting rights in the canton of Neuchâtel in 2002. The study uses the Difference-in-Differences (DiD) and Synthetic Control Method (SCM) to analyze the effect of this policy reform.

## How to Use
To replicate the analysis or extend the research:
1. Clone the repository to your local machine.
2. Install necessary R packages: `tidyverse`, `ggplot2`, `dplyr`, `lubridate`, `fixest`, etc.
3. Run the scripts in the order of their presentation (starting with `table_generation.R` for any necessary data preparation).

## Citation
If you use any data or methods from this repository, please cite the source as follows:

Schnellmann, Valentin. (2024). *Bachelor's Thesis: Non-citizen Enfranchisement and Labor Market Integration in Switzerland*. University of St. Gallen.

## License
This repository is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
