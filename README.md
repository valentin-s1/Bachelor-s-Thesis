# Bachelor-s-Thesis
Data and Codes for BA

This repository contains the data, R scripts, and other materials used in my Bachelor's thesis. The thesis investigates the impact of non-citizen enfranchisement on labor market integration in Switzerland, focusing on the effects on unemployment rates among foreigners in the canton of Neuchâtel after the introduction of voting rights.

## Contents of the Package

### 1. R Scripts
- **`table_generation.R`**  
  R code to generate tables used in the thesis based on original calculations.
  
- **`parallel_trend_assumption_check.R`**  
  Code used to check the Parallel Trend Assumption for the Difference-in-Differences (DiD) methodology.

- **`DiD_and_SCM_analysis.R`**  
  Contains code for the main Difference-in-Differences (DiD) and Synthetic Control Method (SCM) analysis, along with the generation of tables and figures in the results section.

### 2. Data Files
- **Excel Files**  
  The datasets used in the thesis include variables related to unemployment rates, share of foreigners, and political party voting shares across Swiss cantons. 

  - **`unemployment_data.xlsx`**  
    Monthly unemployment rates for foreigners in Swiss cantons from 1990 to 2010.

  - **`voting_data.xlsx`**  
    Social Democratic Party (SP) vote share in cantonal parliaments.

  - **`cultural_distance_data.xlsx`**  
    Measures of cultural distance between Neuchâtel and other Swiss cantons.

  **Data Sources:**  
  - [Federal Statistical Office (BFS)](https://www.bfs.admin.ch)  
  - [Social Democratic Party (SP)](https://www.sp-ps.ch)  

### 3. PDFs
- **RMS Files**  
  PDF documents containing Research Management System (RMS) reports with detailed explanations of methodology and results.

---

## Data Availability Statement (DAS)

The data used for this thesis is publicly available from the Swiss Federal Statistical Office (BFS) and other publicly accessible sources. No data exemption has been granted for any of the datasets. The datasets can be downloaded directly from the BFS website or obtained upon request from the respective institutions.

---

## Code Execution Instructions

1. **Clone the repository** to your local machine.
2. **Install the required R packages** by running the following in R:
    ```r
    install.packages(c("tidyverse", "ggplot2", "dplyr", "lubridate", "fixest"))
    ```
3. **Run the R scripts** in the following order:
    - `table_generation.R`: For initial data preparation and table creation.
    - `parallel_trend_assumption_check.R`: To check the parallel trends assumption for the DiD analysis.
    - `DiD_and_SCM_analysis.R`: To perform the main analysis (DiD and SCM) and generate figures/tables.

---

## Computational Requirements

- **Software:** R (version 4.0.0 or higher)
- **Required R Packages:** `tidyverse`, `ggplot2`, `dplyr`, `lubridate`, `fixest`
- **Hardware Requirements:** No special hardware requirements. Any standard machine with R installed should be sufficient for running the analysis.
- **Expected Running Time:** Depending on your system, running all scripts may take a few minutes for data preparation and a longer time for DiD and SCM analyses.

---

## List of Tables and Figures

- **Tables**  
  - Table 1: Descriptive Statistics
  - Table 2: Difference-in-Differences Results
  - Table 3: Synthetic Control Method Results

- **Figures**  
  - Figure 1: Unemployment Trends for Treated and Control Groups
  - Figure 2: Synthetic Control Estimates
  
All figures and tables are saved in the `output/` directory after running the analysis scripts.

---

## Data Citations

- **Federal Statistical Office (BFS)**. (2024). *Unemployment Data 1990-2010*. Available at: [https://www.bfs.admin.ch](https://www.bfs.admin.ch).
- **Social Democratic Party (SP)**. (2024). *Voting Share Data*. Available at: [https://www.sp-ps.ch](https://www.sp-ps.ch).

---

## License

This repository is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details.
