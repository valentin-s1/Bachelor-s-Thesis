# Bachelor-s-Thesis
Data and Codes for BA

This repository contains the data, R scripts, and other materials used in my Bachelor's thesis. The thesis investigates the impact of non-citizen enfranchisement on labor market integration in Switzerland, focusing on the effects on unemployment rates among foreigners in the canton of Neuchâtel after the introduction of voting rights.

## Contents of the Package

### 1. R Scripts
- **`table_generation.R`**  
  R code to generate tables used in the thesis based on own calculations.
  
- **`parallel_trend_assumption_check.R`**  
  Code used to check the Parallel Trend Assumption for the Difference-in-Differences (DiD) methodology.

- **`DiD_and_SCM_analysis.R`**  
  Contains code for the main Difference-in-Differences (DiD) and Synthetic Control Method (SCM) analysis, along with the generation of tables and figures in the results and Appendix section.

### 2. Data Files
- **Excel Files**  
  The datasets used in the thesis include variables related to unemployment rates, share of foreigners, and political party voting shares across Swiss cantons. 

  - **`Clean_Arbeitslosenquote.xlsx`**  
    Monthly unemployment rates for foreigners in Swiss cantons from 1993 to 2009 (SECO, 2020).
  
  - **`Cultural Distance (share) 2010-2023.xlsx`**  
    The data covers the years 2010 to 2023 and includes the number of foreigners per canton as well as their composition, i.e., the number of foreigners by nationality 
    (BFS, 2021). Cultural distance shares were calculated based on the World Values Survey, which classifies countries along two dimensions and allows for the creation of 
    culturally similar or distant groups relative to Switzerland (Inglehart & Baker, 2000). The final version, which consists only of the shares for 2010, is in 
    **`Clean_Cultural_Distance_share_2010.xlsx`**.

  - **`Clean_SP_Share.xlsx`**  
    This dataset includes the SP vote share, representing the voting strength of the Social Democratic Party (SP) at the cantonal level, calculated as the percentage of 
    valid votes received by the SP relative to the total valid votes cast. The measure is standardized across cantons by considering fictitious voters, which adjusts for 
    differences in the number of seats per canton to enable inter-cantonal comparisons (BFS, 2015).

  - **`Clean_Foreigner_Share.xlsx`**  
    This dataset contains annual foreigner shares per canton from 1990 to 2010 (BFS, 2021).

  - **`Clean_Income_per_Capita_1993-2010.xlsx`**  
     Contains data on average income per canton for the years 1993-2010 for legal entities via direct federal tax (ESTV, 2029).

  **Data Sources:**  
  - See below.

### 3. HTML files
- HTML files created from .R Markdown (.RMD) documents that contain the same code as the corresponding R files but are rendered in HTML format, displaying both the code and the generated outputs directly.

---

## Data Availability Statement (DAS)

The data used for this thesis is publicly available from the Swiss Federal Statistical Office (BFS) and other publicly accessible sources. No data exemption has been granted for any of the datasets. The datasets can be downloaded directly from the BFS website or obtained upon request from the respective institutions.

---

## Code Execution Instructions

1. **Clone the repository** to your local machine.
2. **Install the required R packages** by running the following in R:
    ```r
    install.packages(c("tidyverse", §readxl", "broom", "modelsummary", "dplyr", "ggplot", "ggtext", "ggfixest", "lmtest", "fixest", "gt", "gtsummary", "plm",  "ggplot2", "dplyr", "lubridate", "Synth", "knitr", "kableExtra", "webshot2", "ragg"))
    ```
3. **Run the R scripts** in the following order:
    - `table_generation.R`: For initial data preparation and table creation.
    - `parallel_trend_assumption_check.R`: To check the parallel trends assumption for the DiD analysis.
    - `DiD_and_SCM_analysis.R`: To perform the main analysis (DiD and SCM) and generate figures/tables.

---

## Computational Requirements

- **Software:** R (version 4.0.0 or higher)
- **Required R Packages:** `tidyverse`, `ggplot2`, `dplyr`, `lubridate`, `fixest`, etc.
- **Hardware Requirements:** No special hardware requirements. Any standard machine with R installed should be sufficient for running the analysis.
- **Expected Running Time:** Depending on your system, running all scripts may take a few minutes for data preparation and a longer time for DiD and SCM analyses.

---

## List of Tables and Figures

- **Tables**  
  - Table 1: Accepted Referenda on NC Enfranchisement in Swiss Cantons  
  - Table 2: Share of Foreign Population in Swiss Cantons with NC Voting Rights  
  - Table 3: DiD Estimates of the Effect on NC Enfranchisement on Foreigner Unemployment Rate  
  - Table 5: DiD Estimates Neuchâtel vs. Synthetic Neuchâtel (SCM)  

  - **Table A**  
    - Table A1: List of Referenda on NC Enfranchisement  
    - Table A2: Classification of Culturally Similar and Distant Countries  
    - Table A3: Cultural Distance across Cantons 2010 and 2023  
    - Table A4: Control Group Composition: Group I and II  
    - Table A5: Summary Statistics for the Pre-Treatment Period (1998-2001), by Group  

  - **Table B**  
    - Table B1: Regression Results for Parallel Trend Test Using Lead Variables  
    - Table B2: Placebo Test Results (1990-2001) - Group II and Group I  
    - Table B3: DiD Estimates – Collapsed Model with Pre- and Post-Treatment Averages  
    - Table B4: DiD Estimates – Robustness Check, Excluding 2009 Outlier  

  - **Table C**  
    - Table C1: Weights of Cantons in the SCM for Synthetic Neuchâtel  

- **Figures**  
  - Figure 1: Unemployment Rate: Neuchâtel vs. Sub-Control Group (Mean)  
  - Figure 2: Single Event-Study Lead/Lag Analysis: Neuchâtel vs Group II  
  - Figure 4: Unemployment Rate: Neuchâtel vs. Synthetic Neuchâtel  
  - Figure 5: Histogram of Post/Pre RMPSE Ratio of all Units  

  - **Figure B**  
    - Figure B1: Unemployment Rate Trends: Neuchâtel vs. Group II (Individual and Mean)  
    - Figure B2: Unemployment Rate Trends: Neuchâtel vs. Group I (Mean)  
    - Figure B3: Unemployment Rate Trends: Neuchâtel vs. Group I (Individual and Mean)  

  - **Figure C**  
    - Figure C1: Iterative Placebo Test for SCM: Unemployment Rate Trends  
    - Figure C2: Gap Plot: Actual vs. Synthetic Neuchâtel (Predictor: F. Unemployment)  
    - Figure C3: Gap Plot: Actual vs. Synthetic Neuchâtel (Predictor: Income)  

  
All figures and tables are saved in the `output/` directory after running the analysis scripts.

---

## Data Citations

- Staatssekretariat für Wirtschaft [SECO]. (2024). *Arbeitslose und Stellensuchende* [Dataset]. In arbeit.swiss. [https://www.amstat.ch/v2/amstat_de.html](https://www.amstat.ch/v2/amstat_de.html)
- Bundesamt für Statistik [BFS]. (2021). *Ständige und nichtständige Wohnbevölkerung nach Kanton, Anwesenheitsbewilligung, Staatsangehörigkeit, Geschlecht und Alter, 2022 - 2022 | Tabelle* [Dataset]. [https://www.bfs.admin.ch/asset/de/26605195](https://www.bfs.admin.ch/asset/de/26605195)
- Bundesamt für Statistik [BFS]. (2015). *Politik Statistik*. Politik-stat. [http://www.politik-stat.ch/nrw2015CH_de.html](http://www.politik-stat.ch/nrw2015CH_de.html)
- Eidgenössische Steuerverwaltung [ESTV]. (2019). *Statistiken zur direkten Bundessteuer* [Dataset]. In Steuerstatistiken. [https://www.estv.admin.ch/estv/de/home/die-estv/steuerstatistiken-estv/allgemeine-steuerstatistiken/direkte-bundessteuer/dbst-jp-kanton-ab-1983.html](https://www.estv.admin.ch/estv/de/home/die-estv/steuerstatistiken-estv/allgemeine-steuerstatistiken/direkte-bundessteuer/dbst-jp-kanton-ab-1983.html)
- Inglehart, R., & Baker, W. E. (2000). *Modernization, cultural change, and the persistence of traditional values*. American Sociological Review, 65(1), 19–51. [https://doi.org/10.1177/000312240006500103](https://doi.org/10.1177/000312240006500103)





- **Social Democratic Party (SP)**. (2024). *Voting Share Data*. Available at: [https://www.sp-ps.ch](https://www.sp-ps.ch).


