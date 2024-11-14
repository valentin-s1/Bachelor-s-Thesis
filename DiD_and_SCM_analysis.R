################################################################################
##########  NC Enfranchisement & Labour Market Integration #####################
################################################################################

##### File 2/3: Parallel_Trend_Assumption_Check
#### Author: Valentin Sascha Schnellmann
### Date: 14.11.2024

# Description: This R file conducts a DiD analysis and a SCM analysis to evaluate 
#              the impact of granting voting rights to migrants on the unemployment 
#              rate in Neuchâtel. The analysis explores both short-term and long-term 
#              effects using control groups of cantons without such voting rights. 
#              The DiD analysis includes TWFE to control for unobserved heterogeneity, 
#              and SCM aims to create a synthetic Neuchâtel to improve causal inference.
#
#### Data used:
# Unemployment data per canton (1993-2009) from the Federal Statistical Office
# (SECO, 2020); see link to data in README.md), control variables such as:
# - foreigner share per canton, annually (BFS, 2023),
# - SP vote share per canton, all 4 years (BFS, 2015),
# - cultural distance share, constant (BFS, 2024),
# - average income per capita per canton, annual (ESTV, 2019)
#
#
#### Steps:
# 1. Load relevant packages and data
# 2. Data cleaning and preparation 
# 3. Add control variables used
# 4. Summary Statistics
## Difference-in-Difference
# 5. TWFE Regression
# 6. Time Dynamic Treatment Effects
# 7. Robusstness Checks:
#     7.1 Collapsed Model
#     7.2 Excluding Year 09
# 8. Additional Summary Statistics
## Synthetic Control Method
# 9. SCM Preparation
# 10. Predictor Identification 
# 11. Creation of actual Synthetic Neuchatel
# 12. Gap Plot creation
# 13. Parallel Trend Assumption Check
# 14. DiD Regression using Synth. Neuchatel
# 15. Robustness Checks: 
#      15.1 Placebo Test
#      15.2 Iterative Re-estimation 
#
### Tables of Content: Appendix B
# Table A5: Summary Statistics for the Pre-Treatment Period (1998-2001), by Group
# Table 3: DiD Estimates of the Effect on NC Enfranchisement on Foreigner Unemployment Rate
# Figure 2: Single Event-Study Lead/Lag Analysis: Neuchâtel vs Group II
# Table B3: DiD Estimates – Collapsed Model with Pre- and Post-Treatment Averages
# Table B4: DiD Estimates – Robustness Check, Excluding 2009 Outlier
# Figure C1: Iterative Placebo Test for SCM: Unemployment Rate Trends
# Figure C2: Gap Plot: Actual vs. Synthetic Neuchâtel (Predictor: F. Unemployment)
# Figure C3: Gap Plot: Actual vs. Synthetic Neuchâtel (Predictor: Income)
# Figure 4: Unemployment Rate: Neuchâtel vs. Synthetic Neuchâtel
# Table 5: DiD Estimates Neuchâtel vs. Synthetic Neuchâtel (SCM)
# Figure 5: Histogram of Post/Pre RMPSE Ratio of all Units



################################ START #########################################

#### First Step: load relevant packages and data ####
# load packages
library(tidyverse)
library(readxl)
library(broom)
library(modelsummary)
library(dplyr)
library(ggplot2)
library(lmtest)
library(fixest)
library(gt)
library(plm)
library(ggfixest)
library(Synth)

# load independent variable
data_unemployment_raw <- read_excel("Data1/Clean_Arbeitslosenquote.xlsx") %>%
  .[-1, ] %>%
  .[, -2] %>% 
  { setNames(., trimws(names(.))) }


# load control variables
data_culturald_raw <- read_excel("Data1/Clean_Cultural_Distance_Share_2010.xlsx")
data_fshare_raw <- read_excel("Data1/Clean_Foreigner_Share.xlsx")
data_SPshare_raw <- read_excel("Data1/Clean_SP_Share.xlsx")



#### Step 2: Data cleaning and preparation ####
# clean independent variable
data_long_unemployment_clean <- data_unemployment_raw %>%
  pivot_longer(cols = -Kanton, names_to = "Monat_Jahr", values_to = "Arbeitslosenquote") %>%
  separate(Monat_Jahr, into = c("Monat", "Jahr"), sep = " ") %>%
  mutate(Datum = as.Date(paste(Jahr, Monat, "01", sep = "-"), "%Y-%B-%d")) %>%
  filter(!is.na(Kanton)) %>%
  mutate(StateID = as.numeric(as.factor(Kanton))) %>%
  mutate(treatment = ifelse(Kanton == "Neuenburg", 1, 0)) %>%
  mutate(
    time_numeric = as.numeric(format(as.Date(Datum), "%Y")) + 
      (as.numeric(format(as.Date(Datum), "%m")) - 1) / 12) %>%
  mutate(post = ifelse(Datum >= as.Date("2002-01-01"), 1, 0)) %>%
  mutate(Arbeitslosenquote = as.numeric(Arbeitslosenquote) / 100) 


# Clean foreigner share variable
data_long_fshare_clean <- data_fshare_raw %>%
  mutate(across(`1990`:`2010`, as.numeric))  %>%
  pivot_longer(
    cols = `1990`:`2010`,
    names_to = "Jahr",
    values_to = "Ausländeranteil")  %>%
  mutate(
    Jahr = as.numeric(Jahr),  
    Ausländeranteil = round(Ausländeranteil, 6)) %>%
  filter(Jahr >= 1993 & Jahr <= 2009) %>%
  rowwise() %>%
  do(data.frame(Kanton = .$Kanton,
                Datum = seq(as.Date(paste0(.$Jahr, "-01-01")), as.Date(paste0(.$Jahr, "-12-01")), 
                            by = "month"),
                Ausländeranteil = .$Ausländeranteil)) %>%
  ungroup() %>%
  filter(complete.cases(.))



# Clean cultural distance share
data_long_culturald_clean <- data_culturald_raw %>%
  rowwise() %>%
  do(data.frame(Kanton = .$Kanton,
                Datum = seq(as.Date("1993-01-01"), as.Date("2009-12-01"), by = "month"),
                cultural_distance = round(.$`Cultural Distance`, 3))) %>%
  ungroup()

# Clean SP Share
data_long_SPshare_clean <- data_SPshare_raw  %>%
  select(-c(`2011`, `2015`, `2019`, `2023`)) %>%
  mutate(across(c(`1991`, `1995`, `1999`, `2003`, `2007`), as.numeric))  %>%
  pivot_longer(
    cols = c(`1991`, `1995`, `1999`, `2003`, `2007`),
    names_to = "Jahr",
    values_to = "sp_share") %>%
  mutate(
    Jahr = as.numeric(Jahr),
    sp_share = round(sp_share, 3)) %>% 
  mutate(
    start_date = case_when(
      Jahr == 1991 ~ as.Date("1991-10-01"),
      Jahr == 1995 ~ as.Date("1995-10-01"),
      Jahr == 1999 ~ as.Date("1999-10-01"),
      Jahr == 2003 ~ as.Date("2003-10-01"),
      Jahr == 2007 ~ as.Date("2007-10-01")),
    end_date = case_when(
      Jahr == 1991 ~ as.Date("1995-09-30"),
      Jahr == 1995 ~ as.Date("1999-09-30"),
      Jahr == 1999 ~ as.Date("2003-09-30"),
      Jahr == 2003 ~ as.Date("2007-09-30"),
      Jahr == 2007 ~ as.Date("2010-12-31"))) %>%
  rowwise() %>%
  do(data.frame(Kanton = .$Kanton,
                Datum = seq(.$start_date, .$end_date, by = "month"),
                sp_share = .$sp_share)) %>%
  ungroup()  %>%
  filter(Datum >= as.Date("1993-01-01") & Datum <= as.Date("2009-12-01"))



#### Step 3: Add control varaibles used ####
## merge data to create new dataset
data_long_did_full <- data_long_unemployment_clean %>%
  left_join(data_long_culturald_clean, by = c("Kanton", "Datum")) %>%
  left_join(data_long_fshare_clean, by = c("Kanton", "Datum")) %>%
  left_join(data_long_SPshare_clean, by = c("Kanton", "Datum"))

## add variabel Economic Regions to dataset
data_long_did_full_Regions <- data_long_did_full %>%
  mutate(
    Wirtschaftsregion = case_when(
      Kanton %in% c("Waadt", "Wallis", "Genf") ~ 1,
      Kanton %in% c("Bern", "Freiburg", "Solothurn", "Neuenburg", "Jura") ~ 2,
      Kanton %in% c("Basel-Stadt", "Basel-Landschaft", "Aargau") ~ 3,      
      Kanton == "Zürich" ~ 4, 
      Kanton %in% c("Glarus", "Schaffhausen", "Appenzell Ausserrhoden", 
                    "Appenzell Innerrhoden", "St. Gallen", "Graubünden", "Thurgau") ~ 5,
      Kanton %in% c("Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden", "Zug") ~ 6, 
      Kanton == "Tessin" ~ 7)
  )

## create data sets for following analysis
# data for control group (whole Switzerland excl. cantons with NCV rights)
data_did_twfe_control <- data_long_did_full_Regions  %>%
  filter(!Kanton %in% c("Jura", "Appenzell Ausserrhoden", "Waadt", "Graubünden",
                        "Freiburg","Basel-Stadt", "Genf")) %>%
  filter(Jahr >= 1998)

# data for sub control group (parallel trend assumption fulfilled)
data_did_twfe_subcontrol <- data_long_did_full_Regions %>%
  filter(Kanton %in% c("Neuenburg", "Aargau", "Bern", 
                       "Luzern", "Schaffhausen", "Solothurn", 
                       "St. Gallen", "Thurgau", 
                       "Schwyz", "Zug")) %>%
  filter(Jahr >= 1998)



#### Step 4: Summary Statistics #####
## Summary Statistics                             ################### [Table A5]
# Function to create a structured summary table
create_compact_summary_table <- function(data_neuchatel, data_control, data_subcontrol) {
  # Helper function to generate summary statistics
  get_summary <- function(data, group_name) {
    data %>%
      summarise(
        Unemployment_Mean = mean(Arbeitslosenquote, na.rm = TRUE),
        Unemployment_SD = sd(Arbeitslosenquote, na.rm = TRUE),
        Unemployment_Min = min(Arbeitslosenquote, na.rm = TRUE),
        Unemployment_Max = max(Arbeitslosenquote, na.rm = TRUE),
        Unemployment_Median = median(Arbeitslosenquote, na.rm = TRUE),
        Unemployment_n = sum(!is.na(Arbeitslosenquote)),
        
        Foreigner_Mean = mean(Ausländeranteil, na.rm = TRUE),
        Foreigner_SD = sd(Ausländeranteil, na.rm = TRUE),
        Foreigner_Min = min(Ausländeranteil, na.rm = TRUE),
        Foreigner_Max = max(Ausländeranteil, na.rm = TRUE),
        Foreigner_Median = median(Ausländeranteil, na.rm = TRUE),
        Foreigner_n = sum(!is.na(Ausländeranteil)),
        
        Cultural_Mean = mean(cultural_distance, na.rm = TRUE),
        Cultural_SD = sd(cultural_distance, na.rm = TRUE),
        Cultural_Min = min(cultural_distance, na.rm = TRUE),
        Cultural_Max = max(cultural_distance, na.rm = TRUE),
        Cultural_Median = median(cultural_distance, na.rm = TRUE),
        Cultural_n = sum(!is.na(cultural_distance)),
        
        SP_Mean = mean(sp_share, na.rm = TRUE),
        SP_SD = sd(sp_share, na.rm = TRUE),
        SP_Min = min(sp_share, na.rm = TRUE),
        SP_Max = max(sp_share, na.rm = TRUE),
        SP_Median = median(sp_share, na.rm = TRUE),
        SP_n = sum(!is.na(sp_share))
      ) %>%
      pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "_") %>%
      mutate(Group = group_name) %>%
      # Rename Variables
      mutate(Variable = case_when(
        Variable == "Unemployment" ~ "Unemployment Rate",
        Variable == "Foreigner" ~ "Foreigner Share",
        Variable == "Cultural" ~ "Cultural Distant Share",
        Variable == "SP" ~ "SP Share",
        TRUE ~ Variable
      ))
  }
  
  # Generate summaries for each group
  summary_neuchatel <- get_summary(data_neuchatel, "Neuchâtel")
  summary_control <- get_summary(data_control, "Control Group (Group I)")
  summary_subcontrol <- get_summary(data_subcontrol, "Sub-Control Group (Group II)")
  
  combined_summary <- bind_rows(summary_neuchatel, summary_control, summary_subcontrol) %>%
    select(Group, Variable, Mean, SD, Min, Max, Median, n) %>%
    mutate(Group = factor(Group, levels = c("Neuchâtel", 
                                            "Control Group (Group I)", 
                                            "Sub-Control Group (Group II)"))) %>%
    arrange(Group, Variable)%>%
    # Blank out repeated group names
    mutate(Group = ifelse(duplicated(Group), "", as.character(Group)))
  
  # Create gt table
  gt(combined_summary) %>%
    tab_header(
      title = "Table A4. Summary Statistics") %>%
    opt_align_table_header(align = "left") %>%
    cols_label(
      Group = "Group",
      Variable = "Variable",
      Mean = "Mean",
      SD = "SD",
      Min = "Min",
      Max = "Max",
      Median = "Median",
      n = "n"
    ) %>%
    fmt_number(
      columns = c(Mean, SD, Min, Max, Median),
      decimals = 3
    ) %>%
    tab_spanner(
      label = "Statistics",
      columns = c(Mean, SD, Min, Max, Median, n)
    ) %>%
    cols_align(
      align = "left",
      columns = vars(Group)
    )
}

# Apply the function
data_neuchatel <- data_did_twfe_control %>% filter(Kanton == "Neuenburg")
create_compact_summary_table(data_neuchatel, data_did_twfe_control, data_did_twfe_subcontrol)


############## Difference-in-Difference Analysis ###############################

#### Step 5: TWFE Regressions ####
## TWFE for Group II (Sub-Control Group)
model1.1 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment)| 
                            time_numeric, 
                          data = data_did_twfe_subcontrol, 
                          cluster = c("Kanton"))

model1.2 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) 
                          + Ausländeranteil + cultural_distance + sp_share| 
                            time_numeric, 
                          data = data_did_twfe_subcontrol, 
                          cluster = c("Kanton"))

model1.3 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) 
                          + Ausländeranteil + cultural_distance + sp_share| 
                            time_numeric + Wirtschaftsregion, 
                          data = data_did_twfe_subcontrol, 
                          cluster = c("Kanton"))


## TWFE for Group I (Control-Group)
model2.1 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment)| 
                            time_numeric, 
                          data = data_did_twfe_control, 
                          cluster = c("Kanton"))

model2.2 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) 
                          + Ausländeranteil + cultural_distance + sp_share| 
                            time_numeric, 
                          data = data_did_twfe_control, 
                          cluster = c("Kanton"))

model2.3 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) 
                          + Ausländeranteil + cultural_distance + sp_share| 
                            time_numeric + Wirtschaftsregion, 
                          data = data_did_twfe_control, 
                          cluster = c("Kanton"))

## Combine Regressions in output Table                ############### [Table 3]
etable(model1.1, model1.2, model1.3, model2.1, model2.2, model2.3)



#### Step 6: Time Dynamic Treatment Effects ####
## create dataset for subcontrol             ####################### [Figure 2]
# annual means, treatment variable and time to treatment
data_event_study_annual <- data_did_twfe_subcontrol %>%
  mutate(Jahr = as.numeric(Jahr)) %>%
  group_by(Kanton, Jahr) %>%  
  summarise(
    Arbeitslosenquote = mean(Arbeitslosenquote, na.rm = TRUE),
    Ausländeranteil = mean(Ausländeranteil, na.rm = TRUE),
    cultural_distance = mean(cultural_distance, na.rm = TRUE),
    sp_share = mean(sp_share, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  mutate(
    time_to_treatment = Jahr - 2002,
    # Definiere die `treat` Variable: 1 für Neuenburg, 0 für alle anderen Kantone
    treat = if_else(Kanton == "Neuenburg", 1, 0)
  )

# Single Unit Event Study using fixest and period -1 as reference
event_study_model <- fixest::feols(
  Arbeitslosenquote ~ i(time_to_treatment, treat, ref = c(-1)) 
  + Ausländeranteil | Jahr,
  cluster = ~Kanton,
  data = data_event_study_annual
)

# use ggfixest to plot results
figure2 <- ggiplot(
  event_study_model,
  main = "Single Event-Study Lead/Lag Analysis",
  xlab = "Years around the Introduction of Voting Rights (2002)",
  ylab = "Estimate and 95% Confidence Intervals",
  geom_style = "pointrange") +
  ggtitle("Figure 2. Single Event-Study Lead/Lag Analysis: Neuchâtel vs Group II") +
  theme(
    plot.title = element_text(hjust = 0),  
    plot.caption = element_text(hjust = 0)) +
  labs(caption = "Note: Estimates are based on annual means of monthly data, 
       averaged for each year.\n - Using yearly fixed effects, 
       share of foreigner as control, and clustered SE at canton level.") 

# save figure
ggsave(
  "output/figure2_image.png", 
  plot = figure2, 
  width = 10, 
  height = 5, 
  bg = "white")




#### Step 7: Robustness Cheks ####
### 7.1 Collapsed Model ###
## colapsed model for Group II (Sub-Control)
# colapse data for Group II
data_collapsed_subcontrol <- data_did_twfe_subcontrol %>%
  mutate(treatment = ifelse(Kanton == "Neuenburg", 1, 0)) %>%
  mutate(post = ifelse(Jahr >= 2002, 1, 0)) %>%
  group_by(Kanton, post) %>%
  summarise(
    unemployment_rate = mean(Arbeitslosenquote, na.rm = TRUE),
    share_foreigners = mean(Ausländeranteil, na.rm = TRUE),
    cultural_distance = mean(cultural_distance, na.rm = TRUE),
    sp_share = mean(sp_share, na.rm = TRUE),
    treatment = first(treatment),
    StateID = first(StateID)) %>%
  ungroup()

# Regressions
model_collapsed1.1 <- fixest::feols(unemployment_rate ~ i(post, treatment), 
                                    data = data_collapsed_subcontrol, 
                                    cluster = c("Kanton"))

model_collapsed1.2 <- fixest::feols(unemployment_rate ~ i(post, treatment) 
                                    + share_foreigners + cultural_distance 
                                    + sp_share, 
                                    data = data_collapsed_subcontrol, 
                                    cluster = c("Kanton"))


## colapsed model for Group I (Control Group)
# Colapse data for Group I
data_collapsed_control <- data_did_twfe_control %>%
  mutate(treatment = ifelse(Kanton == "Neuenburg", 1, 0)) %>%
  mutate(post = ifelse(Jahr >= 2002, 1, 0)) %>%
  group_by(Kanton, post) %>%
  summarise(
    unemployment_rate = mean(Arbeitslosenquote, na.rm = TRUE),
    share_foreigners = mean(Ausländeranteil, na.rm = TRUE),
    cultural_distance = mean(cultural_distance, na.rm = TRUE),
    sp_share = mean(sp_share, na.rm = TRUE),
    treatment = first(treatment),
    StateID = first(StateID)) %>%
  ungroup()

# Regressions
model_collapsed2.1 <- fixest::feols(unemployment_rate ~ i(post, treatment), 
                                    data = data_collapsed_control, 
                                    cluster = c("Kanton"))

model_collapsed2.2 <- feols(unemployment_rate ~ i(post, treatment) 
                            + share_foreigners + cultural_distance 
                            + sp_share, 
                            data = data_collapsed_control, 
                            cluster = c("Kanton"))


# Combine Regressions for Output                     ################ [Table B3]
etable(model_collapsed1.1, model_collapsed1.2, model_collapsed2.1, model_collapsed2.2)


### 7.1 Excluding Year 09 ###
### create reduced dataset excluding year 2009
# for Group II (Sub-Control Group)
data_did_twfe_subcontrol_reduced <- data_did_twfe_subcontrol %>%
  filter(Jahr < 2009)

# for Group I (Control Group)
data_did_twfe_control_reduced <- data_did_twfe_control %>%
  filter(Jahr < 2009)

## TWFE for Group II
model_reduced1.1 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment)| 
                                    time_numeric, 
                                  data = data_did_twfe_subcontrol_reduced, 
                                  cluster = c("Kanton"))

model_reduced1.2 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) 
                                  + Ausländeranteil + cultural_distance + sp_share| 
                                    time_numeric, 
                                  data = data_did_twfe_subcontrol_reduced, 
                                  cluster = c("Kanton"))

model_reduced1.3 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) 
                                  + Ausländeranteil + cultural_distance + sp_share| 
                                    time_numeric + Wirtschaftsregion, 
                                  data = data_did_twfe_subcontrol_reduced, 
                                  cluster = c("Kanton"))


## TWFE for Group I
model_reduced2.1 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment)| 
                                    time_numeric, 
                                  data = data_did_twfe_control_reduced, 
                                  cluster = c("Kanton"))

model_reduced2.2 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) 
                                  + Ausländeranteil + cultural_distance + sp_share| 
                                    time_numeric, 
                                  data = data_did_twfe_control_reduced, 
                                  cluster = c("Kanton"))

model_reduced2.3 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) 
                                  + Ausländeranteil + cultural_distance + sp_share| 
                                    time_numeric + Wirtschaftsregion, 
                                  data = data_did_twfe_control_reduced, 
                                  cluster = c("Kanton"))

# Combine Regressions for Output Table              ################ [Table B4]
etable(model_reduced1.1, model_reduced1.2, model_reduced1.3, 
       model_reduced2.1, model_reduced2.2, model_reduced2.3)



#### Step 8: Additional Summary Statistics ####
## only Pre-Treatment Period using Years 1998-2001
# Function to create a structured summary table
create_compact_summary_table <- function(data_neuchatel, data_control, data_subcontrol) {
  # Helper function to generate summary statistics
  get_summary <- function(data, group_name) {
    data %>%
      summarise(
        Unemployment_Mean = mean(Arbeitslosenquote, na.rm = TRUE),
        Unemployment_SD = sd(Arbeitslosenquote, na.rm = TRUE),
        Unemployment_Min = min(Arbeitslosenquote, na.rm = TRUE),
        Unemployment_Max = max(Arbeitslosenquote, na.rm = TRUE),
        Unemployment_Median = median(Arbeitslosenquote, na.rm = TRUE),
        Unemployment_n = sum(!is.na(Arbeitslosenquote)),
        
        Foreigner_Mean = mean(Ausländeranteil, na.rm = TRUE),
        Foreigner_SD = sd(Ausländeranteil, na.rm = TRUE),
        Foreigner_Min = min(Ausländeranteil, na.rm = TRUE),
        Foreigner_Max = max(Ausländeranteil, na.rm = TRUE),
        Foreigner_Median = median(Ausländeranteil, na.rm = TRUE),
        Foreigner_n = sum(!is.na(Ausländeranteil)),
        
        Cultural_Mean = mean(cultural_distance, na.rm = TRUE),
        Cultural_SD = sd(cultural_distance, na.rm = TRUE),
        Cultural_Min = min(cultural_distance, na.rm = TRUE),
        Cultural_Max = max(cultural_distance, na.rm = TRUE),
        Cultural_Median = median(cultural_distance, na.rm = TRUE),
        Cultural_n = sum(!is.na(cultural_distance)),
        
        SP_Mean = mean(sp_share, na.rm = TRUE),
        SP_SD = sd(sp_share, na.rm = TRUE),
        SP_Min = min(sp_share, na.rm = TRUE),
        SP_Max = max(sp_share, na.rm = TRUE),
        SP_Median = median(sp_share, na.rm = TRUE),
        SP_n = sum(!is.na(sp_share))
      ) %>%
      pivot_longer(everything(), names_to = c("Variable", ".value"), names_sep = "_") %>%
      mutate(Group = group_name) %>%
      # Rename Variables
      mutate(Variable = case_when(
        Variable == "Unemployment" ~ "Unemployment Rate",
        Variable == "Foreigner" ~ "Foreigner Share",
        Variable == "Cultural" ~ "Cultural Distant Share",
        Variable == "SP" ~ "SP Share",
        TRUE ~ Variable
      ))
  }
  
  # Generate summaries for each group
  summary_neuchatel <- get_summary(data_neuchatel, "Neuchâtel")
  summary_control <- get_summary(data_control, "Control Group (Group I)")
  summary_subcontrol <- get_summary(data_subcontrol, "Sub-Control Group (Group II)")
  
  combined_summary <- bind_rows(summary_neuchatel, summary_control, summary_subcontrol) %>%
    select(Group, Variable, Mean, SD, Min, Max, Median, n) %>%
    mutate(Group = factor(Group, levels = c("Neuchâtel", 
                                            "Control Group (Group I)", 
                                            "Sub-Control Group (Group II)"))) %>%
    arrange(Group, Variable)%>%
    # Blank out repeated group names
    mutate(Group = ifelse(duplicated(Group), "", as.character(Group)))
  
  # Create gt table
  gt(combined_summary) %>%
    tab_header(
      title = "Table A4. Summary Statistics") %>%
    opt_align_table_header(align = "left") %>%
    cols_label(
      Group = "Group",
      Variable = "Variable",
      Mean = "Mean",
      SD = "SD",
      Min = "Min",
      Max = "Max",
      Median = "Median",
      n = "n"
    ) %>%
    fmt_number(
      columns = c(Mean, SD, Min, Max, Median),
      decimals = 3
    ) %>%
    tab_spanner(
      label = "Statistics",
      columns = c(Mean, SD, Min, Max, Median, n)
    ) %>%
    cols_align(
      align = "left",
      columns = vars(Group)
    )
}

# Filter for years lower or equal 2001
data_did_twfe_subcontrol_pre <- subset(data_did_twfe_subcontrol, Jahr <= 2001)
data_did_twfe_control_pre <- subset(data_did_twfe_control, Jahr <= 2001)

# Apply the function
data_neuchatel <- data_did_twfe_control_pre %>% filter(Kanton == "Neuenburg")
create_compact_summary_table(data_neuchatel, data_did_twfe_control_pre, data_did_twfe_subcontrol_pre)




################ Synthetic Control Method Analysis #############################

#### Step 9: SCM Preparation ####
## load data on yearly income per capita / canton
income_data <- read_excel("Data1/Clean_Income_per_Capita_1993-2010.xlsx")
income_data_long <- income_data %>%
  mutate(across(starts_with("19"), as.numeric)) %>%
  mutate(across(starts_with("20"), as.numeric)) %>%
  pivot_longer(cols = c(starts_with("19"), starts_with("20")), 
               names_to = "Jahr", 
               values_to = "income")

## merge data
data_scm_control <- data_long_did_full_Regions %>%
  left_join(income_data_long, by = c("Kanton", "Jahr")) %>%
  filter(!Kanton %in% c("Jura", "Appenzell Ausserrhoden", "Waadt", "Graubünden",
                        "Freiburg","Basel-Stadt", "Genf")) %>%
  mutate(StateID = as.numeric(StateID))


# Define variables as numeric and create nummeric values for time
data_scm_control <- data_scm_control %>%
  mutate(
    StateID = as.numeric(StateID),
    time_numeric = as.numeric(time_numeric),
    Arbeitslosenquote = as.numeric(Arbeitslosenquote),
    income = as.numeric(income),
    Ausländeranteil = as.numeric(Ausländeranteil)
  ) %>%
  group_by(Kanton) %>%
  mutate(Year2 = as.numeric(row_number())) %>%  
  ungroup()

# create df, for Synth
data_scm_control_df <- as.data.frame(data_scm_control)


#### Step 10: Predictor Identification ####
### Check for Specification for optimal Predictors by comparing MSPE
# 1. Income and Share of Foreigners
dataprep.out1 <- dataprep(data_scm_control_df,
                          predictors             = c("Ausländeranteil", "income"),
                          dependent              = "Arbeitslosenquote", 
                          unit.variable          = "StateID",           
                          time.variable          = "Year2",   
                          unit.names.variable    = "Kanton",  
                          treatment.identifier   = 13,  
                          controls.identifier    = setdiff(unique(data_scm_control_df$StateID), 13), 
                          time.predictors.prior  = 1:108,       
                          time.optimize.ssr      = 1:108,   
                          time.plot              = 1:204      
)

# weigth table
synth.out1 <- synth(dataprep.out1)

synth.tables1 <- synth.tab(
  dataprep.res = dataprep.out1,
  synth.res    = synth.out1)

print(synth.tables1)



## synth plot
path.plot(synth.res    = synth.out1,
          dataprep.res = dataprep.out1,
          Ylab         = c("Arbeitslosenquote"),
          Xlab         = c("Year2"),
          Legend       = c("State A","Synthetic State A"),
          Legend.position = c("topleft")
)
abline(v   = 109,
       lty = 2)



# 2. Foreigner Unemployment Rate
dataprep.out2 <- dataprep(data_scm_control_df,
                          predictors             = c("Arbeitslosenquote"), 
                          dependent              = "Arbeitslosenquote", 
                          unit.variable          = "StateID",   
                          time.variable          = "Year2", 
                          unit.names.variable    = "Kanton",
                          treatment.identifier   = 13,  
                          controls.identifier    = setdiff(unique(data_scm_control_df$StateID), 13), 
                          time.predictors.prior  = 1:108,                       
                          time.optimize.ssr      = 1:108,               
                          time.plot              = 1:204        
)

# weigth table
synth.out2 <- synth(dataprep.out2)

synth.tables2 <- synth.tab(
  dataprep.res = dataprep.out2,
  synth.res    = synth.out2)

print(synth.tables2)



## synth plot
path.plot(synth.res    = synth.out2,
          dataprep.res = dataprep.out2,
          Ylab         = c("Arbeitslosenquote"),
          Xlab         = c("Year2"),
          Legend       = c("State A","Synthetic State A"),
          Legend.position = c("topleft")
)
abline(v   = 109,
       lty = 2)



# 3. Income (average Income per capita / canton)
dataprep.out3 <- dataprep(data_scm_control_df,
                          predictors             = c("income"),  
                          dependent              = "Arbeitslosenquote",           
                          unit.variable          = "StateID",                     
                          time.variable          = "Year2",                       
                          unit.names.variable    = "Kanton",                    
                          treatment.identifier   = 13,                              
                          controls.identifier    = setdiff(unique(data_scm_control_df$StateID), 13), 
                          time.predictors.prior  = 1:108,                    
                          time.optimize.ssr      = 1:108,          
                          time.plot              = 1:204       
)

# weigth table
synth.out3 <- synth(dataprep.out3)

synth.tables3 <- synth.tab(
  dataprep.res = dataprep.out3,
  synth.res    = synth.out3)

print(synth.tables3)



## synth plot
path.plot(synth.res    = synth.out3,
          dataprep.res = dataprep.out3,
          Ylab         = c("Arbeitslosenquote"),
          Xlab         = c("Year2"),
          Legend       = c("State A","Synthetic State A"),
          Legend.position = c("topleft")
)
abline(v   = 109,
       lty = 2)


## -> results suggest only using Foreigner Unemployment Rate as Predictor!






#### Step 11: Creation of actual Synthetic Neuchatel ####
# According to results above, only use Arbeitslosenquote
dataprep.out <- dataprep(data_scm_control_df,
                         predictors             = c("Arbeitslosenquote"),  
                         dependent              = "Arbeitslosenquote",         
                         unit.variable          = "StateID",        
                         time.variable          = "Year2",      
                         unit.names.variable    = "Kanton",       
                         treatment.identifier   = 13,             
                         controls.identifier    = setdiff(unique(data_scm_control_df$StateID), 13),
                         time.predictors.prior  = 1:108,              
                         time.optimize.ssr      = 1:108,    
                         time.plot              = 1:204  
)

# weigth table
synth.out <- synth(dataprep.out)

synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res    = synth.out)

print(synth.tables)

## create new Dataset
# Extract u for synthetic Neuenburg
synth_neuenburg <- as.numeric(dataprep.out$Y0plot %*% synth.out$solution.w)

# Create new dataframe for Synthetic Neuenburg
synthetic_neuenburg_df <- data_scm_control_df %>%
  filter(StateID == 13) %>%                # Filter to get the structure of Neuenburg
  mutate(StateID = 27,                     # Assign a new StateID for synthetic Neuenburg
         Kanton = "Synthetic Neuenburg",   # Name it "Synthetic Neuenburg"
         Arbeitslosenquote = synth_neuenburg)  

# Combine datasets 
data_scm_all <- bind_rows(data_scm_control_df, synthetic_neuenburg_df)





#### Step 12: Gap Plot Creation ####
### GAP for Predictor: Foreigner Unemployment Rate
# Calculate the gap between actual and synthetic Neuenburg
gap_data <- data_scm_control_df %>%
  filter(StateID == 13) %>%  # Filter actual Neuenburg data
  mutate(Gap = Arbeitslosenquote - synth_neuenburg)
gap_data$Datum <- as.Date(gap_data$Datum)

# Create a data frame for plotting
gap_plot_data <- data.frame(
  Year = gap_data$Datum,
  Gap = gap_data$Gap
)

# create GAP plot for Predictor: F. Unemployment  ########### [Figure C2]
figureC2 <- ggplot() +
  # Plot the gap line using the provided data
  geom_line(data = gap_plot_data, aes(x = Year, y = Gap), size = 0.6, color = "black") +
  # Add a vertical dotted line at the policy implementation year (2002)
  geom_vline(xintercept = as.Date("2002-01-01"), linetype = "dotted", color = "black", size = 0.6) +
  # Add a horizontal dashed line at y = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  # Customize x-axis labels for date format
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.05, 0.05, by = 0.01)) +  
  # Labels and title
  labs(
    title = "Gap Plot: Actual vs. Synthetic Neuenburg (Predictor: F. Unemployment)",
    x = "Year",
    y = "Gap (Actual - Synthetic)"
  ) +
  # Apply formatting
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "none",  # No legend needed since it's a single line
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14)
  )


### GAP for Predictor: Income
# Calculate the gap between actual and synthetic Neuenburg in this case
# Calculate the gap between actual and synthetic Neuchâtel
gap_data1 <- data_scm_control_df %>%
  filter(StateID == 13) %>%  # Filter for actual Neuchâtel data
  mutate(Gap = Arbeitslosenquote - as.numeric(dataprep.out3$Y0plot %*% synth.out3$solution.w))
gap_data1$Datum <- as.Date(gap_data1$Datum)

# Create a data frame for plotting
gap_plot_data1 <- data.frame(
  Year = gap_data1$Datum,
  Gap = gap_data1$Gap
)

# # create GAP plot for Predictor: Income     ################ [Figure C3]
figureC3 <- ggplot() +
  geom_line(data = gap_plot_data1, aes(x = Year, y = Gap), size = 0.6, color = "black") +
  geom_vline(xintercept = as.Date("2002-01-01"), linetype = "dotted", color = "black", size = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.6) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(-0.05, 0.05, by = 0.01)) +  
  labs(
    title = "Gap Plot: Actual vs. Synthetic Neuenburg (Predictors: Income)",
    x = "Year",
    y = "Gap (Actual - Synthetic)"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "none",  
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size = 0.3),
    axis.ticks.length = unit(0.3, "cm"),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    plot.title = element_text(hjust = 0.5, size = 14)
  )


## save plots in output/
ggsave("output/figureC2_image.png", plot = figureC2, width = 10, height = 5, bg = "white")
ggsave("output/figureC3_image.png", plot = figureC3, width = 10, height = 5, bg = "white")





#### Step 13: Parallel Trend Assumption Check ####
## filter dataset for only Neuchatel and Synthetic
plot_data_scm <- data_scm_all %>%
  mutate(Arbeitslosenquote = Arbeitslosenquote * 100) %>%
  filter(Kanton %in% c("Neuenburg", "Synthetic Neuenburg"))

## Create plot for Neuchatel and Synthetic Neuchatel    ############ [Figure 4]
figure4 <- ggplot() +
  geom_line(data = plot_data_scm %>% filter(Kanton == "Synthetic Neuenburg"), 
            aes(x = Datum, y = Arbeitslosenquote, linetype = "Synthetic Neuchâtel"), size = 0.6, color = "black") +
  geom_vline(xintercept = as.Date("2002-01-01"), linetype = "dotted", color = "black", size = 0.6) +
  geom_line(data = plot_data_scm %>% filter(Kanton == "Neuenburg"), 
            aes(x = Datum, y = Arbeitslosenquote, linetype = "Neuchâtel"), 
            size = 0.6, color = "black") +
  scale_linetype_manual(values = c("Neuchâtel" = "solid", "Synthetic Neuchâtel" = "dashed")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) +  
  labs(title = "Unemployment Rate: Neuchâtel vs. Synthetic Neuchâtel",
       x = "Year",
       y = "Unemployment Rate (%)") +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0, 1),
    legend.justification = c("left", "top"), 
    legend.background = element_rect(color = "black", fill = "white", size = 0.2), 
    legend.margin = margin(10, 30, 10, 30),
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size = 0.3), 
    axis.ticks.length = unit(0.3, "cm") 
  )

## save plots in output/
ggsave("output/figure4_image.png", plot = figure4, width = 10, height = 5, bg = "white")




#### Step 14: DID Regression using Synth. Neuchatel ####
# correct treatment and post variable
data_scm_all_twfe <- data_scm_all %>%
  mutate(
    treatment = ifelse(Kanton == "Neuenburg", 1, 0),
    post = ifelse(Jahr >= 2002, 1, 0))

# regressions with time variation
model_scm_total <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) | 
                                   time_numeric,
                                 data = data_scm_all_twfe %>% 
                                   filter(
                                     Kanton %in% c("Neuenburg", "Synthetic Neuenburg")),
                                 vcov = "HC1")

model_scm_1998 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) | 
                                  time_numeric,
                                data = data_scm_all_twfe %>%
                                  filter(
                                    Kanton %in% c("Neuenburg", "Synthetic Neuenburg"),
                                    Jahr >= 1998),
                                vcov = "HC1")

model_scm_2005 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) | time_numeric,
                                data = data_scm_all_twfe %>%
                                  filter(
                                    Kanton %in% c("Neuenburg", "Synthetic Neuenburg"),
                                    Jahr >= 1998, Jahr <= 2005),
                                vcov = "HC1")

model_scm_2005_2007 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) | time_numeric,
                                     data = data_scm_all_twfe %>%
                                       filter(Kanton %in% c("Neuenburg", "Synthetic Neuenburg"),
                                              Jahr >= 1998, Jahr < 2002 | Jahr > 2005, Jahr <= 2007),
                                     vcov = "HC1")

model_scm_2007_2009 <- fixest::feols(Arbeitslosenquote ~ i(post, treatment) | 
                                       time_numeric,
                                     data = data_scm_all_twfe %>%
                                       filter(Kanton %in% c("Neuenburg", "Synthetic Neuenburg"),
                                              Jahr >= 1998, Jahr < 2002 | Jahr > 2007, Jahr <= 2009),
                                     vcov = "HC1")

# Combine Regression in Table                         ################## [Table 5]
etable(model_scm_total, model_scm_1998, model_scm_2005, model_scm_2005_2007, model_scm_2007_2009)






#### Step 15: Robustness Checks: 
### 15.1 Placbo Test ###
### Neuenburg
## again calculate Synthetic Neuchatel
dataprep.out_NE <- dataprep(data_scm_control_df,
                            predictors             = c("Arbeitslosenquote"),
                            dependent              = "Arbeitslosenquote",      
                            unit.variable          = "StateID",      
                            time.variable          = "Year2",              
                            unit.names.variable    = "Kanton",               
                            treatment.identifier   = 13,                         
                            controls.identifier    = setdiff(unique(data_scm_control_df$StateID), 13), 
                            time.predictors.prior  = 1:108,                       
                            time.optimize.ssr      = 1:108,                     
                            time.plot              = 1:204)

# weight table
synth.out_NE <- synth(dataprep.out)

## extract data for synt. Neuchatel
neuenburg_actual <- dataprep.out_NE$Y1plot

neuenburg_synthetic <- dataprep.out_NE$Y0plot %*% synth.out_NE$solution.w

# calculate overall gaps between Neucahtel and Synt. Neuchatel
gap_values <- neuenburg_actual - neuenburg_synthetic

# define Pre- and Post-treatment period
pre_treatment_period <- 1:108  
post_treatment_period <- 109:204

# calculate pre RMSPE
pre_rmspe <- sqrt(mean(gap_values[pre_treatment_period]^2))

# Calculate post RMSPE
post_rmspe <- sqrt(mean(gap_values[post_treatment_period]^2))

# calculate Share of RMSPE
rmspe_ratio_neuenburg <- post_rmspe / pre_rmspe


### Permutation Placebo Test (individually check all control cantons)
# create empty data.frame for RMSPE ratio
placebo_results <- data.frame(Kanton = character(), RMSPE_Ratio = numeric(), stringsAsFactors = FALSE)

# Iteration for all cantons except Neuchatel
for (kanton_id in unique(data_scm_control_df$StateID)) {
  if (kanton_id == 13) next  # skip Neuchatel
  
  # dataprep for all placebo cantons
  dataprep.out_placebo <- dataprep(
    data_scm_control_df,
    predictors             = c("Arbeitslosenquote"),
    dependent              = "Arbeitslosenquote",
    unit.variable          = "StateID",
    time.variable          = "Year2",
    unit.names.variable    = "Kanton",
    treatment.identifier   = kanton_id,
    controls.identifier    = setdiff(unique(data_scm_control_df$StateID), kanton_id),
    time.predictors.prior  = 1:108,
    time.optimize.ssr      = 1:108,
    time.plot              = 1:204
  )
  
  # extraction of data
  synth.out_placebo <- synth(dataprep.out_placebo)
  synth_kanton <- as.numeric(dataprep.out_placebo$Y0plot %*% synth.out_placebo$solution.w)
  
  # calculate RMSPE values
  pre_treatment_period <- 1:108
  post_treatment_period <- 109:204
  pre_rmspe <- sqrt(mean((dataprep.out_placebo$Y1plot[pre_treatment_period] - synth_kanton[pre_treatment_period])^2))
  post_rmspe <- sqrt(mean((dataprep.out_placebo$Y1plot[post_treatment_period] - synth_kanton[post_treatment_period])^2))
  
  # Berechne das RMSPE-Verhältnis und speichere das Ergebnis
  rmspe_ratio <- post_rmspe / pre_rmspe
  placebo_results <- rbind(placebo_results, data.frame(Kanton = unique(data_scm_control_df$Kanton[data_scm_control_df$StateID == kanton_id]), RMSPE_Ratio = rmspe_ratio))
}

# add Neuchatel to Control Cantons
placebo_results <- rbind(placebo_results, data.frame(Kanton = "Neuenburg", RMSPE_Ratio = rmspe_ratio_neuenburg))

# show results
print(placebo_results)

# ggplot to visualize distribution                 ############### [Figure 5]
figure5 <- ggplot(placebo_results, aes(x = RMSPE_Ratio)) +
  geom_histogram(bins = 25, fill = "gray", color = "black") +
  geom_vline(aes(xintercept = rmspe_ratio_neuenburg), color = "black", linetype = "dashed", size = 0.5) +
  labs(title = "Permutationstest: RMSPE-Verhältnisse für alle Kantone",
       x = "Post/Pre RMSPE Ratio",
       y = "Frequency") +
  annotate("text", x = rmspe_ratio_neuenburg + 0.08, y = 3.5, label = "Neuenburg", color = "black", vjust = 1) +
  theme_minimal() +
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size = 0.3), 
    axis.ticks.length = unit(0.3, "cm") 
  )

## save plots in output/
ggsave("output/Figure5_image.png",        
       plot = figure5,                  
       width = 10,                         
       height = 5,                          
       dpi = 300,                           
       bg = "white") 


## Calculation of Percentile-Rank for Neuchatel
neuenburg_percentile <- sum(placebo_results$RMSPE_Ratio <= rmspe_ratio_neuenburg) / nrow(placebo_results) * 100
cat("RSMPE Ratio for Neuchatel lies within", neuenburg_percentile, "Percnetile of all Cantons.\n")





### 15.2 Iterative Re-estimation ###
# 1. Calculate initial SCm with ALL control cantons
dataprep.out_full <- dataprep(
  foo = data_scm_control_df,
  predictors = c("Arbeitslosenquote"),
  dependent = "Arbeitslosenquote",
  unit.variable = "StateID",
  time.variable = "Year2",
  treatment.identifier = 13,  # Neuenburg as Treatment
  controls.identifier = setdiff(unique(data_scm_control_df$StateID), 13),
  time.predictors.prior = 1:108,
  time.optimize.ssr = 1:108,
  time.plot = 1:204
)

synth.out_full <- synth(dataprep.out_full)
synth_main <- (dataprep.out_full$Y0plot %*% synth.out_full$solution.w) * 100

# 2. Reduce Number of Control Cantons iteratively by 1 + save results
control_cantons <- setdiff(unique(data_scm_control_df$StateID), 13)
num_controls <- length(control_cantons)
synth_leave_one_out <- list()  # list to save results

for (k in num_controls:2) {
  # choose random control cantons
  selected_controls <- sample(control_cantons, k)
  
  # Calculate Synthetic Control for choosen Cantons
  dataprep.out_iter <- dataprep(
    foo = data_scm_control_df,
    predictors = c("Arbeitslosenquote"),
    dependent = "Arbeitslosenquote",
    unit.variable = "StateID",
    time.variable = "Year2",
    treatment.identifier = 13,
    controls.identifier = selected_controls,
    time.predictors.prior = 1:108,
    time.optimize.ssr = 1:108,
    time.plot = 1:204
  )
  
  synth.out_iter <- synth(dataprep.out_iter)
  synth_leave_one_out[[k]] <- (dataprep.out_iter$Y0plot %*% synth.out_iter$solution.w) * 100
}

# 3. change plot and add Years instead of numbers
plot_data <- data.frame(
  Year2 = 1:204,
  Datum = seq(as.Date("1993-01-01"), by = "month", length.out = 204),  
  Neuchatel = dataprep.out_full$Y1plot * 100,  
  SyntheticMain = synth_main  
)

# leave-one-out results
for (k in num_controls:2) {
  plot_data[[paste0("SyntheticLeave", k)]] <- synth_leave_one_out[[k]]
}

# just to keep the grey marks for whole period
plot_data_grey <- plot_data %>% filter(Datum >= as.Date("1993-01-01"))

# create plot using ggplot               ######################### [Figure C1]
p <- ggplot(plot_data_grey, aes(x = Datum)) +
  lapply(2:num_controls, function(k) {
    leave_one_out_col <- paste0("SyntheticLeave", k)
    if (leave_one_out_col %in% names(plot_data_grey)) {
      geom_line(aes_string(y = leave_one_out_col), color = "grey", linetype = "solid", alpha = 0.5)
    }
  }) +
  labs(title = "Unemployment Rate: Neuchâtel vs. Synthetic Neuchâtel",
       x = "Year",
       y = "Unemployment Rate (%)") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  theme_minimal() +
  theme(
    legend.position = "none",
    panel.border = element_rect(color = "black", fill = NA, size = 0.3),
    panel.grid = element_blank(),
    axis.ticks = element_line(color = "black", size = 0.3), 
    axis.ticks.length = unit(0.3, "cm") 
  )

# add relevant lines in polot
figureC1 <- p +
  geom_line(data = plot_data, aes(x = Datum, y = X13), color = "black", size = 0.6, linetype = "solid") +
  geom_line(data = plot_data, aes(x = Datum, y = w.weight), color = "black", size = 0.6, linetype = "dashed") +
  geom_vline(xintercept = as.Date("2002-01-01"), linetype = "dotted", color = "black", size = 0.6)

## save plots in output/
ggsave("output/figureC1_image.png",        
       plot = figureC1,                  
       width = 10,                         
       height = 5,                          
       dpi = 300,                           
       bg = "white") 






