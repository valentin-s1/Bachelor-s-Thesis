################################################################################
##########  NC Enfranchisement & Labour Market Integration #####################
################################################################################

##### File 2/3: Parallel_Trend_Assumption_Check
#### Author: Valentin Sascha Schnellmann
### Date: 14.11.2024

#### Description:
# This R file is used to check the Parallel Trend Assumption for a DiD analysis
# investigating the effects of granting voting rights to NC on the unemployment
# rate in Neuchatel.
# The file includes steps for visualizing trends, perfoming statistical tests
# for parallelism, and conducting potential placebo tests.
#
#
#### Data used:
# Unemployment data per canton (1993-2009) from the Federal Statistical Office
# (SECO, 2020); see link to data in README.md), control variables such as:
# - foreigner share per canton, annually (BFS, 2023),
# - SP vote share per canton, all 4 years (BFS, 2015),
# - cultural distance share, constant (BFS, 2024)
#
#
#### Steps:
# 1. Load relevant packages and data
# 2. Visual Insepction of Trends for P.T.A
# 3. Lead Variable Approach
# 4. Placebo Tests Pre-Treat.
#
### Tables of Content: Appendix B
# Figure 1: Unemployment Rate: Neuchatel vs. Sub-Control Group (Mean)
# Figure B1: Unemployment Rate Trends: Neuchatel vs. Group II (Individual and Mean)
# Figure B2: Unemployment Rate Trends: Neuchatel vs. Group I (Mean)
# Figure B3: Unemployment Rate Trends: Neuchatel vs. Group I (Individual and Mean)
# Table B1: Regression Results for Parallel Trend Test Using Lead Variables
# Table B2: Placebo Test Results (1990-2001) - Group II and Group I



################################ START #########################################

#### First Step: load relevant packages ####
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

# load data
data_raw <- read_excel("Data1/Clean_Arbeitslosenquote.xlsx")

## Data cleaning and Preparation
#adjust columns
data_raw <- data_raw[-1, ]
data_raw <- data_raw[ , -2]

names(data_raw) <- trimws(names(data_raw))

#data in longformat
data_long <- data_raw %>%
  pivot_longer(cols = -Kanton, names_to = "Monat_Jahr", values_to = "Arbeitslosenquote") %>%
  separate(Monat_Jahr, into = c("Monat", "Jahr"), sep = " ") %>%
  mutate(Datum = as.Date(paste(Jahr, Monat, "01", sep = "-"), "%Y-%B-%d")) %>%
  filter(!is.na(Kanton))

#data for DiD 
# Create the StateID, treatment, post variable and filter for relevant controls
data_long_did <- data_long %>%
  mutate(StateID = as.numeric(as.factor(Kanton))) %>%
  mutate(treatment = ifelse(Kanton == "Neuenburg", 1, 0)) %>%
  mutate(
    time_numeric = as.numeric(format(as.Date(Datum), "%Y")) + 
      (as.numeric(format(as.Date(Datum), "%m")) - 1) / 12) %>%
  mutate(post = ifelse(Datum >= as.Date("2001-01-01"), 1, 0)) %>%  
  filter(!Kanton %in% c("Jura", "Appenzell Ausserrhoden", "Wallis", "Graubünden",
                        "Freiburg","Basel-Stadt", "Genf")) %>% 
  mutate(Arbeitslosenquote = as.numeric(Arbeitslosenquote))

## create data sets for following analysis
# data for control group (whole Switzerland excl. cantons with NCV rights)
data_long_control <- data_long_did

# data for sub control group (parallel trend assumption fulfilled)
data_long_subcontrol <- data_long_did %>%
  filter(Kanton %in% c("Neuenburg", "Aargau", "Bern", 
                       "Luzern", "Schaffhausen", "Solothurn", 
                       "St. Gallen", "Thurgau", 
                       "Schwyz", "Zug"))



#### END: Cleaning and Preparation ####




############ Step 2: Visual Insepction of Trends for P.T.A #####################

## Visual Inspection of Control Group (Group I)
## only compare means
# Create a summary dataframe with the mean unemployment rate for control cantons
control_means <- data_long_control %>%
  filter(Kanton != "Neuenburg") %>%
  group_by(Datum) %>%
  summarise(Mean_Unemployment = mean(Arbeitslosenquote))

# Plot Neuchatel and Group 1 (control cantons / mean) ######### [Figure B2]

figureB2 <- ggplot() +
  geom_line(data = control_means, aes(x = Datum, 
                                      y = Mean_Unemployment, 
                                      linetype = "Control Cantons (mean)"), 
            size = 0.6, color = "black") + 
  geom_vline(xintercept = as.Date("2002-01-01"), 
             linetype = "dotted", 
             color = "black", 
             size = 0.6) +  
  geom_line(data = data_long_control %>% filter(Kanton == "Neuenburg"), 
            aes(x = Datum, 
                y = Arbeitslosenquote, 
                linetype = "Neuchâtel"), 
            size = 0.6, 
            color = "black") +
  scale_linetype_manual(values = c("Neuchâtel" = "solid", 
                                   "Control Cantons (mean)" = "dashed")) + 
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) +  
  labs(title = "Unemployment Rate: Neuenburg vs. Group I (Mean)",
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


# Adjusted plot to include all control cantons in light gray ######## [Figure B3]
figureB3 <- ggplot() +
  # Add individual control canton lines with low opacity (light grey)
  geom_line(data = data_long_control %>% filter(Kanton != "Neuenburg"), 
            aes(x = Datum, y = Arbeitslosenquote, group = Kanton), 
            color = "lightgrey", size = 0.5, alpha = 0.4) +
  # Add the mean unemployment rate for the control group
  geom_line(data = control_means, aes(x = Datum, 
                                      y = Mean_Unemployment, 
                                      linetype = "Control Cantons (mean)"), 
            size = 0.6, color = "black") +  
  # Add a vertical dashed line for the policy intervention
  geom_vline(xintercept = as.Date("2002-01-01"), 
             linetype = "dotted",
             color = "black", size = 0.6) +  
  # Add the unemployment rate for Neuchâtel
  geom_line(data = data_long_control %>% filter(Kanton == "Neuenburg"), 
            aes(x = Datum, 
                y = Arbeitslosenquote, 
                linetype = "Neuchâtel"), 
            size = 0.6, color = "black") +  
  scale_linetype_manual(values = c("Neuchâtel" = "solid", 
                                   "Control Cantons (mean)" = "dashed")) +  
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) +  
  labs(title = "Unemployment Rate: Neuenburg vs. Group I (Mean) with Individual Control Cantons",
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
ggsave("output/figureB2_image.png", plot = figureB2, width = 10, height = 5, bg = "white")
ggsave("output/figureB3_image.png", plot = figureB3, width = 10, height = 5, bg = "white")


### Visual Inspection of Sub-Control Group (Group II)
##individuel inspection leads to inclusion of following cantons:
# Aargau, Bern, Luzern, Schaffhausen, Solothurn, St. Gallen, Thurgau, Schwyz, Zug

## only compare means
# Create a summary dataframe with the mean unemployment rate for control cantons
subcontrol_means <- data_long_subcontrol %>%
  filter(Kanton != "Neuenburg") %>%
  group_by(Datum) %>%
  summarise(Mean_Unemployment = mean(Arbeitslosenquote))

# Plot Neuenburg and control group means ######## [Figure 1]
figure1 <- ggplot() +
  geom_line(data = subcontrol_means, aes(x = Datum, y = Mean_Unemployment, 
                                         linetype = "Control Cantons (mean)"), 
            size = 0.6, color = "black") +
  geom_vline(xintercept = as.Date("2002-01-01"), 
             linetype = "dotted", 
             color = "black", 
             size = 0.6) +
  geom_line(data = data_long_subcontrol %>% filter(Kanton == "Neuenburg"), 
            aes(x = Datum, y = Arbeitslosenquote, linetype = "Neuchâtel"), 
            size = 0.6, color = "black") +
  scale_linetype_manual(values = c("Neuchâtel" = "solid", 
                                   "Control Cantons (mean)" = "dashed")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) +  
  labs(title = "Unemployment Rate: Neuenburg vs. Group II (means)",
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

# Include all Control Cantons next to mean ######## [Figure B1]
figureB1 <- ggplot() +
  # Add individual control canton lines with low opacity (light grey)
  geom_line(data = data_long_subcontrol %>% filter(Kanton != "Neuenburg"), 
            aes(x = Datum, y = Arbeitslosenquote, group = Kanton), 
            color = "lightgrey", size = 0.5, alpha = 0.4) +
  # Add the mean unemployment rate for the control group
  geom_line(data = subcontrol_means, aes(x = Datum, 
                                         y = Mean_Unemployment, 
                                         linetype = "Control Cantons (mean)"), 
            size = 0.6, color = "black") +
  # Add a vertical dashed line for the policy intervention
  geom_vline(xintercept = as.Date("2002-01-01"), linetype = "dotted", 
             color = "black", size = 0.6) +
  # Add the unemployment rate for Neuenburg
  geom_line(data = data_long_subcontrol %>% filter(Kanton == "Neuenburg"), 
            aes(x = Datum, y = Arbeitslosenquote, linetype = "Neuchâtel"), 
            size = 0.6, color = "black") +
  scale_linetype_manual(values = c("Neuchâtel" = "solid", 
                                   "Control Cantons (mean)" = "dashed")) +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(0, 15, by = 1)) +  
  labs(title = "Unemployment Rate: Neuenburg vs. Group II",
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
ggsave("output/figure1_image.png", plot = figure1, width = 10, height = 5, bg = "white")
ggsave("output/figureB1_image.png", plot = figureB1, width = 10, height = 5, bg = "white")



#################### Step 3: Lead Variable Approach ############################
### Group I (Swiss Cantons w/o NC enfranchsiement)
# Filter the data for pre-treatment period (before 2002) and create leads
pre_treatment_control_test <- data_long_control %>%
  filter(time_numeric < 2002) %>%
  mutate(
    Arbeitslosenquote = Arbeitslosenquote / 100,
    lead1 = ifelse(time_numeric >= 2001 & time_numeric < 2002 & treatment == 1, 1, 0),
    lead2 = ifelse(time_numeric >= 2000 & time_numeric < 2001 & treatment == 1, 1, 0),
    lead3 = ifelse(time_numeric >= 1999 & time_numeric < 2000 & treatment == 1, 1, 0), 
    lead4 = ifelse(time_numeric >= 1998 & time_numeric < 1999 & treatment == 1, 1, 0),
    lead5 = ifelse(time_numeric >= 1997 & time_numeric < 1998 & treatment == 1, 1, 0),
    lead6 = ifelse(time_numeric >= 1996 & time_numeric < 1997 & treatment == 1, 1, 0),
    lead7 = ifelse(time_numeric >= 1995 & time_numeric < 1996 & treatment == 1, 1, 0),
    lead8 = ifelse(time_numeric >= 1994 & time_numeric < 1995 & treatment == 1, 1, 0),
    lead9 = ifelse(time_numeric >= 1993 & time_numeric < 1994 & treatment == 1, 1, 0)
  )


# Run a regression to check parallel trends (no post-treatment periods)
parallel_trend_model_control <- feols(Arbeitslosenquote ~ lead1 + lead2 + 
                                        lead3 + lead4 + lead5 + lead6 + 
                                        lead7 + lead8 | StateID + time_numeric, 
                                      data = pre_treatment_control_test)



### Group II (Sub-Control Group)
# Filter the data for pre-treatment period (before 2002) and create leads
pre_treatment_subcontrol_test <- data_long_subcontrol  %>%
  filter(time_numeric < 2002) %>%
  mutate(
    Arbeitslosenquote = Arbeitslosenquote / 100,
    lead1 = ifelse(time_numeric >= 2001 & time_numeric < 2002 & treatment == 1, 1, 0),
    lead2 = ifelse(time_numeric >= 2000 & time_numeric < 2001 & treatment == 1, 1, 0),
    lead3 = ifelse(time_numeric >= 1999 & time_numeric < 2000 & treatment == 1, 1, 0), 
    lead4 = ifelse(time_numeric >= 1998 & time_numeric < 1999 & treatment == 1, 1, 0),
    lead5 = ifelse(time_numeric >= 1997 & time_numeric < 1998 & treatment == 1, 1, 0),
    lead6 = ifelse(time_numeric >= 1996 & time_numeric < 1997 & treatment == 1, 1, 0),
    lead7 = ifelse(time_numeric >= 1995 & time_numeric < 1996 & treatment == 1, 1, 0),
    lead8 = ifelse(time_numeric >= 1994 & time_numeric < 1995 & treatment == 1, 1, 0),
    lead9 = ifelse(time_numeric >= 1993 & time_numeric < 1994 & treatment == 1, 1, 0)
  )


# Run a regression to check parallel trends (no post-treatment periods)
parallel_trend_model_subcontrol <- feols(Arbeitslosenquote ~ lead1 + lead2 + 
                                           lead3 + lead4 + lead5 + lead6 + 
                                           lead7 + lead8 | StateID + time_numeric, 
                                         data = pre_treatment_subcontrol_test)

## combine Regression table ################ [Table B1]
etable(parallel_trend_model_subcontrol, parallel_trend_model_control)



#################### Step 4: Placebo Tests Pre-Treat. ##########################
### Group I (Swiss Cantons w/o NC enfranchsiement)
## create placebo dataset
placebo_test_control <- data_long_control %>%
  mutate(Arbeitslosenquote = Arbeitslosenquote / 100) %>%
  filter(time_numeric < 2002) %>%
  #remove years that violate PTA
  dplyr::filter(Jahr >= 1998) %>% 
  #create fake treatment years Interactions
  dplyr::mutate(
    FakeTreat1 = ifelse(Kanton == "Neuenburg" & Datum >= as.Date("1999-01-01"), 1, 0),
    FakeTreat2 = ifelse(Kanton == "Neuenburg" & Datum >= as.Date("2000-01-01"), 1, 0),
    FakeTreat3 = ifelse(Kanton == "Neuenburg" & Datum >= as.Date("2001-01-01"), 1, 0)
  )

# Run Regressions
control_1999 <- fixest::feols(Arbeitslosenquote ~ FakeTreat1 | StateID + time_numeric,
                              data = placebo_test_control)
control_2000 <- fixest::feols(Arbeitslosenquote ~ FakeTreat2 | StateID + time_numeric,
                              data = placebo_test_control)
control_2001 <- fixest::feols(Arbeitslosenquote ~ FakeTreat3 | StateID + time_numeric,
                              data = placebo_test_control)


### Group II (Sub-Conrol Group)
## create placebo dataset
placebo_test <- data_long_subcontrol %>%
  mutate(Arbeitslosenquote = Arbeitslosenquote / 100) %>%
  filter(time_numeric < 2002) %>%
  #remove years that violate PTA
  dplyr::filter(Jahr >= 1998) %>% 
  #create fake treatment years Interactions
  dplyr::mutate(
    FakeTreat1 = ifelse(Kanton == "Neuenburg" & Datum >= as.Date("1999-01-01"), 1, 0),
    FakeTreat2 = ifelse(Kanton == "Neuenburg" & Datum >= as.Date("2000-01-01"), 1, 0),
    FakeTreat3 = ifelse(Kanton == "Neuenburg" & Datum >= as.Date("2001-01-01"), 1, 0)
  )

# Run Regressions
subcontrol_1999 <- fixest::feols(Arbeitslosenquote ~ FakeTreat1 | StateID + time_numeric,
                                 data = placebo_test)
subcontrol_2000 <- fixest::feols(Arbeitslosenquote ~ FakeTreat2 | StateID + time_numeric,
                                 data = placebo_test)
subcontrol_2001 <- fixest::feols(Arbeitslosenquote ~ FakeTreat3 | StateID + time_numeric,
                                 data = placebo_test)


## combine Regression table ################ [Table B2]
etable(subcontrol_1999, subcontrol_2000, subcontrol_2001, control_1999, control_2000, control_2001)


