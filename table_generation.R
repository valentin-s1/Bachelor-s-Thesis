################################################################################
##########  NC Enfranchisement & Labour Market Integration #####################
################################################################################

##### File 1/3: Table_Generation
#### Author: Valentin Sascha Schnellmann
### Date: 14.11.2024

#### Information:
# Data used for simple creation of Tables used in Bachelor's Thesis are
# noted in Footnotes. Most data is Author's own calculation or link to data
# is provided in Readme.md file.

### Tables of Content:
# Table 1: Accepted Referenda on NC Enfranchisement in Swiss Cantons
# Table 2: Share of Foreign Population in Swiss Cantons with NC Voting Rights
# Table A1: List of Referenda on NC Enfranchisement
# Table A2: Classification of Culturally Similar and Distant Countries
# Table A3: Cultural Distance across Cantons 2010 and 2023
# Table A4: Control Group Composition: Group I and II
# Table C1: Weights of Cantons in the SCM for Synthetic Neuchatel



################################ START #########################################

## First Step: load relevant packages to create Table
##              Mostly gt is used
# load packages
library(knitr)
library(kableExtra)
library(dplyr)
library(gt)
library(webshot2)
library(ragg)

# set chrome for Output creation
Sys.setenv(CHROMOTE_CHROME = "C:/Users/valen/AppData/Local/Google/Chrome/Application/chrome.exe")

########################
######  Table 1  #######
########################
### Approved Referenda
## create dataframe
table_data <- data.frame(
  "Vote Date" = c("20.03.1977", "30.04.1995", "24.09.2000", "22.09.2002", 
                  "18.05.2003", "16.05.2004", "23.03.2005", "24.04.2005", 
                  "17.06.2007", "28.09.2014"),
  "Canton" = c("Jura", "Appenzell A.R.", "Neuchâtel", "Vaud", "Grison", 
               "Fribourg", "Basel-City", "Geneva", "Neuchâtel", "Jura"),
  "Effective Date" = c("01.01.1997", "Opt-in", "01.01.2002", "14.04.2003", 
                       "Opt-in", "01.01.2005", "Opt-in", "24.04.2005", 
                       "17.06.2007", "28.09.2014"),
  "Yes Share" = c("80%", "Cantonal assembly", "76.60%", "55.90%", 
                  "59.70%", "58.00%", "76.50%", "52.30%", 
                  "54.40%", "54.00%"),
  "Suffrage Type" = c("Active local+cantonal", "Full local", 
                      "Active cantonal", "Full local", "Full local", 
                      "Full local", "Full local", "Active local", 
                      "Passive local", "Passive local"),
  "Opt-in" = c("NO", "YES", "NO", "NO", "YES", "NO", "YES", 
               "NO", "NO", "NO"),
  "Nr. Municipalities" = c("All", "4/20", "All", "All", "30/101", "All", "0", 
                           "All", "All", "All")
)

## change colnames
colnames(table_data) <- c("Vote Date", "Canton", "Effective Date", "Yes Share",  
                          "Suffrage Type", "Opt-in", "Nr. Municipalities")

## create table with gt (Table 1)
table1 <- table_data %>%
  gt() %>%
  tab_options(
    heading.align = "left"  # Aligns the title to the left
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "black")  # Ensures the text is black
    ),
    locations = cells_body()  # Applies to the entire table body
  ) %>%
  tab_footnote(
    footnote = "While passive voting rights at the municipal level were approved, 
    those at the cantonal level were rejected.",
    locations = cells_body(
      columns = vars(`Vote Date`),
      rows = table_data$`Vote Date`== "17.06.2007"
    )) %>%  
  tab_source_note(source_note = "Note: full = passive and active voting rights.   
                  Sources: Adlerer et al. (2016), Koukal et al. (2021)")

## save table
gtsave(
  table1, 
  "output/table1_image.png", 
  vwidth = 1000, 
  vheight = 800, 
)


#######################
######  Table 2  ######
#######################
### Foreigner Share (own calculation)
## Create dataframe
data_foreigner_share <- data.frame(
  Cantons = c("Appenzell A.R.", "Freiburg", "Geneva", "Grisons", "Jura", "Neuchâtel", "Vaud", "Weighted Average", "Switzerland"),
  `Share of Foreigners (%)` = c(17.8, 24.7, 41.7, 20.6, 15.9, 26.6, 33.6, 32.5, 27.0)
)

## change colnames
colnames(data_foreigner_share) <- c("Cantons", "Share of Foreginers (%)")

## create tahle with gt (Table 2)
table2 <- data_foreigner_share %>%
  gt() %>%
  tab_options(
    heading.align = "left"  # Aligns the title to the left
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),  # Make text bold
    locations = cells_body(
      rows = data_foreigner_share$Cantons %in% c("Weighted Average", "Switzerland") 
    )
  ) %>%
  tab_footnote(
    footnote = "The weighted average considers only those municipalities that have 
    implemented non-citizen voting rights within cantons where this policy is not 
    fully implemented canton-wide.",
    locations = cells_body(
      columns = vars(`Cantons`),
      rows = data_foreigner_share$Cantons== "Weighted Average"
    )) %>%
  tab_source_note(source_note = "Sources: Swiss Federal Statistical Office, 
                  Authors' own calculations")

## save table
gtsave(
  table2, 
  "output/table2_image.png", 
  vwidth = 800, 
  vheight = 800, 
)



#######################
######  TableA1  ######
#######################
### Total list of referendum
## create dataframe
ref_list <- data.frame(
  "Vote Date" = c("27.09.1992", "06.06.1993", "28.11.1993", "26.09.1993",
                  "12.06.1994", "04.12.1994", "04.12.1994", "22.10.1995",
                  "10.03.1996", "09.06.1996", "16.03.1997", "23.11.1997",
                  "24.09.2000", "04.03.2001", "04.03.2001", "22.09.2002",
                  "16.05.2004", "30.10.2005", "24.04.2005", "24.04.2005",
                  "25.09.2005", "17.06.2007", "17.06.2007", "17.06.2007",
                  "26.09.2010", "26.09.2010", "26.09.2010", "04.09.2011",
                  "27.11.2011", "22.09.2013", "28.09.2014", "28.09.2014",
                  "25.09.2016", "09.06.2023"),
  "Canton" = c("VD", "GE", "GE", "ZH", "BS", "BE", "BE", "UR", "AG", "JU",
               "FR", "SO", "NE", "GE", "SH", "VD", "FR", "BS", "GE", "GE",
               "SO", "JU", "NE", "NE", "BS", "BS", "BE", "VD", "LU", "ZH",
               "JU", "SH", "NE", "GE"),
  "Suffrage" = c("Full local + full cantonal", "Full local", "Eligibility court of arbitration", 
                 "Full local opt-in", "Full local + full cantonal", "Full local + full cantonal", 
                 "Full local opt-in", "Active cantonal", "Full local", "Passive local opt-in", 
                 "Full local", "Full local + full cantonal", "Active cantonal", "Full local", 
                 "Active local + active cantonal", "Full local", "Full local", "Full local opt-in", 
                 "Full local", "Active local + active cantonal", "Full local opt-in", 
                 "Full local for executive", "Passive local + passive cantonal", "Passive local", 
                 "Full cantonal", "Active cantonal", "Full local opt-in", "Full cantonal", 
                 "Active local opt-in", "Full local opt-in", "Full local for executive", 
                 "Full local + full cantonal", "Passive cantonal", "Passive cantonal"),
  "Accepted" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0, 1, 
                 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0),
  "Yes Share" = c("26%", "29%", "45%", "26%", "26%", "22%", "40%", "16%", "16%", 
                  "47%", "24%", "12%", "77%", "48%", "30%", "56%", "58%", "77%", 
                  "47%", "52%", "39%", "49%", "41%", "54%", "19%", "39%", "28%", 
                  "31%", "16%", "25%", "54%", "15%", "46%", "39%"),
  "Vote Type" = c("Initiative", "Initiative", "Counterproposal", "Initiative", 
                  "Initiative", "Initiative", "Counterproposal", "Initiative", 
                  "Initiative", "Referendum", "Initiative", "Initiative", 
                  "Complete revision", "Law revision", "Partial revision", 
                  "Complete revision", "Complete revision", "Complete revision", 
                  "Initiative", "Initiative", "Complete revision", "Law revision", 
                  "Initiative", "Counterproposal", "Initiative", "Counterproposal", 
                  "Initiative", "Initiative", "Initiative", "Initiative", "Law revision", 
                  "Initiative", "Law revision", "Initiative")
)

## change colnames
colnames(ref_list) <- c("Vote Date", "Canton", "Suffrage", "Accepted",  
                        "Yes Share", "Vote Type")

# Create table with gt
tableA1 <- ref_list %>%
  gt() %>%
  tab_options(
    table.font.size = 12, # Adjust font size as needed
    heading.align = "left"  # Aligns the heading to the left
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "black")  # Ensures the text is black
    ),
    locations = cells_body()  # Applies to the entire table body
  ) %>%
  tab_source_note(source_note = "Source: Adler et al. (2016), cantonal archives, 
                  cantonal chancelleries")

# Save table (optional)
gtsave(
  tableA1,
  "output/tableA1_image.png", 
  vwidth = 2500, 
  vheight = 3500)

#######################
######  Table A2  ######
#######################
### Classification of Culturally Distant Countries
## Create dataframe
# Data for cultural distance table
cultural_distance <- data.frame(
  Category = c("Classified as Culturally Similar", rep("", 11), "Classified as Culturally Distant"),
  Column1 = c("Australia", "Austria", "Belgium", "Canada", "Denmark", "Finland", 
              "France", "Germany", "Great Britain", "Iceland", "Ireland", "Italy", 
              "All remaining countries"),
  Column2 = c("Liechtenstein", "Japan", "Netherlands", "New Zealand", "Norway", 
              "Portugal", "South Korea", "Spain", "Sweden", "Taiwan", "United States", "", "")
)


# Create the table using gt
cultural_table <- cultural_distance %>%
  gt() %>%
  tab_options(
    heading.align = "left"
  ) %>%
  cols_label(
    Column1 = "Countries",
    Column2 = " "
  ) %>%
  cols_align(
    align = "left",
    columns = c(Category, Column1, Column2)
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "black")  # Ensures the text is black
    ),
    locations = cells_body()  # Applies to the entire table body
  ) %>%
  tab_source_note(source_note = "Note: Cultural distance based on Inglehart and Baker (2000).
                  \n Lichtenstein does not participate in the WVS and was therefore 
                  not included in Inglehart and Baker's (2002) classifications.
                  \n I added Lichtenstein to the culturally similar countries, 
                  since they are arguably culturally close to the Swiss population.") %>%
  tab_source_note(source_note = "Source: Koukal et al. (2021)")


cultural_table

# Save table as image
gtsave(
  cultural_table,
  "output/tableA2_image.png", 
  vwidth = 1000, 
  vheight = 800
)


#######################
######  Table A3  #####
#######################
### Cultural Distance (share) 2010 and 2023
# Create dataframe
data_cultural_distance <- data.frame(
  Canton = c("Switzerland", "Zurich", "Bern", "Lucerne", "Uri", "Schwyz", "Obwalden", 
             "Nidwalden", "Glarus", "Zug", "Fribourg", "Solothurn", "Basel-City", 
             "Basel-Country", "Schaffhausen", "Appenzell A.R.", "Appenzell I.R.", 
             "St. Gallen", "Grisons", "Aargau", "Thurgau", "Ticino", "Vaud",
             "Valais", "Neuchâtel", "Geneva", "Jura"),
  
  Cultural_Distance_2010 = c(0.385553908, 0.397373298, 0.446507213, 0.515471458, 0.42103738, 
                             0.474379138, 0.370486986, 0.33354742, 0.451060048, 0.391179326, 
                             0.345006, 0.556101913, 0.421003578, 0.428167047, 0.471588012, 
                             0.379475576, 0.433035714, 0.521413061, 0.238443808, 0.491074494, 
                             0.388620103, 0.22542168, 0.29320732, 0.247754058, 0.257262669, 
                             0.329670207, 0.291043925),
  
  Cultural_Distance_2023 = c(0.433995866, 0.448591056, 0.532308387, 0.54023836, 0.493796526, 
                             0.482004819, 0.401739941, 0.404162384, 0.494171862, 0.427777639, 
                             0.353645833, 0.603365418, 0.43241337, 0.479235192, 0.490656289, 
                             0.454581673, 0.478702807, 0.5443614, 0.3287905, 0.528482017, 
                             0.426218222, 0.239025845, 0.338068621, 0.277930577, 0.300413171, 
                             0.395794846, 0.399243061)
)

# Sort the data by 'Cultural_Distance_2010', but keep 'Switzerland' at the top
data_cultural_distance <- data_cultural_distance[-1, ]  # Remove Switzerland for sorting
data_cultural_distance <- data_cultural_distance[order(-data_cultural_distance$Cultural_Distance_2010), ]  # Sort other cantons
data_cultural_distance <- rbind(data.frame(Canton = "Switzerland", 
                                           Cultural_Distance_2010 = 0.385553908, 
                                           Cultural_Distance_2023 = 0.433995866), 
                                data_cultural_distance)  # Add Switzerland back on top

# Create new variables for the rank in 2010 and 2023 (excluding Switzerland from ranking)
data_cultural_distance$Rank_2010 <- c(NA, rank(-data_cultural_distance$Cultural_Distance_2010[-1]))
data_cultural_distance$Rank_2023 <- c(NA, rank(-data_cultural_distance$Cultural_Distance_2023[-1]))

# Create a column for the difference between 2010 and 2023
data_cultural_distance$Difference <- data_cultural_distance$Cultural_Distance_2023 - data_cultural_distance$Cultural_Distance_2010

# Adjust column names for display (remove underscores)
colnames(data_cultural_distance) <- c("Canton", "Cultural Distance (2010)", "Cultural Distance (2023)", "Rank 2010", "Rank 2023", "Difference")

# Create the table using gt
cultural_distance_table <- data_cultural_distance %>%
  gt() %>%
  tab_options(
    heading.align = "left"  # Align the title to the left
  ) %>%
  tab_footnote(
    footnote = "Difference is the change in Cultural Distance from 2010 to 2023",
    locations = cells_body(
      columns = vars(Difference),
      rows = data_cultural_distance$Difference == "0.048441958"
    )) %>%
  tab_footnote(
    footnote = "
Levene's Test for Homogeneity of Variance shows that the variance of Cultural 
    Distance between 2010 and 2023 is not significantly different (F(1, 52) = 0.0621, p = 0.8042). 
    This indicates that the differences between the cantons have remained relatively stable 
    over this period."
  ) %>%
  tab_source_note(
    source_note = "Sources: Swiss Federal Statistical Office, Authors' own calculations"
  )

# Save table as image (optional)
gtsave(
  cultural_distance_table,
  "output/tableA3_image.png", 
  vwidth = 900, 
  vheight = 2200
)

## variance test
# calcualte variance for years 2010 and 2023
var_2010 <- var(data_cultural_distance$`Cultural Distance (share) 2010`)
var_2023 <- var(data_cultural_distance$`Cultural Distance (share) 2023`)

# pring variance
var_2010
var_2023

library(car)
leveneTest(Cultural_Distance ~ Year, data = data.frame(
  Cultural_Distance = c(data_cultural_distance$`Cultural Distance (share) 2010`, 
                        data_cultural_distance$`Cultural Distance (share) 2023`),
  Year = factor(rep(c(2010, 2023), each = nrow(data_cultural_distance)))
))


# ANOVA with interaction of year / canton
data_long_cultural_distance <- data.frame(
  Cultural_Distance = c(data_cultural_distance$`Cultural Distance (share) 2010`, 
                        data_cultural_distance$`Cultural Distance (share) 2023`),
  Canton = rep(data_cultural_distance$Canton, 2),
  Year = factor(rep(c(2010, 2023), each = nrow(data_cultural_distance)))
)

# ANOVA
result_interaction <- aov(Cultural_Distance ~ Canton * Year, data = data_long_cultural_distance)
summary(result_interaction)


#######################
######  Table A4  #####
#######################
### Group I and Group II
# Create dataframe
group_list <- data.frame(
  Category = c("Control Canton", rep("", 8), "Treatment Canton"),
  Column1 = c("Aargau", "Appenzell I.R.", "Basel-Landschaft", "Bern", "Glarus", "Lucerne", 
              "Nidwalden", "Obwalden", "Schaffhausen", "Neuchâtel"),
  Column2 = c("Schwyz", "Solothurn", "St.Gallen", "Thurgau", "Ticino", 
              "Uri", "Valais", "Zug", "Zurich", ""),
  Column3 = c("Aargau", "Bern", "Lucerne", "Schaffhausen", "Solothurn", "St.Gallen", "Thurgau", 
              "Schwyz", "Zug", "Neuchâtel")
)

# Create the table using gt
group_table <- group_list %>%
  gt() %>%
  tab_options(
    heading.align = "left"
  ) %>%
  cols_label(
    Column1 = "Group I",
    Column2 = "",
    Column3 = "Group II"
  ) %>%
  cols_align(
    align = "left",
    columns = c(Category, Column1, Column2, Column3)
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "black")  # Ensures the text is black
    ),
    locations = cells_body()  # Applies to the entire table body
  ) %>%
  tab_source_note(source_note = "Note: Group II consists of non-neighboring, similar
  cantons to Neuchâtel. Bern is included despite being a minimal neighboring canton 
                  due to close structural alignment.")


group_table
# Save table as image
gtsave(
  group_table,
  "output/tableA4_image.png", 
  vwidth = 800, 
  vheight = 600
)


#######################
######  Table C1  #####
#######################
### Weight Table for SCM (Data from own Calculation in other file)
## Create dataframe
data_weights <- data.frame(
  Category = c("", rep("                ", 9), "RMSPE (Pre-treatment)"),
  Column1 = c(0.045, 0.040, 0.044, 0.046, 0.042, 0.046, 0.042, 0.043, 0.046, "", "1.185×10⁻⁴"),
  Column2 = c("Zurich", "Bern", "Lucerne", "Uri", "Schwyz", "Obwalden", 
              "Nidwalden", "Glarus", "Zug", "", ""),
  Column3 = c(0.044, 0.046, 0.044, 0.046, 0.044, 0.043, 0.251, 0.044, 0.045, "", ""), 
  Column4 = c("Solothurn", "Basel-Landschaft", "Schaffhausen", "Appenzell I.R.", 
              "St.Gallen", "Aargau", "Thurgau", "Ticino", "Valais", "", ""))

# Create the table using gt
weight_table <- data_weights %>%
  gt() %>%
  tab_options(
    heading.align = "left"
  ) %>%
  cols_label(
    Category = "",
    Column1 = "Weight",
    Column2 = "Canton",
    Column3 = "Weight",
    Column4 = "Canton"
  ) %>%
  cols_align(
    align = "left",
    columns = c(Category, Column1, Column2, Column3, Column4)
  ) %>%
  tab_style(
    style = list(
      cell_text(color = "black")  # Ensures the text is black
    ),
    locations = cells_body()  # Applies to the entire table body
  ) %>%
  tab_source_note(source_note = "Note: Weights represent the contribution of each 
  canton to the synthetic control unit for Neuchâtel. The RMSPE (Root Mean Squared Prediction Error) 
  in the pre-treatment period is 1.185×10⁻⁴, indicating the goodness-of-fit of the synthetic control.")

## save table
gtsave(
  weight_table, 
  "output/tableC1_image.png", 
  vwidth = 800, 
  vheight = 800, 
)


#################################### END #######################################