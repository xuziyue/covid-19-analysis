# Author: Ziyue Xu
# Data: 22nd December 2020
# Contact: ziyue.xu@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(tidyverse)

raw_covid19 <- read_csv("COVID19-eng.csv")

# remove Not Stated/Unknown values
raw_covid19 <- raw_covid19[which(raw_covid19$`Age group` != 99), ]
raw_covid19 <- raw_covid19[which(raw_covid19$Death != 9), ]
raw_covid19 <- raw_covid19[which(raw_covid19$Asymptomatic != 9), ]
raw_covid19 <- raw_covid19[which(raw_covid19$`Hospital status` != 9), ]

# create one variable for the response variable
covid19 <-
  raw_covid19 %>% 
  mutate(Fatal_outcome = ifelse(Death==1, 1, 0))

# mutate age category
covid19 <- covid19 %>%
  mutate(Age_group_modified = case_when(`Age group` == 1 ~ "age 0-19",
                                        `Age group` == 2 | `Age group` == 3 ~ "age 20-39",
                                        `Age group` == 4 | `Age group` == 5 ~ "age 40-59",
                                        `Age group` == 6 | `Age group` == 7 | `Age group` == 8 ~ "age >= 60"
                                        ))

# mutate gender category
covid19 <- covid19 %>%
  mutate(Gender_modified = case_when(Gender == 1 ~ "Male",
                                     Gender == 2 ~ "Female",
                                     Gender == 9 ~ "Not stated/Other"
                                     ))

# mutate whether is asymptomatic category
covid19 <- covid19 %>%
  mutate(Asymptomatic_modified = case_when(Asymptomatic == 1 ~ "Yes",
                                           Asymptomatic == 2 ~ "No"))

# mutate hospital status category
covid19 <- covid19 %>%
  mutate(Hospital_status_modified = case_when(`Hospital status` == 1 ~ "Hospitalized - ICU",
                                              `Hospital status` == 2 ~ "Hospitalized - Non-ICU",
                                              `Hospital status` == 3 ~ "Not Hospitalized"))

# mutate region category
covid19 <- covid19 %>%
  mutate(Region_modified = case_when(Region == 1 ~ "Atlantic",
                                     Region == 2 ~ "Quebec",
                                     Region == 3 ~ "Ontario and Nunavut",
                                     Region == 4 ~ "Prairies",
                                     Region == 5 ~ "British Columbia and Yukon"
                                     ))


# mutate (create) a new variable called Developed_regions indicates whether the
# region is Ontario and Nunavut, or Quebec
covid19 <- covid19 %>%
  mutate(Developed_regions = ifelse(Region_modified=="Ontario and Nunavut" |
                                      Region_modified=="Quebec", 1, 0))

# keep variables of interests
cleaned_covid19 <- 
  covid19 %>% 
  select(Fatal_outcome,
         Age_group_modified,
         Gender_modified,
         Asymptomatic_modified,
         Hospital_status_modified,
         Region_modified,
         Developed_regions)

# Saving the cleaned observational data as a csv file in my working directory
write_csv(cleaned_covid19, "covid19_cases_canada.csv")
