# Vaccines_Structural_Equation_Modelling 
# Code for the article 'A Structural Equation Modelling Approach to Understanding the Determinants of Childhood Vaccination in Nigeria, Uganda and Guinea'
# This script takes the raw data file and prepares it for the main analysis 
# March 2022

# Packages ----------------------------------------------------------------
library(dplyr) 
library(forcats)
library(hablar)
library(here)
library(sjPlot)


# Functions ---------------------------------------------------------------
AgreementScale <- function(variable) {
  recode_factor(
    variable,
    "1" = "Strongly disagree",
    "2" = "Slightly disagree",
    "3" = "Neither agree nor disagree",
    "4" = "Slightly agree",
    "5" = "Strongly agree",
    "6" = "(DK)/(Refused)", 
    "0" = "(DK)/(Refused)", 
  )
}

DKrecode <- function(variable){
  recode_factor(variable, 
                "(DK)/(Refused)" = "Neither agree nor disagree")
}

numeric_code <- function(x){
  case_when(
    x == "Strongly disagree" ~ 1, 
    x == "Slightly disagree" ~ 2, 
    x == "Neither agree nor disagree" ~ 3, 
    x == "Slightly agree" ~ 4, 
    x == "Strongly agree" ~ 5
  )
}


# Set-up ------------------------------------------------------------------

# Load data 
Data_path <- here::here("Raw_data_vaccines") 
data_og <- readRDS(Data_path)

# Variable details 
data_og %>% view_df(show.type = TRUE, file = "SEM_variable_details")



# Data tidying and coding -------------------------------------------------

# Create variable subset
all_vars <- c("C1_5_C1", "C1_6_C1", "C1_7_C1", "C1_9_C1", "C1_10_C1", "C2_1_C2", "C2_2_C2", "C2_3_C2", "C2_4_C2", "C2_5_C2", "C2_6_C2", "C2_7_C2", "C2_8_C2", "B5_1_B5", "B5_2_B5", "B5_3_B5", "B5_4_B5", "B5_5_B5", "B5_6_B5", "B5_7_B5", "B5_8_B5", "B5_9_B5", "C1_1_C1", "C1_2_C1", "C1_8_C1", "C5_1_C5", "C5_2_C5", "C5_3_C5", "C5_4_C5", "E6_3_E6", "E6_7_E6", "E6_9_E6", "E7_1_E7", "E7_4_E7", "E7_5_E7", "E7_6_E7", "E7_7_E7", "E7_8_E7", "E7_9_E7", "E7_10_E7", "E8_1_E8", "E8_2_E8", "E8_3_E8", "E8_4_E8", "E9_1_E9", "E9_3_E9", "E9_4_E9", "E9_5_E9", "E14_3_E14", "E14_4_E14", "E14_6_E14", "E14_7_E14", "E14_8_E14", "Respondent_Serial", "SS1", "SS3", "S4", "S5a", "S6", "SS6b", "S7b", "SS11", "S13", "S14")
scale_vars <-c("C1_5_C1", "C1_6_C1", "C1_7_C1", "C1_9_C1", "C1_10_C1", "C2_1_C2", "C2_2_C2", "C2_3_C2", "C2_4_C2", "C2_5_C2", "C2_6_C2", "C2_7_C2", "C2_8_C2", "B5_1_B5", "B5_2_B5", "B5_3_B5", "B5_4_B5", "B5_5_B5", "B5_6_B5", "B5_7_B5", "B5_8_B5", "B5_9_B5", "C1_1_C1", "C1_2_C1", "C1_8_C1", "C5_1_C5", "C5_2_C5", "C5_3_C5", "C5_4_C5", "E6_3_E6", "E6_7_E6", "E6_9_E6", "E7_1_E7", "E7_4_E7", "E7_5_E7", "E7_6_E7", "E7_7_E7", "E7_8_E7", "E7_9_E7", "E7_10_E7", "E8_1_E8", "E8_2_E8", "E8_3_E8", "E8_4_E8", "E9_1_E9", "E9_3_E9", "E9_4_E9", "E9_5_E9", "E14_3_E14", "E14_4_E14", "E14_6_E14", "E14_7_E14", "E14_8_E14")


## Restrict to needed variables
data_an <- data_og %>% 
  select(all_vars)


## Code variables
data_an$SS1 <-
  factor(data_an$SS1,
         levels = c(1, 2, 3),
         labels = c("Nigeria", "Uganda", "Guinea"))

data_an$S4 <-
  factor(data_an$S4,
         levels = c(1, 2, 3),
         labels = c("Urban", "Peri-urban", "Rural"))

data_an$S4 <-
  fct_collapse(data_an$S4,
               "Urban" = c("Urban", "Peri-urban"))

data_an$S5a <-
  factor(data_an$S5a,
         levels = c(1:9),
         labels = c("Less than 18", "18-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-56", "65+"))

data_an$S5a <- droplevels(data_an$S5a)

data_an$S7b <-
  factor(data_an$S7b,
         levels = c(1:12),
         labels = c("Biological mother", "Stepmother", "Aunt", "Grandmother", "Biological father", "Stepfather", "Uncle", "Grandfather", "Biological father", "Stepfather", "Other", "Other"))
data_an$S7b <- droplevels(data_an$S7b)


data_an$SS11 <-
  factor(
    data_an$SS11,
    levels = c(1, 2, 3),
    labels = c("Not vaccinated", "Partially vaccinated", "Fully vaccinated"))


## Fix education coding 
data_NGUG <- data_an %>% 
  filter(SS1 %in% c("Nigeria", "Uganda"))  
data_NGUG$S13 <- factor(data_NGUG$S13,
         levels = c(1:10),
         labels = c("No formal education", "Some primary", "Primary completed", "Some secondary", "Secondary completed", "Technical College", "Bachelors degree", "Masters degree", "PhD", "Prefer not to answer"))
data_GU <- data_an %>% 
  filter(SS1 == "Guinea")
data_GU$S13 <- factor(data_GU$S13,
                        levels = c(1:11),
                        labels = c("No formal education", "Islamic education", "Some primary", "Primary completed", "Some secondary", "Secondary completed", "Technical College", "Bachelors degree", "Masters degree", "PhD", "Prefer not to answer"))
data_an <- plyr::rbind.fill(data_NGUG, data_GU)

rm(data_NGUG)
rm(data_GU)

## Create education groupings
data_an <- data_an %>%
  mutate(education = S13)

data_an$education <- 
  fct_collapse(data_an$education, 
               "No formal education" = c("No formal education", "Islamic education"),
               "Primary" = c("Some primary", "Primary completed"), 
               "Secondary" = c("Some secondary", "Secondary completed"), 
               "Higher education" = c("Technical College", "Bachelors degree", "Masters degree", "PhD")
               )


## Fix income coding 
data_NG <- data_an %>% 
  filter(SS1 == "Nigeria")

data_NG$S14 <- factor(data_NG$S14, 
                      levels = c(1:8, 16), 
                      labels = c("Below 9,999N", "10,000-50,000N", "50,001-200,000N", "200,001-500,000N", "500,001-800,000N", "800,001-900,000N", "900,001-1m N", "1mil+ N", "Prefer not to say"))

data_UG <- data_an %>% 
  filter(SS1 == "Uganda")

data_UG$S14 <- factor(data_UG$S14, 
                      levels = c(5:12, 16), 
                      labels = c("Below 500,000 UGX", "501,000- 999,999 UGX", "1,000,000 - 1,499,999 UGX", "1,500,000-1,999,999 UGX", "Below 2,000,000 UGX", "2,000,000-3,000,000 UGX", "3,000,001-4,000,000 UX", "4,000,001-5,000,000 UGX", "Prefer not to say"))

data_GU <- data_an %>% 
  filter(SS1 == "Guinea")

data_GU$S14 <- factor(data_GU$S14, 
                      levels = c(9:13, 16), 
                      labels = c("Below 250,000 FG", "250,001-1,983,626 FG", "1,983,627-2,000,000 FG", "3,000,001-4,999,999 FG", "5,000,000 + FG", "Prefer not to say"))

data_an <- plyr::rbind.fill(data_NG, data_UG, data_GU)
rm(data_NG)
rm(data_UG)
rm(data_GU)


## Create income bands
data_an <- data_an %>% 
  mutate(income_band = S14)

data_an$income_band <- 
  fct_collapse(data_an$income_band, 
                "Low" = c("Below 9,999N", "10,000-50,000N", "Below 500,000 UGX", "Below 250,000 FG", "250,001-1,983,626 FG"), 
               "Middle" = c("50,001-200,000N", "200,001-500,000N", "500,001-800,000N", "501,000- 999,999 UGX", "1,000,000 - 1,499,999 UGX", "1,500,000-1,999,999 UGX", "Below 2,000,000 UGX", "1,983,627-2,000,000 FG", "3,000,001-4,999,999 FG"), 
               "High" = c("800,001-900,000N", "900,001-1m N", "1mil+ N", "2,000,000-3,000,000 UGX", "3,000,001-4,000,000 UX", "4,000,001-5,000,000 UGX", "5,000,000 + FG")
               )


data_an$S4 <-
  fct_collapse(data_an$S4,
               "Urban" = c("Urban", "Peri-urban"))


## Code scales
data_an <- data_an %>%
  convert(num(all_of(scale_vars)))

data_an[scale_vars] <-  lapply(data_an[scale_vars], AgreementScale)

# Save the file
saveRDS(data_an, file = "Analysis_data")










