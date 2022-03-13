# Vaccines_Structural_Equation_Modelling 
# Code for the article 'A Structural Equation Modelling Approach to Understanding the Determinants of Childhood Vaccination in Nigeria, Uganda and Guinea'
# This script reproduces the analysis in the main article  
# March 2022

# Packages ------------------------------------
library(tidyverse)
library(here)
library(hablar)
library(table1)
library(lavaan)
library(semPlot)
library(collapse)
library(semTable)
library(sjPlot)
library(psych)
library(parameters)



# Functions ---------------------------------------------------------------

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

# Load data (using output from Data Preparation script)
Data_path <- here::here("Analysis_data") 
data_an <- readRDS(Data_path)

# Variable subsets
scale_vars <-c("C1_5_C1", "C1_6_C1", "C1_7_C1", "C1_9_C1", "C1_10_C1", "C2_1_C2", "C2_2_C2", "C2_3_C2", "C2_4_C2", "C2_5_C2", "C2_6_C2", "C2_7_C2", "C2_8_C2", "B5_1_B5", "B5_2_B5", "B5_3_B5", "B5_4_B5", "B5_5_B5", "B5_6_B5", "B5_7_B5", "B5_8_B5", "B5_9_B5", "C1_1_C1", "C1_2_C1", "C1_8_C1", "C5_1_C5", "C5_2_C5", "C5_3_C5", "C5_4_C5", "E6_3_E6", "E6_7_E6", "E6_9_E6", "E7_1_E7", "E7_4_E7", "E7_5_E7", "E7_6_E7", "E7_7_E7", "E7_8_E7", "E7_9_E7", "E7_10_E7", "E8_1_E8", "E8_2_E8", "E8_3_E8", "E8_4_E8", "E9_1_E9", "E9_3_E9", "E9_4_E9", "E9_5_E9", "E14_3_E14", "E14_4_E14", "E14_6_E14", "E14_7_E14", "E14_8_E14")
community_vars <-c("C1_5_C1", "C1_6_C1", "C1_7_C1", "C1_9_C1", "C1_10_C1", "C2_1_C2", "C2_2_C2", "C2_3_C2", "C2_4_C2", "C2_5_C2", "C2_6_C2", "C2_7_C2", "C2_8_C2")
family_vars <- c("B5_1_B5", "B5_2_B5", "B5_3_B5", "B5_4_B5", "B5_5_B5", "B5_6_B5", "B5_7_B5", "B5_8_B5", "B5_9_B5", "C1_1_C1", "C1_2_C1", "C1_8_C1", "C5_1_C5", "C5_2_C5", "C5_3_C5", "C5_4_C5")
vaccine_vars <- c("E6_3_E6", "E6_7_E6", "E6_9_E6", "E7_1_E7", "E7_4_E7", "E7_5_E7", "E7_6_E7", "E7_7_E7", "E7_8_E7", "E7_9_E7", "E7_10_E7", "E8_1_E8", "E8_2_E8", "E8_3_E8", "E8_4_E8", "E9_1_E9", "E9_3_E9", "E9_4_E9", "E9_5_E9", "E14_3_E14", "E14_4_E14", "E14_6_E14", "E14_7_E14", "E14_8_E14")



# Table 1 -----------------------------------------------------------------


## Table 1
### Apply labels 
label(data_an$SS1) <- "Country"
label(data_an$S4) <- "Setting"
label(data_an$S7b) <- "Relationship to child"
label(data_an$SS11) <- "Child's vaccination status"
label(data_an$education) <- "Education"
label(data_an$S5a) <- "Age"
label(data_an$income_band) <- "Income level"
label(data_an$S6) <- "Number of children"

table1(~ S4 + S5a + education + income_band + S6 + S7b + SS11 | SS1, data = data_an, overall = "Total")


# Factor analysis  ---------------------------------------------------------------------
# Recode DK/Refused to missing 

data_an[scale_vars] <-
  lapply(data_an[scale_vars], DKrecode)

# Convert to numeric 
data_an[scale_vars] <- lapply(data_an[scale_vars], numeric_code)

# Start with community variables to do EFA
data_comm <- data_an %>% 
  select(community_vars)

# Correlation matrix 
comm_EFA_cor <- cor(data_comm, use = "pairwise.complete.obs")

# Scree plot 
psych::scree(comm_EFA_cor, factors = FALSE)

# Try 2 factors 
comm_EFA_2 <- psych::fa(data_comm, nfactors = 2)
comm_EFA_2$loadings

# Try 3 factors 
comm_EFA_3 <- psych::fa(data_comm, nfactors = 3)
comm_EFA_3$loadings

# Try 4 factors 
comm_EFA_4 <- psych::fa(data_comm, nfactors = 4)
comm_EFA_4$loadings

# Try 5 factors 
comm_EFA_5 <- psych::fa(data_comm, nfactors = 5)
comm_EFA_5$loadings

# Try 6 factors 
comm_EFA_6 <- psych::fa(data_comm, nfactors = 6)
comm_EFA_6$loadings

# Possible factor: 
# Community belonging: C2_1_C2 + C2_2_C2 + C2_3_C2


# Family EFA
#Select family variables
data_fam <- data_an %>% 
  select(all_of(family_vars))

# Correlation matrix 
fam_EFA_cor <- cor(data_fam, use = "pairwise.complete.obs")

# Scree plot 
scree(fam_EFA_cor, factors = FALSE)

# Try 5 factors 
fam_EFA_5 <- fa(data_fam, nfactors = 5)
fam_EFA_5$loadings

# Try 6 factors 
fam_EFA_6 <- fa(data_fam, nfactors = 6)
fam_EFA_6$loadings

# Possible factors here: 
# Belief in religious protection: C5_1_C5 + C5_2_C5 + C5_3_C5 + C5_4_C5
# Influence from outside sources: B5_4_B5 + B5_5_B5 + B5_7_B5
# Male control: B5_1_B5 + B5_9_B5 + B5_6_B5


# Vaccine EFA
#Select vaccine variables
data_vacc <- data_an %>% 
  select(all_of(vaccine_vars))

# Correlation matrix 
vacc_EFA_cor <- cor(data_vacc, use = "pairwise.complete.obs")

# Scree plot 
scree(vacc_EFA_cor, factors = FALSE)

# Try 6 factors 
vacc_EFA_6 <- fa(data_vacc, nfactors = 6)
vacc_EFA_6$loadings

# Try 5 factors 
vacc_EFA_5 <- fa(data_vacc, nfactors = 5)
vacc_EFA_5$loadings

# Try 4 factors 
vacc_EFA_4 <- fa(data_vacc, nfactors = 4)
vacc_EFA_4$loadings

# Possible factors: 
# Support for vaccination from authority figures: E14_3_E14 + E14_4_E14 + E14_6_E14 + E14_7_E14 + E14_8_E14
# Other things are more important and children will be fine anyway: E7_6_E7 + E7_10_E7 + E9_1_E9 + E9_4_E9 + E9_5_E9
# Bad service delivery experience: E8_2_E8 + E8_3_E8 _ E8_4_E8 
# Visceral reaction: E6_3_E6 + E6_7_E6 + E6_9_E6

## All possible factors: 
# X Community belonging: C2_1_C2 + C2_2_C2 + C2_3_C2
# A: Belief in religious protection: C5_1_C5 + C5_2_C5 + C5_3_C5 + C5_4_C5
# Y Openness to influence from outside sources: B5_4_B5 + B5_5_B5 + B5_7_B5
# B: Male control: B5_1_B5 + B5_9_B5 + B5_6_B5
# C: Perceived support for vaccination from authority figures: E14_3_E14 + E14_4_E14 + E14_6_E14 + E14_7_E14 + E14_8_E14
# D: Other things are more important and children will be fine anyway: E7_6_E7 + E7_10_E7 + E9_1_E9 + E9_4_E9 + E9_5_E9
# E: Bad service delivery experience: E8_2_E8 + E8_3_E8 _ E8_4_E8 
# F: Visceral reaction: E6_3_E6 + E6_7_E6 + E6_9_E6


# SEM (full sample) ---------------------------------------------------------------------
# Model 0- just the factors 
model0 <- 
  'A =~ C5_1_C5 + C5_2_C5 + C5_3_C5 + C5_4_C5
   B =~ B5_1_B5 + B5_9_B5 + B5_6_B5 + B5_3_B5
   C =~ E14_3_E14 + E14_4_E14 + E14_6_E14 + E14_7_E14 + E14_8_E14
   D =~ E7_6_E7 + E7_10_E7 + E9_1_E9 + E9_4_E9 + E9_5_E9
   E =~ E8_2_E8 + E8_3_E8 + E8_4_E8
   F =~ E6_3_E6 + E6_7_E6 + E6_9_E6'


model0.fit <- cfa(model = model0, 
                  data = data_an)

summary(model0.fit, standardized = TRUE, fit.measures = T)

modificationindices(model0.fit, sort = TRUE)

# Model 0.1- with covariances added
model0.1 <- 
  'A =~ C5_1_C5 + C5_2_C5 + C5_3_C5 + C5_4_C5
   B =~ B5_1_B5 + B5_9_B5 + B5_6_B5 + B5_3_B5
   C =~ E14_3_E14 + E14_4_E14 + E14_6_E14 + E14_7_E14 + E14_8_E14
   D =~ E7_6_E7 + E7_10_E7 + E9_1_E9 + E9_4_E9 + E9_5_E9
   E =~ E8_2_E8 + E8_3_E8 + E8_4_E8
   F =~ E6_3_E6 + E6_7_E6 + E6_9_E6
   E14_3_E14 ~~ E14_4_E14
   E7_6_E7 ~~ E7_10_E7'


model0.1.fit <- cfa(model = model0.1, 
                  data = data_an)

summary(model0.1.fit, standardized = TRUE, fit.measures = T)

# Model 0.2- remove low fit
model0.2 <- 
  'A =~ C5_1_C5 + C5_2_C5 + C5_3_C5 + C5_4_C5
   B =~ B5_1_B5 + B5_9_B5 + B5_3_B5
   C =~ E14_3_E14 + E14_4_E14 + E14_6_E14 + E14_7_E14 + E14_8_E14
   D =~ E7_6_E7 + E7_10_E7 + E9_1_E9 + E9_4_E9 + E9_5_E9
   E =~ E8_2_E8 + E8_3_E8 + E8_4_E8
   F =~ E6_3_E6 + E6_7_E6 + E6_9_E6
   E14_3_E14 ~~ E14_4_E14
   E7_6_E7 ~~ E7_10_E7'


model0.2.fit <- cfa(model = model0.2, 
                    data = data_an)

summary(model0.2.fit, standardized = TRUE, fit.measures = T)

# Use 0.1 going forward as removal in 0.2 does not change fit metrics



# Create the outcome variable 
data_an <- data_an %>% mutate(outcome = SS11)
data_an$outcome <- fct_collapse(data_an$outcome,
                                 "Undervac" =  c("Not vaccinated", "Partially vaccinated"))

# Model 1
model1 <- 
  'A =~ C5_1_C5 + C5_2_C5 + C5_3_C5 + C5_4_C5
   B =~ B5_1_B5 + B5_9_B5 + B5_6_B5 + B5_3_B5
   C =~ E14_3_E14 + E14_4_E14 + E14_6_E14 + E14_7_E14 + E14_8_E14
   D =~ E7_6_E7 + E7_10_E7 + E9_1_E9 + E9_4_E9 + E9_5_E9
   E =~ E8_2_E8 + E8_3_E8 + E8_4_E8
   F =~ E6_3_E6 + E6_7_E6 + E6_9_E6
   outcome ~ A + B + C + D + E + F
 E14_3_E14 ~~ E14_4_E14
   E7_6_E7 ~~ E7_10_E7'


model1.fit <- cfa(model = model1, 
                  data = data_an, 
                  ordered = "outcome", 
                  link = "probit")

summary(model1.fit, standardized = TRUE, fit.measures = T)

modificationindices(model1.fit, sort = TRUE)

# 95% CI
model_parameters(model1.fit, ci = 0.95, standardize = FALSE, component = c("regression"))
model_parameters(model1.fit, ci = 0.95, standardize = TRUE, component = c("regression"))



# Country models ----------------------------------------------------------


# Nigeria 
data_NG <- data_an %>% 
  filter(SS1 == "Nigeria")

modelNG.fit <- cfa(model = model1, 
                  data = data_NG, 
                  ordered = "outcome", 
                  link = "probit")

summary(modelNG.fit, standardized = TRUE, fit.measures = T)




# Uganda
data_UG <- data_an %>% 
  filter(SS1 == "Uganda")

modelUG.fit <- cfa(model = model1, 
                   data = data_UG, 
                   ordered = "outcome", 
                   link = "probit")

summary(modelUG.fit, standardized = TRUE, fit.measures = T)




# Guinea
data_GU <- data_an %>% 
  filter(SS1 == "Guinea")

modelGU.fit <- cfa(model = model1, 
                   data = data_GU, 
                   ordered = "outcome", 
                   link = "probit")

summary(modelGU.fit, standardized = TRUE, fit.measures = T)











