
#####################
### Data cleaning ###
#####################

###############
### Library ###
###############
library(readr)
library(tidyverse)


#################
### Load data ###
#################

mabp_data<- medisr::mabp
mabp_data <- tibble::tibble(mabp_data)

#Omkonvertering af kategoriske variabler

#Sex, diabetes, hypertoni og NIHSS
mabp_data <- mabp_data %>%
  mutate(
    sex_chr=ifelse(sex==1,yes="Female", no="Male"),
    diabates_chr=ifelse(Diabetes==1,yes="Yes",no="No"),
    hypertension_chr=ifelse(Hyperton==1,yes="Yes",no="No"),
    nihss_chr=case_when(
      nihss_adm ==0 ~ "No stroke",
      nihss_adm >= 1 & nihss_adm <= 4 ~"Minor stroke",
      nihss_adm >= 5 & nihss_adm <= 15 ~"Moderate stroke",
      nihss_adm >= 16 & nihss_adm <= 20 ~"Moderate to severe stroke",
      nihss_adm >= 21 & nihss_adm <= 42 ~"Severe stroke",
      TRUE ~ NA_character_
    )
  )

##################
### Objectives ###
##################

###############
### Library ###
###############

library(lubridate)
library(dplyr)
library(ggplot2)
library(medisr)
library(VGAM)

#########################
### Primary objective ###
#########################

mabp_data <- mabp_data %>%
  mutate(
    below70mmHg_cum = rowSums(70 > select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    above100mmHg_cum = rowSums(100 < select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    below70mmHg = 2 < below70mmHg_cum,
    above100mmHg = 2 < above100mmHg_cum,
    outs70100mmHg = 2 < above100mmHg_cum | 2 < below70mmHg_cum,
    primary_good_functional_outcome = mrs_3m < 3
  )

###########################
### Secondary objective ###
###########################

mabp_data <- mabp_data %>%
  mutate(
    secondary_outcome_alive = mrs_3m < 6
  )

###############
### RR og CI ###
###############

###############
### Library ###
###############

library(epitools)
library(risks)

mabp_data$Outcome_dicho <- factor(
  mabp_data$primary_good_functional_outcome,
  labels = c("Bad outcome", "Good outcome")
)
mabp_data$outs70100mmHg <- factor(
  mabp_data$outs70100mmHg,
  labels = c("Non-exposed",
             "Exposed")
)
results <- epitab(table(mabp_data$outs70100mmHg,mabp_data$Outcome_dicho),
                  method = "riskratio",
                  conf.level = 0.95,
                  pvalue = "fisher.exact")
prop.table(table(mabp_data$outs70100mmHg,mabp_data$Outcome_dicho),margin=1)*100
results$tab

#Blandt patienter med færre episoder med bp udenfor normalen (<70 eller >100) < 2 (Non-exposed) havde 48,0 % et godt outcome.
#Blandt patienter med flere episoder med bp udenfor normalen (<70 eller >100) > 2 (Exposed) havde 39,3 % et godt outcome.

# RR = 0,809 : 1-0,809 = 0,19 <=> 19%
# Patienter med flere episoder med bp udenfor normalen havde cirka 19 % lavere sandsynlighed for et godt outcome sammenlignet med patienter med ≤ 2 episoder.

