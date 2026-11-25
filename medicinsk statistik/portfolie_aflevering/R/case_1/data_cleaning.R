
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
