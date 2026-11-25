
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

#################
### Load data ###
#################

mabp_data<- medisr::mabp
mabp_data <- tibble::tibble(mabp_data)


#########################
### Primary objective ###
#########################

mabp_data <- mabp_data %>%
  mutate(
    below70mmHg_cum = rowSums(70 > select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    below70mmHg = 2 < below70mmHg_cum,
    primary_outcome_good_functional_outcome = mrs_3m < 3
  )

###########################
### Secondary objective ###
###########################

mabp_data <- mabp_data %>%
  mutate(
    secondary_outcome_alive = mrs_3m < 6
  )

