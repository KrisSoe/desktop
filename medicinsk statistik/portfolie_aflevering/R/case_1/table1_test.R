################
### Table 1 ###
###############

###############
### Library ###
###############
library(tidyverse)
library(tableone)

#################
### Load data ###
#################

mabp_data<- medisr::mabp
mabp_data <- tibble::tibble(mabp_data)


#########################
### Primary objective ###
#########################

