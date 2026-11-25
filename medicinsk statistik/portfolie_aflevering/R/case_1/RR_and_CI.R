
###############
### RR og CI ###
###############

###############
### Library ###
###############

library(epitools)
library(risks)


#################
### Load data ###
#################

mabp_data<- medisr::mabp
mabp_data <- tibble::tibble(mabp_data)


mabp_data$Outcome_dicho <- factor(
  mabp_data$primary_outcome_good_functional_outcome,
  labels = c("Bad outcome", "Good outcome")
)
mabp_data$thrombectomy <- factor(
  mabp_data$below70mmHg,
  labels = c("Above 70 mmHg",
             "Below 70 mmHg")
)
results <- epitab(table(mabp_data$below70mmHg,mabp_data$Outcome_dicho),
                  method = "riskratio",
                  conf.level = 0.95,
                  pvalue = "fisher.exact")
prop.table(table(mabp_data$below70mmHg,mabp_data$Outcome_dicho),margin=1)*100
results$tab
