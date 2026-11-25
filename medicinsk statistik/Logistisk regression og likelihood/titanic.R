#Installing the packages if needed
pkgs <- c("readxl","ggplot2","tidyr","dplyr", "broom")
options(repos = c(CRAN = "https://cloud.r-project.org"))
to_get <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_get)) install.packages(to_get, dependencies = TRUE)

library(readxl)
library(dplyr)
library(ggplot2)
library(broom)

setwd("C:/Users/cdahl/OneDrive - Aalborg Universitet/Undervisning/statistics/Medicinsk Statistik/Lecture 10 Logistic regression")
# Import the dataset
titanic_data <- read_excel("titanic-Full.xlsx")

############# Cross Tabulations ###########
# Create a cross-tabulation with totals
contingency_table <- addmargins(table(titanic_data$survived, titanic_data$sex))
print(contingency_table)

# Calculate expected values
chi_square_test <- chisq.test(table(titanic_data$survived, titanic_data$sex))
expected_values <- chi_square_test$expected
expected_table <- addmargins(expected_values)
print(expected_table)

# Perform the chi-square test
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)


######## Lowess scatter plot ##########
# Create the scatterplot with a loess fit
ggplot(titanic_data, aes(x = age, y = survived)) +
  geom_point(alpha = 0.5) +  # Add scatter points
  geom_smooth(method = "loess", span = 0.8, color = "red") +  # Add loess fit
  labs(title = "Scatterplot of Survived vs Age with Loess Fit", x = "Age", y = "Survived") +
  theme_minimal()


###### deining catagorical factors and reference catagories #########

# Convert 'survived' and 'sex' to factors if they are not already
titanic_data$survived <- as.factor(titanic_data$survived)
titanic_data$sex <- relevel(as.factor(titanic_data$sex), ref = "male")
titanic_data$pclass <- relevel(as.factor(titanic_data$pclass), ref = "3")
titanic_data$sibsp <- relevel(as.factor(titanic_data$sibsp), ref = "0")
titanic_data$parch <- relevel(as.factor(titanic_data$parch), ref = "0")
titanic_data$sibsp1 <- relevel(as.factor(titanic_data$sibsp1), ref = "0")
titanic_data$parch1 <- relevel(as.factor(titanic_data$parch1), ref = "0")

# Fit a simple logistic regression model
logistic_model <- glm(survived ~ sex, data = titanic_data, family = binomial)
summary(logistic_model)

# Get the odds ratio
odds_ratio <- exp(coef(logistic_model))
# Get the 95% confidence intervals for the odds ratio
conf_int <- exp(confint(logistic_model))
# Combine the results into a data frame
results <- data.frame(
  Term = names(odds_ratio),
  Odds_Ratio = odds_ratio,
  `2.5 %` = conf_int[, 1],
  `97.5 %` = conf_int[, 2])
print(results)


# Fit a simple logistic regression model
logistic_model <- glm(survived ~ pclass, data = titanic_data, family = binomial)
summary(logistic_model)

# Get the odds ratio
odds_ratio <- exp(coef(logistic_model))
# Get the 95% confidence intervals for the odds ratio
conf_int <- exp(confint(logistic_model))
# Combine the results into a data frame
results <- data.frame(
  Term = names(odds_ratio),
  Odds_Ratio = odds_ratio,
  `2.5 %` = conf_int[, 1],
  `97.5 %` = conf_int[, 2])
print(results)

# Fit a simple logistic regression model
logistic_model <- glm(survived ~ sibsp, data = titanic_data, family = binomial)
summary(logistic_model)

# Fit a simple logistic regression model
logistic_model <- glm(survived ~ parch, data = titanic_data, family = binomial)
summary(logistic_model)

# Fit a simple logistic regression model
logistic_model <- glm(survived ~ age, data = titanic_data, family = binomial)
summary(logistic_model)
# Get the odds ratio
odds_ratio <- exp(coef(logistic_model))
# Get the 95% confidence intervals for the odds ratio
conf_int <- exp(confint(logistic_model))
# Combine the results into a data frame
results <- data.frame(
  Term = names(odds_ratio),
  Odds_Ratio = odds_ratio,
  `2.5 %` = conf_int[, 1],
  `97.5 %` = conf_int[, 2])
print(results)

# Fit a simple logistic regression model
logistic_model <- glm(survived ~ age1 + age2 + age3, data = titanic_data, family = binomial)
summary(logistic_model)
# Get the odds ratio
odds_ratio <- exp(coef(logistic_model))
# Get the 95% confidence intervals for the odds ratio
conf_int <- exp(confint(logistic_model))
# Combine the results into a data frame
results <- data.frame(
  Term = names(odds_ratio),
  Odds_Ratio = odds_ratio,
  `2.5 %` = conf_int[, 1],
  `97.5 %` = conf_int[, 2])
print(results)



##### Multifactorical model #####

# Fit the logistic regression model
logistic_model <- glm(survived ~ sex * pclass + sibsp1 + parch1 + age1 +age2 + age3, 
                      data = titanic_data, family = binomial)
summary(logistic_model)

# Get the odds ratio
odds_ratio <- exp(coef(logistic_model))
# Get the 95% confidence intervals for the odds ratio
conf_int <- exp(confint(logistic_model))
# Combine the results into a data frame
results <- data.frame(
  Term = names(odds_ratio),
  Odds_Ratio = odds_ratio,
  `2.5 %` = conf_int[, 1],
  `97.5 %` = conf_int[, 2])
print(results)
