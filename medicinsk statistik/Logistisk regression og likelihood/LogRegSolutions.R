#Installing the packages if needed
pkgs <- c("readxl","ggplot2","tidyr","dplyr", "broom")
options(repos = c(CRAN = "https://cloud.r-project.org"))
to_get <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_get)) install.packages(to_get, dependencies = TRUE)

library(readxl)
library(dplyr)
library(ggplot2)
library(broom)


##### Exercise 1 ########

setwd("C:/Users/cdahl/OneDrive - Aalborg Universitet/Undervisning/statistics/Medicinsk Statistik/Lecture 10 Logistic regression")
# Import the dataset
data <- read_excel("depressed.xlsx")

# Create a cross-tabulation with totals
contingency_table <- addmargins(table(data$depressed, data$gender))
print(contingency_table)

# Calculate expected values
chi_square_test <- chisq.test(table(data$depressed, data$gender))
expected_values <- chi_square_test$expected
expected_table <- addmargins(expected_values)
print(expected_table)

# Perform the chi-square test
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)



######## Exercise 2 ##########

# Create the scatterplot with a loess fit
ggplot(data, aes(x = sleep, y = depressed)) +
  geom_point(alpha = 0.5) +  # Add scatter points
  geom_smooth(method = "loess", span = 0.6, color = "red") +  # Add loess fit
  labs(title = "Scatterplot of Survived vs Age with Loess Fit", x = "SleepQL", y = "depressed") +
  theme_minimal()

# Create the scatterplot with a loess fit
ggplot(data, aes(x = selfest, y = depressed)) +
  geom_point(alpha = 0.5) +  # Add scatter points
  geom_smooth(method = "loess", span = 0.6, color = "red") +  # Add loess fit
  labs(title = "Scatterplot of Survived vs Age with Loess Fit", x = "Self Esteem", y = "depressed") +
  theme_minimal()

# Create the scatterplot with a loess fit
ggplot(data, aes(x = age, y = depressed)) +
  geom_point(alpha = 0.5) +  # Add scatter points
  geom_smooth(method = "loess", span = 0.9, color = "red") +  # Add loess fit
  labs(title = "Scatterplot of Survived vs Age with Loess Fit", x = "Age", y = "depressed") +
  theme_minimal()


###### deining catagorical factors and reference catagories #########

# Convert 'depressed' and 'gender' to factors adds labels and reference catagory
data$depressed <- relevel(as.factor(data$depressed), levels = c(0, 1), labels = c("No", "Yes"), ref = "0")
data$gender    <- relevel(as.factor(data$gender),    levels = c(0, 1), labels = c("Male", "Female"), ref = "0")


# Fit a simple logistic regression model
logistic_model <- glm(depressed ~ gender, data = data, family = binomial)
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


#### Exercise 4 ####

# Fit a simple logistic regression model
logistic_model <- glm(depressed ~ sleep + gender + selfest + age, data = data, family = binomial)
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
