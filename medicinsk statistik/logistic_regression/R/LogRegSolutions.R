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

library(readxl)
depressed_data <- read_excel("raw-data/depressed.xlsx")
View(depressed)

# Create a table
table1 <- table(depressed_data$depressed, depressed_data$gender)
print(table1)

#   0  1
#0 77 63
#1 23 37

# Create a cross-tabulation with totals
contingency_table <- addmargins(table(depressed_data$depressed, depressed_data$gender))
print(contingency_table)

#      0   1 Sum
#0    77  63 140
#1    23  37  60
#Sum 100 100 200

# Calculate expected values
chi_square_test <- chisq.test(table(depressed_data$depressed, depressed_data$gender))
expected_values <- chi_square_test$expected
expected_table <- addmargins(expected_values)
print(expected_table)

#      0   1 Sum
#0    70  70 140
#1    30  30  60
#Sum 100 100 200

# Perform the chi-square test
chi_square_test <- chisq.test(table1)
print(chi_square_test)

#Pearson's Chi-squared test with Yates' continuity correction
#
#data:  table1
#X-squared = 4.0238, df = 1, p-value = 0.04486

######## Exercise 2 ##########

# Create the scatterplot with a loess fit
ggplot(depressed_data, aes(x = sleep, y = depressed)) +
  geom_point(alpha = 0.5) +  # Add scatter points
  geom_smooth(method = "loess", span = 0.6, color = "red") +  # Add loess fit
  labs(title = "Scatterplot of Survived vs Age with Loess Fit", x = "SleepQL", y = "depressed") +
  theme_minimal()

# Create the scatterplot with a loess fit
ggplot(depressed_data, aes(x = selfest, y = depressed)) +
  geom_point(alpha = 0.5) +  # Add scatter points
  geom_smooth(method = "loess", span = 0.6, color = "red") +  # Add loess fit
  labs(title = "Scatterplot of Survived vs Age with Loess Fit", x = "Self Esteem", y = "depressed") +
  theme_minimal()

# Create the scatterplot with a loess fit
ggplot(depressed_data, aes(x = age, y = depressed)) +
  geom_point(alpha = 0.5) +  # Add scatter points
  geom_smooth(method = "loess", span = 0.9, color = "red") +  # Add loess fit
  labs(title = "Scatterplot of Survived vs Age with Loess Fit", x = "Age", y = "depressed") +
  theme_minimal()


###### defining catagorical factors and reference catagories #########

# Convert 'depressed' and 'gender' to factors adds labels and reference catagory
depressed_data$depressed <- relevel(as.factor(depressed_data$depressed), levels = c(0, 1), labels = c("No", "Yes"), ref = "0")
depressed_data$gender    <- relevel(as.factor(depressed_data$gender),    levels = c(0, 1), labels = c("Male", "Female"), ref = "0")


# Fit a simple logistic regression model
logistic_model <- glm(depressed ~ gender, data = depressed_data, family = binomial)
summary(logistic_model)

#Call:
#  glm(formula = depressed ~ gender, family = binomial, data = depressed_data)
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)  -1.2083     0.2376  -5.085 3.68e-07 ***
#  gender1       0.6761     0.3152   2.145    0.032 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 244.35  on 199  degrees of freedom
#Residual deviance: 239.65  on 198  degrees of freedom
#AIC: 243.65
#
#Number of Fisher Scoring iterations: 4

# Get the odds ratio
odds_ratio <- exp(coef(logistic_model))
print(odds_ratio)

#(Intercept)   gender1
#0.2987013   1.9661836 <- odds ratio er værdien til højre

# Get the 95% confidence intervals for the odds ratio
conf_int <- exp(confint(logistic_model))
print(conf_int)

#                2.5 %    97.5 %
#(Intercept) 0.1834184 0.4677144
#gender1     1.0665066 3.6839683 <- confidence interval

# Combine the results into a data frame
results <- data.frame(
  Term = names(odds_ratio),
  Odds_Ratio = odds_ratio,
  `2.5 %` = conf_int[, 1],
  `97.5 %` = conf_int[, 2])
print(results)

#                   Term Odds_Ratio    X2.5..   X97.5..
#(Intercept) (Intercept)  0.2987013 0.1834184 0.4677144
#gender1         gender1  1.9661836 1.0665066 3.6839683

#### Exercise 4 ####

# Fit a simple logistic regression model
logistic_model <- glm(depressed ~ sleep + gender + selfest + age, data = depressed_data, family = binomial)
summary(logistic_model) #man undersøger; Hvordan påvirker søvn, køn, selvværd og alder sandsynligheden for at være deprimeret?

#Call:
#glm(formula = depressed ~ sleep + gender + selfest + age, family = binomial,
#    data = depressed_data)
#
#Coefficients:
# Estimate: Regressionkoefficienten (β)
# Std. Error: Standardfejlen på estimatet.
# z value: forholdet mellem estimate/std. error
# Pr(<|z|): p-værdien
#             Estimate Std. Error z value Pr(>|z|)
#(Intercept)  5.07874    1.44415   3.517 0.000437 ***
#  sleep       -0.07932    0.01741  -4.555 5.23e-06 ***
#  gender       0.88061    0.36524   2.411 0.015905 *
#  selfest     -0.04662    0.01239  -3.762 0.000168 ***
#  age         -0.01296    0.02103  -0.616 0.537762
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 244.35  on 199  degrees of freedom
#Residual deviance: 194.78  on 195  degrees of freedom
#AIC: 204.78
#
#Number of Fisher Scoring iterations: 5


# Get the odds ratio
odds_ratio <- exp(coef(logistic_model))
print(odds_ratio)

#(Intercept)       sleep      gender     selfest         age
#160.5714979   0.9237469   2.4123818   0.9544483   0.9871244

# Get the 95% confidence intervals for the odds ratio
conf_int <- exp(confint(logistic_model))
print(conf_int)
#                 2.5 %       97.5 %
#(Intercept) 10.3549742 3066.3748950
#sleep        0.8903982    0.9535640
#gender       1.1944894    5.0301101
#selfest      0.9305788    0.9771207
#age          0.9460647    1.0280182

# Combine the results into a data frame
results <- data.frame(
  Term = names(odds_ratio),
  Odds_Ratio = odds_ratio,
  `2.5 %` = conf_int[, 1],
  `97.5 %` = conf_int[, 2])
print(results)

#                   Term  Odds_Ratio     X2.5..      X97.5..
#(Intercept) (Intercept) 160.5714979 10.3549742 3066.3748950
#sleep             sleep   0.9237469  0.8903982    0.9535640
#gender           gender   2.4123818  1.1944894    5.0301101
#selfest         selfest   0.9544483  0.9305788    0.9771207
#age                 age   0.9871244  0.9460647    1.0280182


