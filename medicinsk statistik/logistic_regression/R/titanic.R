#Installing the packages if needed
pkgs <- c("readxl","ggplot2","tidyr","dplyr", "broom")
options(repos = c(CRAN = "https://cloud.r-project.org"))
to_get <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_get)) install.packages(to_get, dependencies = TRUE)

library(readxl)
library(dplyr)
library(ggplot2)
library(broom)

library(readxl)
titanic_data <- read_excel("raw-data/titanic-Full.xlsx")
View(titanic_Full)

############# Cross Tabulations ###########
# Create a table
table1 <- table(titanic_data$survived, titanic_data$sex)
print(table1)

#   female male
#0    127  682
#1    339  161

# Create a cross-tabulation with totals - bare for at visualisering
contingency_table <- addmargins(table(titanic_data$survived, titanic_data$sex))
print(contingency_table)

#female male  Sum
#0      127  682  809
#1      339  161  500
#Sum    466  843 1309

# Calculate expected values - på baggrund af data beregnes de forventede udfald
chi_square_test <- chisq.test(table(titanic_data$survived, titanic_data$sex))
expected_values <- chi_square_test$expected
expected_table <- addmargins(expected_values)
print(expected_table)

#female     male  Sum
#0   288.0015 520.9985  809
#1   177.9985 322.0015  500
#Sum 466.0000 843.0000 1309

# Perform the chi-square test - chi-sqaure udføres for at afgøre, om udfaldende,
#i forhold til de forventede udfald, skyldes tilfældigheder eller ikke
chi_square_test <- chisq.test(table1)
print(chi_square_test)

#Pearson's Chi-squared test with Yates' continuity correction

#data:  table1
#X-squared = 363.62, df = 1, p-value < 2.2e-16

######## Lowess scatter plot ##########
# Create the scatterplot with a loess fit
ggplot(titanic_data, aes(x = age, y = survived)) +
  geom_point(alpha = 0.5) +  # Add scatter points
  geom_smooth(method = "loess", span = 1, color = "red") +  # Add loess fit
  labs(title = "Scatterplot of Survived vs Age with Loess Fit", x = "Age", y = "Survived") +
  theme_minimal()


###### defining catagorical factors and reference catagories #########

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

#Call:
#  glm(formula = survived ~ sex, family = binomial, data = titanic_data)
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept) -1.44363    0.08762  -16.48   <2e-16 ***
#  sexfemale    2.42544    0.13602   17.83   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 1741.0  on 1308  degrees of freedom
#Residual deviance: 1368.1  on 1307  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 1372.1
#
#Number of Fisher Scoring iterations: 4

# Get the odds ratio
odds_ratio <- exp(coef(logistic_model))
print(odds_ratio)
#(Intercept)   sexfemale
#0.2360704  11.3071844

# Get the 95% confidence intervals for the odds ratio
conf_int <- exp(confint(logistic_model))
print(conf_int)
#                2.5 %     97.5 %
#(Intercept) 0.1981912  0.2794823
#sexfemale   8.6871038 14.8093675

# Combine the results into a data frame
results <- data.frame(
  Term = names(odds_ratio),
  Odds_Ratio = odds_ratio,
  `2.5 %` = conf_int[, 1],
  `97.5 %` = conf_int[, 2])
print(results)
#                   Term Odds_Ratio    X2.5..    X97.5..
#(Intercept) (Intercept)  0.2360704 0.1981912  0.2794823
#sexfemale     sexfemale 11.3071844 8.6871038 14.8093675

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
                      data = titanic_data, family = binomial) # * samler køn og klasse faktor
summary(logistic_model)

#
#Call:
#  glm(formula = survived ~ sex * pclass + sibsp1 + parch1 + age1 +
#        age2 + age3, family = binomial, data = titanic_data)
#
#Coefficients:
#                     Estimate Std. Error z value Pr(>|z|)
#(Intercept)        -1.8137     0.1644 -11.029  < 2e-16 ***
#  sexfemale           1.4092     0.2309   6.104 1.04e-09 ***
#  pclass1             1.4422     0.2635   5.474 4.41e-08 ***
#  pclass2            -0.1533     0.2820  -0.544  0.58658
#sibsp11            -0.1522     0.2107  -0.722  0.47015
#sibsp12            -1.5318     0.3777  -4.055 5.01e-05 ***
#  parch11             0.5091     0.2752   1.850  0.06430 .
#parch12             0.2253     0.3381   0.666  0.50513
#parch13            -0.3737     0.6021  -0.621  0.53486
#age1               -0.4416     0.1681  -2.627  0.00862 **
#  age2                0.1982     0.1085   1.826  0.06785 .
#age3               -0.2199     0.1772  -1.241  0.21446
#sexfemale:pclass1   2.5843     0.5490   4.707 2.51e-06 ***
#  sexfemale:pclass2   2.6461     0.4658   5.680 1.34e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 1414.62  on 1045  degrees of freedom
#Residual deviance:  906.42  on 1032  degrees of freedom
#(264 observations deleted due to missingness)
#AIC: 934.42
#
#Number of Fisher Scoring iterations: 6
#
# Get the odds ratio
odds_ratio <- exp(coef(logistic_model))
print(odds_ratio)
#      (Intercept)         sexfemale           pclass1           pclass2           sibsp11
#0.1630558         4.0924798         4.2299045         0.8578357         0.8588205
#sibsp12           parch11           parch12           parch13              age1
#0.2161377         1.6637131         1.2527340         0.6882164         0.6430006
#age2              age3 sexfemale:pclass1 sexfemale:pclass2
#1.2191735         0.8025705        13.2537931        14.0994981

# Get the 95% confidence intervals for the odds ratio
conf_int <- exp(confint(logistic_model))
print(conf_int)
#                      2.5 %     97.5 %
#(Intercept)       0.1169337  0.2230111
#sexfemale         2.6099492  6.4594051
#pclass1           2.5311692  7.1212680
#pclass2           0.4855688  1.4728059
#sibsp11           0.5655290  1.2932930
#sibsp12           0.1011177  0.4453237
#parch11           0.9686257  2.8539136
#parch12           0.6437749  2.4301379
#parch13           0.1975972  2.1395330
#age1              0.4619842  0.8927328
#age2              0.9861949  1.5105732
#age3              0.5625608  1.1227007
#sexfemale:pclass1 4.8665846 43.3061615
#sexfemale:pclass2 5.8214592 36.3994144

# Combine the results into a data frame
results <- data.frame(
  Term = names(odds_ratio),
  Odds_Ratio = odds_ratio,
  `2.5 %` = conf_int[, 1],
  `97.5 %` = conf_int[, 2])
print(results)

#                               Term Odds_Ratio    X2.5..    X97.5..
#(Intercept)             (Intercept)  0.1630558 0.1169337  0.2230111
#sexfemale                 sexfemale  4.0924798 2.6099492  6.4594051
#pclass1                     pclass1  4.2299045 2.5311692  7.1212680
#pclass2                     pclass2  0.8578357 0.4855688  1.4728059
#sibsp11                     sibsp11  0.8588205 0.5655290  1.2932930
#sibsp12                     sibsp12  0.2161377 0.1011177  0.4453237
#parch11                     parch11  1.6637131 0.9686257  2.8539136
#parch12                     parch12  1.2527340 0.6437749  2.4301379
#parch13                     parch13  0.6882164 0.1975972  2.1395330
#age1                           age1  0.6430006 0.4619842  0.8927328
#age2                           age2  1.2191735 0.9861949  1.5105732
#age3                           age3  0.8025705 0.5625608  1.1227007
#sexfemale:pclass1 sexfemale:pclass1 13.2537931 4.8665846 43.3061615
#sexfemale:pclass2 sexfemale:pclass2 14.0994981 5.8214592 36.3994144


###Titanic exercises###

#Build a logistic regression model containing sex, age, passenger class
logistic_model <- glm(survived ~ sex + age + pclass, data = titanic_data, family = binomial)
summary(logistic_model)

#Call:
#glm(formula = survived ~ sex + age + pclass, family = binomial,
#    data = titanic_data)
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept) -1.265431   0.202868  -6.238 4.44e-10 ***
#  sexfemale    2.497845   0.166037  15.044  < 2e-16 ***
#  age         -0.034393   0.006331  -5.433 5.56e-08 ***
#  pclass1      2.289661   0.225802  10.140  < 2e-16 ***
#  pclass2      1.009091   0.198363   5.087 3.64e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 1414.62  on 1045  degrees of freedom
#Residual deviance:  982.45  on 1041  degrees of freedom
#(264 observations deleted due to missingness)
#AIC: 992.45
#
#Number of Fisher Scoring iterations: 4

#Calculate the probability that Jack survives


