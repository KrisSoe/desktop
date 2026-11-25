##################################
###Exercises: Bending the rules###
##################################

###############
###Load data###
###############

SUA_data <- medisr::SUA_data

#20.1.1 A: Let’s start practicing by finding interactions from written text.
#Exercise 1A:
#
#For each of the causal relationships below, name a hypothetical third variable that would lead to an interaction effect:
# - Physical activity impacts body mass index (BMI).
# - Smoking on risk of lung cancer.
# - Antihypertensive treatment on systolic blood pressure.
# - Alcohol consumption on risk of liver disease
#
#Hint: Think of a variable that could make the relationship stronger in some patients and weaker in others

#Exercise 2A:
#
#Which of the following hypotheses contain an interaction:
#
# Definitionen på en interaktion: When variable z changes the effect of x on outcome y
#
# 1 The association between smoking and lung cancer is stronger among individuals with a family history of lung cancer.
# 2 Older individuals are more likely to have hypertension than younger individuals but also to have hypercholesterolemia.
# 3 The effect of exercise on BMI is greater in men than in women and in younger than in older individuals.
# 4 Patients with severe hypertension are more likely to receive antihypertensive medication but not antipsychotic medication.
# 5 The association between alcohol consumption and liver disease is strong among individuals with hepatitis infection but not among individuals with diabetes.
#
# 1 - nej - svar: interaction; svarede også initielt ja, men så blev jeg i tvivl og nu er jeg forvirret
# 2 - what? - svar:No interaction (more like a covariate)
# 3 - ja - svar: interaction
# 4 - nej - svar: No interaction (not dependent on meds)
# 5 - nej - svar: No interaction (one covaraite is not informative)

#Exercise 3A:
#
#For each of the explanations in 2A, write a linear model that expresses the stated relationship.
#
#det forstår jeg ikke

#20.1.2 B: You start working through the data you have been assigned to analyse:
#
#The research team asks you to investigate whether the relationship between uric acid and fasting blood glucose differs between men and women.

SUA_data <- SUA_data |>
  dplyr::mutate(
    sex = factor(sex,
                 levels = c(1, 2),
                 labels = c("male", "female")
    ),
    hyperglycemia = factor(hyperglycemia,
                           levels = c(1, 2),
                           labels = c("not hypoglycemic", "hypoglycemic")
    ),
  ) |>
  dplyr::filter(
    Times == 3
  )

#NOTE that we only use observation 3 (the latest follow-up all participants completed).
#Had we used all observations, we would not adhered to the iid assumption of linear regression,
#which would make overconfident variance estimates. To correct for this, we could have used
#other models that would be more appropriate for this type of data (ask an instructor if interested).
#However, for this lecture, we’re satisfied with only looking at a subset of the dataset.

#Exercise 1B:
#
#Fit the following model:
model1 <- lm(
  FBG ~ UA + sex + UA:sex,
  data = SUA_data
)
summary(model1)
#
#Call:
#  lm(formula = FBG ~ UA + sex + UA:sex, data = SUA_data)
#
#Residuals:
#  Min      1Q  Median      3Q     Max
#-2.1568 -0.5097 -0.1602  0.2881 10.7077
#
#Coefficients:
#               Estimate Std. Error t value
#  (Intercept)   5.645e+00  8.761e-02  64.435
#  UA            5.551e-05  2.394e-04   0.232
#  sexfemale    -6.042e-01  1.040e-01  -5.808
#  UA:sexfemale  1.680e-03  3.083e-04   5.449
#
#Pr(>|t|)
#  (Intercept)   < 2e-16 ***
#  UA              0.817
#  sexfemale    6.64e-09 ***
#  UA:sexfemale 5.25e-08 ***
#---
#  Signif. codes:
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 0.8797 on 6052 degrees of freedom
#Multiple R-squared:  0.01835,	Adjusted R-squared:  0.01786
#F-statistic: 37.71 on 3 and 6052 DF,  p-value: < 2.2e-16
#
# 1. What is the outcome variable?
#    FBG, fasting blood glucose, er outcoum variablen, den afhængige variabel,
#    hvorimod UA og sex er de uafhængige variabler hvortil UA_sex er
#    interaktionsleddet mellem UA og sex
#
#2. Write up the mathematical definition of the model.
#   FBG=alpha+B1UA+B2sex+B3(B1UA*B2sex)+error
#   FBG_i = β0 + β1*UA_i + β2*sex_i + β3*(UA_i*sex_i) + ε_i - det er det samme bare lidt flottere skrevet
#
#3. Which coefficient represents the interaction?
#   Coefficients:
#                  Estimate Std. Error t value
#   UA:sexfemale  1.680e-03  3.083e-04   5.449
#


library(marginaleffects)

plot_predictions(
  model1,
  condition = c("UA", "sex")
)

#Det virkede at plotte det, men find lige ud af hvorfor det er så dårlig kvalitet


