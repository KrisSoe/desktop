#############
### Case 2###
#############


###############
###Load data###
###############

data_train <- medisr::eq5d_train
data_train <- tibble::tibble(data_train)
data_test <- medisr::eq5d_test
data_test <- tibble::tibble(data_test)

# Data documentation

?medisr::eq5d

data_train
data_test

###############
### Library ###
###############
library(tidyverse)
library(survival)
library(ggplot2)
library(ggsci)
library(blandr)

###################
###Data cleaning###
###################

#Making the sex variable categorical

data_train <- data_train %>%
  mutate(
    #sex: 0 = Female, 1 = Male
    sex_chr=ifelse(sex==1, yes = "Male", no = "Female")
  )


data_test <- data_test %>%
  mutate(
    #sex: 0 = Female, 1 = Male
    sex_chr=ifelse(sex==1, yes = "Male", no = "Female")
  )


#################
### Table one ###
#################
#Table one
library(tableone)
?CreateTableOne()
#Main function, create a table 1 or summary table
CreateTableOne(data = data_train)

                  Overall
n                     607
X (mean (SD))      304.00 (175.37)
eq5d (mean (SD))     7.61 (3.05)
AL (mean (SD))      17.44 (3.70)
EC (mean (SD))      44.38 (8.99)
IC (mean (SD))      10.07 (1.90)
age (mean (SD))     66.01 (12.77)
sex (mean (SD))      0.56 (0.50)
sex_chr = Male (%)    339 (55.8)

#RMSE: Hvor store er modellens gennemsnitlige forudsigelsesfejl?
#MAE: Hvor stor er den gennemsnitlige absolutte fejl?
#R²: Hvor meget variation kan modellen forklare?

glimpse(data_train) #giver et kort overblik over datesættet i konsollen

colSums(is.na(data_train)) #tæller manglende observationer

summary(data_train$eq5d)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.
#5.000   6.000   6.000   7.614   9.000  21.000

hist(data_train$eq5d) #laver et hurtigt histogram, her viser den fordelingen af eq5d

summary(data_train)

# Min.	  Den mindste observerede værdi
# 1st Qu.	25-percentilen
# Median	50-percentilen
# Mean	  Gennemsnittet
# 3rd Qu.	75-percentilen
# Max.	  Den største observerede værdi

eq5d
Min.   :5.
1st Qu.:6.
Median :6.
Mean   :7.614
3rd Qu.:9.
Max.   :21.

#Det betyder, at halvdelen af patienterne har en EQ-5D-score på 6 eller derunder, mens gennemsnittet er 7,614.
#At gennemsnittet er højere end medianen tyder på en højreskæv fordeling.


#Hurtig undersøgelse af lineære sammenhænge
plot(data_train$AL, data_train$eq5d)
plot(data_train$EC, data_train$eq5d)
plot(data_train$IC, data_train$eq5d)

#forstår dem ikke helt, synes ikke der ser ud til at være lineær sammenhæng

cor(data_train$eq5d, data_train$AL)
cor(data_train$eq5d, data_train$EC)
cor(data_train$eq5d, data_train$IC)

#> cor(data_train$eq5d, data_train$AL)
[1] -0.6470316
#> cor(data_train$eq5d, data_train$EC)
[1] -0.593234
#> cor(data_train$eq5d, data_train$IC)
[1] -0.4914982

#r tæt på -1: Stærk negativ sammenhæng.
#r tæt på 0: Ingen eller svag lineær sammenhæng.
#r tæt på 1: Stærk positiv sammenhæng.


##############################
###Initial regression model###
##############################

init_model <- lm(eq5d ~ age + sex + AL + EC + IC,
                 data = data_train)
summary(init_model)

#Call:
#  lm(formula = eq5d ~ age + sex + AL + EC + IC, data = data_train)

#Residuals:
#  Min      1Q  Median      3Q     Max
#-6.3119 -1.1637 -0.2738  0.8046 12.8520

#Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
#(Intercept) 20.028595   0.691176  28.978  < 2e-16 ***
#age         -0.005304   0.007432  -0.714   0.4757
#sex          0.155736   0.178766   0.871   0.3840
#AL          -0.362914   0.033394 -10.868  < 2e-16 ***
#EC          -0.105220   0.012683  -8.296 7.08e-16 ***
#IC          -0.114314   0.060886  -1.878   0.0609 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 2.16 on 601 degrees of freedom
#Multiple R-squared:  0.5023,	Adjusted R-squared:  0.4981
#F-statistic: 121.3 on 5 and 601 DF,  p-value: < 2.2e-16

#Modellen viser:

#AL og EC har statistisk signifikante negative sammenhænge med EQ-5D.
#Mere konkret:
# -> AL (Activity Limitations): Jo flere begrænsninger patienten oplever i daglige aktiviteter, desto lavere er patientens EQ-5D-score forventeligt.
# -> EC (Emotional Complaints): Jo flere følelsesmæssige gener patienten oplever, desto lavere er patientens EQ-5D-score forventeligt.
#IC er tæt på, men opnår ikke statistisk signifikans.
#Alder og køn er ikke statistisk signifikante.
#Modellen forklarer cirka 50 % af variationen i EQ-5D.


car::vif(init_model) #Undersøger kolineraitet
age      sex       AL       EC       IC
1.169382 1.024843 1.976843 1.687613 1.740101

#VIF omkring 1: Ingen nævneværdig multikollinearitet.
#VIF over 5: Mulig multikollinearitet.
#VIF over 10: Potentielt alvorlig multikollinearitet.

#Alle VIF-værdier ligger under 2.

#Det betyder: Der er ikke tegn på problematisk multikollinearitet.

###Stepwise linear regression for prediction###
#data_train
data <- data_train
model_train <- lm(eq5d ~ age + sex + AL + EC + IC,
               data = data_train)
step_model_train <- step(model_train, direction = "both") #Det betyder, at R forsøger at finde en bedre model ved både at: Fjerne predictors, som ikke bidrager til modellen | Tilføje predictors igen, hvis det forbedrer modellen.
summary(step_model_train)

#Call:
  lm(formula = eq5d ~ AL + EC + IC, data = data_train) #Din nye model er:  eq5d ~ AL + EC + IC | Det betyder, at stepwise-metoden har fjernet: age, sex

Residuals:
  Min      1Q  Median      3Q     Max
-6.3138 -1.2012 -0.2288  0.7975 12.7975

Coefficients:
              Estimate Std. Error t value  Pr(>|t|)
(Intercept)   19.74040    0.53215  37.095   <2e-16 ***
  AL          -0.35308    0.03136 -11.260   <2e-16 ***
  EC          -0.10744    0.01229  -8.740   <2e-16 ***
  IC          -0.11906    0.05963  -1.996   0.0463 *
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.159 on 603 degrees of freedom
Multiple R-squared:  0.5012,	Adjusted R-squared:  0.4987
F-statistic: 201.9 on 3 and 603 DF,  p-value: < 2.2e-16

#modellen med AL, EC og IC gav en bedre balance mellem modeltilpasning og kompleksitet ifølge AIC.
#IC har en p-værdi på 0,0463 i den nye model.
#Det betyder, at IC nu er statistisk signifikant på 5 %-niveauet.
#Det er interessant, fordi IC ikke var signifikant i den oprindelige model.
#Det kan skyldes, at sammenhængen mellem IC og EQ-5D ændrer sig, når vi ikke længere justerer for alder og køn.

#Sammenligning af modellerne
#Mål             	        Init model    	Stepwise model
#Predictors	              5 	            3
#R²	                      0,5023	        0,5012
#Adjusted R²	            0,4981          0,4987
#Residual standard error	2,160	          2,159

#Det interessante er:

#R² falder næsten ikke.
#Adjusted R² stiger en smule.
#Residual standard error falder en smule.
#Modellen indeholder nu færre predictors.

#Det betyder, at man har opnået næsten samme forklaringsgrad med en simplere model.
#Det er umiddelbart et fornuftigt resultat.

# 5-Fold Cross-Validation
library(caret)
set.seed(2026)
ctrl <- trainControl(method = "cv", number = 5)
model5fold_train <- train(formula(step_model_train), data = data, method = "lm", trControl = ctrl)
print(model5fold_train)

#Linear Regression

607 samples
3 predictor

No pre-processing
Resampling: Cross-Validated (5 fold)
Summary of sample sizes: 486, 485, 486, 485, 486
Resampling results:

  RMSE     Rsquared   MAE
2.15031  0.5068459  1.506135

Tuning parameter 'intercept' was held constant at a value of TRUE

###Forudsigelser med modellen på test data###
predictions <- predict(step_model_train, newdata = data_test)

results <- data.frame(
  Actual = data_test$eq5d,
  Predicted = predictions
)

#Beregn modellens prædiktionsevne
#RMSE
sqrt(mean((results$Actual - results$Predicted)^2))
[1] 2.261783 #rmse test data

#MAE:
mean(abs(results$Actual - results$Predicted))
[1] 1.444674 #mae test data

#MSE
mean((results$Actual - results$Predicted)^2)
[1] 5.115664 #MSE test data

#R^2
cor(results$Actual, results$Predicted)^2
[1] 0.5025772 #R^2 test data

#RMSE: Hvor store modellens fejl er, hvor store fejl vægtes ekstra.
#MAE: Hvor stor den gennemsnitlige absolutte fejl er.
#R²: Hvor stærkt de faktiske og forudsagte værdier hænger lineært sammen.

ggplot(results, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1, color = "red") +
  labs(
    title = "Predicted vs. Actual EQ-5D",
    x = "Actual EQ-5D",
    y = "Predicted EQ-5D"
  ) +
  theme_minimal()

#X-aksen viser patientens faktiske EQ-5D-score.
#Y-aksen viser modellens forudsagte EQ-5D-score.
#Hvert punkt repræsenterer én patient.

#Det interessante er, at patienter med samme faktiske EQ-5D-score kan have forskellige forudsagte værdier.

#Eksempelvis kan patienter med en faktisk EQ-5D på 10 have forudsagte værdier omkring både 8, 10 og 12.
#Hvorfor?
#Fordi patienterne har forskellige AL-, EC- og IC-værdier.
#To patienter kan altså have samme faktiske EQ-5D-score, men forskellige besvarelser af PEmb-QoL.
#Modellen bruger disse besvarelser til at beregne deres individuelle forudsigelser.

#Den røde linje repræsenterer perfekt prædiktion.

#Hvis et punkt ligger:

# På linjen: Modellen rammer den faktiske EQ-5D-score præcist.
# Over linjen: Modellen overvurderer EQ-5D.
# Under linjen: Modellen undervurderer EQ-5D.

#Jo tættere punkterne ligger på linjen, desto bedre prædiktion.

#Beregn forskellen mellem faktisk og forudsagt EQ-5D
results$error <- results$Actual - results$Predicted
mean(results$error)
[1] 0.08351719

summary(results$error)
    Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
-5.32585 -1.09283 -0.13519  0.08352  0.78427 11.85813


###Stepwise linear regression for prediction### ###Det tror jeg ikke at man skal faktisk
#data_test
data <- data_test
model_2 <- lm(eq5d ~ age + sex + AL + EC + IC,
            data = data_test)
step_model_test <- step(model_2, direction = "both") #Det betyder, at R forsøger at finde en bedre model ved både at: Fjerne predictors, som ikke bidrager til modellen | Tilføje predictors igen, hvis det forbedrer modellen.
summary(step_model_test)

#Call:
  lm(formula = eq5d ~ AL + EC + IC, data = data_test)

Residuals:
  Min      1Q  Median      3Q     Max
-5.8323 -1.1582 -0.2604  0.6633 11.8006

Coefficients:
              Estimate Std. Error t value Pr(>|t|)
(Intercept)   20.68349    0.76838  26.918  < 2e-16 ***
  AL          -0.27893    0.04806  -5.804 1.66e-08 ***
  EC          -0.11677    0.01664  -7.019 1.52e-11 ***
  IC          -0.29452    0.08983  -3.279  0.00117 **
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 2.255 on 297 degrees of freedom
Multiple R-squared:  0.5109,	Adjusted R-squared:  0.5059
F-statistic: 103.4 on 3 and 297 DF,  p-value: < 2.2e-16

###Bland Altman model
results$mean <- (results$Actual + results$Predicted)/2
results$difference <- results$Actual - results$Predicted
bias <- mean(results$difference)
sd_diff <- sd(results$difference)
upper_loa <- bias + 1.96 * sd_diff
lower_loa <- bias - 1.96 * sd_diff

ggplot(results, aes(x = mean, y = difference)) +
  geom_point() +
  geom_hline(yintercept = bias, color = "blue") +
  geom_hline(yintercept = upper_loa,
             color = "red", linetype = "dashed") +
  geom_hline(yintercept = lower_loa,
             color = "red", linetype = "dashed") +
  labs(
    title = "Bland-Altman plot",
    x = "Mean of actual and predicted EQ-5D",
    y = "Difference between actual and predicted EQ-5D"
  ) +
  theme_minimal()

###Bland Altman plot i andet format

predictions <- predict(step_model_train, newdata = data_test)
sqrt(mean((data_test$eq5d - predictions)^2))
[1] 2.261783 #rmse test data, havde du også allerede beregnet tidligere

plot(predictions, data_test$eq5d)
abline(a = 0, b = 1)

stats <- blandr.statistics(data_test$eq5d, predictions)
blandr.plot.ggplot(stats)

#low bias, low precision
