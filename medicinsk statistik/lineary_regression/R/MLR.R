#Installing the packages if needed
pkgs <- c("readxl","ppcor","haven","ggplot2","tidyr","dplyr","QuantPsyc","car","MASS","e1071")
options(repos = c(CRAN = "https://cloud.r-project.org"))
to_get <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_get)) install.packages(to_get, dependencies = TRUE)

library(readxl)
library(ppcor)
library(haven)
library(ggplot2)
library(tidyr)
library(dplyr)
library(QuantPsyc)
library(car)
library(MASS)
library(e1071)

setwd("C:/Users/KristineSøgaard/Downloads/medicinsk statistik/lineary_regression")

Example1 <- read_excel("C:/Users/KristineSøgaard/Downloads/medicinsk statistik/lineary_regression/docs/Example1.xlsx")
#View(Example1)

#Start by plotting
ggplot(Example1, aes(x = Age, y = WingLength)) +
  geom_point(size = 3, colour = "blue") +
  geom_smooth(method = "lm",
              se = TRUE,
              colour = "red") +
  labs(
    title = "Linear Regression of Wing Length on Age",
    x = "Age",
    y = "Wing Length"
  ) +
  theme_minimal()

# Fit the linear model
model <- lm(WingLength ~ Age, data = Example1)
summary(model)

# displaying the standard error of regression
summary_model <- summary(model)
standard_error <- summary_model$sigma
print(standard_error)

# Calculating the residuals
residuals <- residuals(model)

# Create a residual plot
plot(model$fitted.values, residuals,
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residual Plot")
abline(h = 0, col = "red")

# Create a histogram of the residuals
hist(residuals,
     main = "Histogram of Residuals",
     xlab = "Residuals")

# Perform the Shapiro-Wilk test for normality
shapiro_test <- shapiro.test(residuals)
print(shapiro_test)

# Create a new data frame with the Age value you want to predict
new_data <- data.frame(Age = 13)

# Predict the WingLength for Age = 13 and obtain the confidence interval
prediction_interval <- predict(model,
                               newdata = new_data,
                               interval = "confidence",
                               level = 0.95)
print(prediction_interval)




### Partial Correlation ###

# Loading the data into a data frame called df
df <- read_dta("Sleep_quality.dta")

# --- Unadjusted correlation ---
cor.test(df$mood, df$sleepqual)

# --- PARTIAL correlations ---
# mood ~ sleep (controlling for age)
 pcor.test(df$mood, df$sleepqual, df["age"], method = "pearson")

# --- SEMI-PARTIAL (PART) correlations ---
# mood ~ sleep (controlling for age)
spcor.test(df$mood, df$sleepqual, df["age"], method = "pearson")


##### Multiple Linear regression #####

df <- read_dta("example20-1.dta")

# 2x2 scatter plots using ggplot2 facets
# Pivot to long format for faceting
df_long <- df %>%
  select(X1, X2, X3, X4, X5) %>%
  pivot_longer(cols = c(X1, X2, X3, X4),
               names_to = "Predictor",
               values_to = "X")

# scatterplots of the data
ggplot(df_long, aes(x = X, y = X5)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "steelblue", linewidth = 1) +
  facet_wrap(~ Predictor, ncol = 2, scales = "free_x") +
  labs(x = NULL, y = "X5",
       title = "Scatter plots of X5 vs. X1–X4 with linear fits") +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank())

# Fit the model
model <- lm(X5 ~ X1 + X2 + X3 + X4, data = df)
summary(model)

# Compute standardized betas
beta <- lm.beta(model)
print(beta)  # Named vector of standardized coefficients


#  Diagnostic plots
par(mfrow = c(2, 2)) # setting the 2by2 space for plotting
# plot 1: residual plot, Check linearity and equal variance (homoscedasticity).
# plot 2: QQ plot: eyeball the normality of teh residuals
# plot 3: Scale-Location plot. Check homoscedasticity with more sensitivity. What you want a roughly horizontal band of points.
# plot 4: Identify influential observations that can disproportionately affect the fit.
plot(model)

# Variance inflation factor. You want this to be less than 10
vif(model)

# Raw residuals and standardized residuals
res_raw <- resid(model)                 # same as model$residuals
res_std <- rstandard(model)             # standardized  residuals
# Quick check
summary(res_raw)

# Plot a histogram of the residuals
df_res <- data.frame(res_raw = res_raw)

ggplot(df_res, aes(x = res_raw)) +
  geom_histogram(aes(y = ..density..), bins = 30, fill = "grey88", color = "white") +
  geom_density(color = "steelblue", linewidth = 1.2) +
  stat_function(fun = dnorm,
                args = list(mean = mean(df_res$res_raw, na.rm = TRUE),
                            sd   = sd(df_res$res_raw, na.rm = TRUE)),
                color = "firebrick", linewidth = 1, linetype = "dashed") +
  labs(title = "Residuals: Histogram with Normal Fit",
       x = "Residuals", y = "Density") +
  theme_minimal(base_size = 12)

# Calculation Skewness and kurtosis
skew  <- skewness(res_raw, na.rm = TRUE, type = 2)
kurt  <- kurtosis(res_raw, na.rm = TRUE, type = 2)
excess_kurt <- kurt - 3
cat(sprintf("Skewness: %.4f\n", skew))
cat(sprintf("Kurtosis (raw): %.4f\n", kurt))
cat(sprintf("Excess Kurtosis (kurtosis - 3): %.4f\n", excess_kurt))

# Test for normality.
# The null-hypothesis is that the residuals are normal distributed
shapiro.test(res_raw)



### stepwise factor elimination  #### NB: Using AIC

# Full model with all predictors
full_mod  <- lm(X5 ~ X1 + X2 + X3 + X4, data = df)

# Null model (intercept only)
null_mod  <- lm(X5 ~ 1, data = df)

# Forward factor selection (Starting from null model and adding factors)
set.seed(123)
forward_fit <- step(
  object   = null_mod,
  scope    = list(lower = null_mod, upper = full_mod),
  direction = "forward",
  trace     = 1  # set to 0 to silence output
)
summary(forward_fit)

# Backwards factor selection (Starting from full model and remove factors)
set.seed(123)
backward_fit <- step(
  object    = full_mod,
  direction = "backward",
  trace     = 1
)
summary(backward_fit)

# or use the MASS package
stepAIC_forward <- stepAIC(null_mod,
                           scope = list(lower = null_mod, upper = full_mod),
                           direction = "forward", trace = 0)

stepAIC_backward <- stepAIC(full_mod, direction = "backward", trace = 0)

stepAIC_both <- stepAIC(null_mod,
                        scope = list(lower = null_mod, upper = full_mod),
                        direction = "both", trace = 1)

###Alcohol opgave"
setwd("C:/Users/KristineSøgaard/Downloads/medicinsk statistik/lineary_regression/raw-data")
df <- read_dta("Alcohol_and_reaction_time.dta")

#Open the data set Alcohol and reaction time. Assume normality (though it is not true for the alcohol variable).
#Run the appropriate correlation analysis for each of the following:
#a) Average units of alcohol with reaction time.

ggplot(df, aes(x = alcunits, y = RT)) +
  geom_point(size = 3, colour = "blue") +
  geom_smooth(method = "lm",
              se = TRUE,
              colour = "red") +
  labs(
    title = "Linear Regression of Average units of alcohol in correlation to reaction time",
    x = "Units of alcohol",
    y = "Reaction time"
  ) +
  theme_minimal()


# Loading the data into a data frame called df
df <- read_dta("Alcohol_and_reaction_time.dta")

# --- Unadjusted correlation ---
cor.test(df$alcunits, df$RT)

#Pearson's product-moment correlation

#data:  df$alcunits and df$RT
#t = 5.9678, df = 58, p-value = 1.54e-07
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# 0.4302613 0.7528167
#sample estimates:
#      cor
#0.6167983


#b) Mood with reaction time.

ggplot(df, aes(x = RT, y = mood)) +
  geom_point(size = 3, colour = "blue") +
  geom_smooth(method = "lm",
              se = TRUE,
              colour = "red") +
  labs(
    title = "Linear Regression of mood in correlation to reaction time",
    x = "Mood",
    y = "Reaction time"
  ) +
  theme_minimal()

# --- Unadjusted correlation ---
cor.test(df$mood, df$RT)


#Pearson's product-moment correlation

#data:  df$mood and df$RT
#t = -2.2074, df = 58, p-value = 0.03126
#alternative hypothesis: true correlation is not equal to 0
#95 percent confidence interval:
# -0.49716517 -0.02632026
#sample estimates:
#       cor
#-0.2783844

#c) Mood with reaction time, controlling for average units of alcohol.

# --- PARTIAL correlations ---
# mood ~ RT (controlling for units of alcohol)
pcor.test(df$mood, df$RT, df["alcunits"], method = "pearson")

#  estimate    p.value statistic  n gp  Method
#1 -0.2455669 0.06083476 -1.912552 60  1 pearson

# --- SEMI-PARTIAL (PART) correlations ---
# mood ~ RT (controlling for units of alcohol)
spcor.test(df$mood, df$RT, df["alcunits"], method = "pearson")

#estimate    p.value statistic  n gp  Method
#1 -0.2431102 0.06354582 -1.892211 60  1 pearson


#Når man laver p-værdier på hældninger, ser man på, om hældningen er statistisk signifikant forskellig fra 0.

#Et hyppigt problem med multipel lineær regression er, at enhederne for hældningen ofte er forskellig.
#derfor vil man standardisere hældningerne

