library(tidyverse)
library(survival)
library(patchwork)
library(broom)
library(survminer)
library(forestmodel)
library(haven)
library(reactable)

#Assignments

#1. Load the data frmgham2_survival.dta available on moodle or from medisr.

install.packages('medisr', repos = c('https://danmazjen.r-universe.dev', 'https://cloud.r-project.org'))
library(medisr)
data(frmgham2_survival)
View(frmgham2_survival)

frmg_surv <- data(frmgham2_survival)

#2. Generate a variable that indicates if a subjects BMI is above 25. Drop patients with missing BMI.

library(dplyr)

data2 <- data %>%
  filter(!is.na(BMI)) %>%      # drop personer med manglende BMI
  mutate(BMI_over25 = BMI > 25)

#3. Make life-tables for stroke-free survival comparing subjects above/below a BMI of 25 at 1, 3, 5, 10 and 15 years. Outcome indicator; stroke_or_death Time to stroke or death; timestrk

#4. Plot the Kaplan-meier curves for both BMI groups.

#5. Run a Log Rank test comparing subjects above/below a BMI of 25.

#6. Make a crude Cox regression analysis comparing subjects above/below a BMI of 25.

#7. Repeat the Cox regression analysis adjusting for age, sex, current smoking status and diabetes.

#8. Repeat above exercises for BMI above 30. How did the results change?

