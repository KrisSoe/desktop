things <- c(1,3,4,8,9)
mean(things)
sd(things)

#> things <- c(1,3,4,8,9)
#> mean(things)
#[1] 5
#> sd(things)
#[1] 3.391165

#skewed - data that does not follow a normal distribution
#kurtosis - describes how much of a probability distribution falls in the tails instead of its center

epil <- MASS::epil

library(dplyr)
library(ggplot2)
library(broom)
library(tidyverse)
library(marginaleffects)
library(sandwich)

epil <- epil |>
  rename(
    seizure_count = y,
    drug = trt,
    baseline_seizures = base,
    trial_end = V4,
    log_base = lbase,
    log_age = lage
  ) |>
  filter(trial_end == 1) |>
  mutate(
    seizure_count = as.integer(seizure_count),
    period = factor(period,
                    levels = c(4),
                    labels = c("week_8")
    )
  )

epil |>
  dplyr::glimpse()

epil |>
  count(drug)

#       drug  n
#1   placebo 28
#2 progabide 31

seizure_lims <- range(c(epil$baseline_seizures, epil$seizure_count))

baseline_data_plot <- epil |>
  ggplot() +
  geom_histogram(
    aes(x=baseline_seizures),
    bins=30
  ) +
  coord_cartesian(xlim = seizure_lims)

posttrial_data_plot <- epil |>
  ggplot() +
  geom_histogram(
    aes(x=seizure_count),
    bins=30
  ) +
  coord_cartesian(xlim = seizure_lims)

# This part takes the two ggplot's and puts them side-by-side using {patchwork}
library(patchwork)
baseline_data_plot + posttrial_data_plot

epil |>
  ggplot(aes(baseline_seizures,seizure_count,color=drug)) +
  geom_point(alpha=0.6) +
  geom_smooth()

#logistisk regression - man er interesseret i log(OR)=log(P/1-P), hvilket er transformeringen som bruges til den lineære regression.
#når man har normalfordelingen er man interesseret i N~(mu,sigma)
#når man har binære værdier er man interesseret i B~(N,P) som er B~(gentagelser/deltagere,sandsynlighed)
#i dette tilfælde kan man ikke bruge almindelig logistisk regression, da dette bygger på binære værdier

y <- rbinom(1e5,1000,1/1000) #Kort fortalt har du simuleret 100.000 stikprøver fra en binomialfordeling og undersøgt dens gennemsnit og varians.
#rbinom() genererer tilfældige tal fra en binomialfordeling.
#1e5 = 100.000 observationer.
#1000 = antal forsøg pr. observation (n = 1000).
#1/1000 = sandsynligheden for succes i hvert forsøg (p = 0,001).
c(mean(y),var(y))
#det observerede gennemsnit
#den observerede varians

#[1] mean: 1.0007900 var:0.9945993

#[1] mean:0.993760 var:0.992411 - ved ikke hvad jeg gjorde anderledes men nu fik jeg dette?

#[1] mean:1.0012900 var:0.9986383 - og nu fik jeg denne værdi?
#Formålet er ofte at vise, at en binomialfordeling med stort n og lille p kan approksimeres af en Poisson-fordeling med parameter
#λ=np=1
#Her er det nemlig præcis situationen: n=1000, p=0.001, så λ=1

#plotter en gaussian og possion model:

mu <- mean(epil$seizure_count) # 7.305085
sigma <- sd(epil$seizure_count) # 9.649467

set.seed(363461174)
dist_draws <- data.frame(
  gaussian = rnorm(1000,mu,sigma),
  poisson = rpois(1000,mu)
) |>
  tidyr::pivot_longer(
    cols = c("gaussian","poisson"),
    names_to = "dist",
    values_to = "value"
  )

dist_draws |>
  ggplot() +
  geom_histogram(
    aes(x=value),
    bins=30
  ) +
  facet_wrap(~dist) +
  labs(
    x="Seizures",
    y="Count (a.u.)"
  ) +
  theme(axis.text.y = element_blank())

#refleksion over ovenstående
#Which best captures the data?
#Which makes the worst fit?
#What else do you notice?

m1_drug <- glm(
  seizure_count ~ drug,
  family = poisson(link="log"),
  data = epil
)
summary(m1_drug)

#Call:
#  glm(formula = seizure_count ~ drug, family = poisson(link = "log"),
#      data = epil)
#
#Coefficients:
#               Estimate Std. Error z value Pr(>|z|)
#(Intercept)    2.07497    0.06696  30.986   <2e-16 ***
#  drugprogabide -0.17142    0.09640  -1.778   0.0754 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for poisson family taken to be 1)
#
#Null deviance: 476.25  on 58  degrees of freedom
#Residual deviance: 473.08  on 57  degrees of freedom
#AIC: 664.85
#
#Number of Fisher Scoring iterations: 6

parameters::parameters(m1_drug)

#Parameter          | Log-Mean |   SE |        95% CI |     z |      p
#-------------------------------------------------------------------
#  (Intercept)      |     2.07 | 0.07 | [ 1.94, 2.20] | 30.99 | < .001
#drug [progabide]   |   -0.17  | 0.10 | [-0.36, 0.02] | -1.78 | 0.075

#man får desuden denne besked
#The model has a log- or logit-link. Consider using `exponentiate = TRUE` to
#interpret coefficients as ratios.

#hvad betyder log-mean her? ved interceptet er det log antallet og ved drug er det log risk ratio
#logistisk regression laver odds ratioer og poisson laver risk ratioer
#

coef(m1_drug)
#  (Intercept) drugprogabide
#   2.0749673    -0.1714164

#koefficienterne skal transformeres, da de stadig er i log format
#
broom::tidy(m1_drug,
            exponentiate = TRUE,
            conf.int = TRUE)
# A tibble: 2 × 7
#term               estimate std.error statistic   p.value conf.low conf.high
#<chr>                <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#  1 (Intercept)      7.96     0.0670     31.0  8.36e-211    6.96       9.06
#  2 drugprogabide    0.842    0.0964     -1.78 7.54e-  2    0.697      1.02

#dette gør det næsten det samme, men man får lidt anden info tror jeg:

parameters::parameters(m1_drug,exponentiate = TRUE)

#Parameter          |  IRR |   SE |       95% CI |     z |      p
#--------------------------------------------------------------
#  (Intercept)      | 7.96 | 0.53 | [6.96, 9.06] | 30.99 | < .001
#drug [progabide]   | 0.84 | 0.08 | [0.70, 1.02] | -1.78 | 0.075

#The intercept reads 7.9642857, which is the expected seizure_count for
#the placebo group (and the average as we would expect!)
#Confirm this for yourself using the following code:
mean(subset(epil,drug=="placebo")$seizure_count) #[1] 7.964286

#Next, it’s the 𝛽1drug named drugprogabide in R: 0.8424707, which is the expected
#count ratio in seizure_count when being on progabide for 8 weeks, compared to placebo.
#Since the count ratio is less than 1, the expected seizure count is lower.

#In fact, 15.8% lower on average.
#To confirm this for yourself, run the following code:
mean(subset(epil,drug=="progabide")$seizure_count) #[1] 6.709677

#and see the average count for the treatment group.
#Finally, prove to yourself that this is what the two coefficients mean
#in relation to each other by this code:
exp(m1_drug$coefficients[2])*mean(subset(epil,drug=="placebo")$seizure_count) #drugprogabide 6.709677

#So the expected effect of being on progabide for 8 weeks was a reduction of
#1.2546083 seizures during 8 weeks, based on this small sample.

#TILFØRELSE AF COVARIATER

m2_baseline <- glm(
  seizure_count ~ drug + baseline_seizures,
  family = poisson(link="log"),
  data = epil
)

broom::tidy(m2_baseline,
            exponentiate = TRUE,
            conf.int = TRUE
)
# A tibble: 3 × 7
#term              estimate std.error statistic  p.value conf.low conf.high
#<chr>                <dbl>     <dbl>     <dbl>    <dbl>    <dbl>     <dbl>
# 1 (Intercept)          3.37    0.0855      14.2  9.07e-46    2.84      3.98
# 2 drugprogabide        0.730   0.0985      -3.20 1.37e- 3    0.601     0.885
# 3 baseline_seizures    1.02    0.00104     20.7  1.73e-95    1.02      1.02

parameters::parameters(m2_baseline,exponentiate = TRUE)
#Parameter         |  IRR |       SE |       95% CI |     z |      p
#-------------------------------------------------------------------
#  (Intercept)     | 3.37 |     0.29 | [2.84, 3.98] | 14.20 | < .001
#drug [progabide]  | 0.73 |     0.07 | [0.60, 0.88] | -3.20 | 0.001
#baseline seizures | 1.02 | 1.06e-03 | [1.02, 1.02] | 20.73 | < .001

#man kan sammenligne modellerne såldes:
coef_df <- bind_rows(
  tidy(m1_drug, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "Model 1 - drug only"),
  tidy(m2_baseline, conf.int = TRUE, exponentiate = TRUE) |> mutate(model = "Model 2 - add baseline seizure")
)

coef_df |>
  ggplot(aes(x = estimate, y = model)) +
  geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
  geom_point(size = 2) +
  geom_errorbar(
    aes(xmin = conf.low, xmax = conf.high),
    width = 0.2
  ) +
  facet_wrap(~term, scales = "free_x") +
  labs(
    x = "Coefficient estimate (95% CI)",
    y = NULL
  ) +
  theme_minimal()

#Here, we plot the expected seizure count at the 8 week follow up for
m1_drug |>
plot_predictions(
  condition=c("drug")
) +
  labs(
    y="Predicted seizures"
  )

#VIGTIGT man adderer koefficienterne inden man eksponentierer dem

#det er smart at overføre data i rater, som er rate=gennemsnit observationer/given periode tror jeg?
#

#Simulating an example:
#Consider the case of reporting events (e.g., death) at two hospitals.
#Hospital 1 records daily events (30 observations of one day each), while Hospital 2 records weekly events (4 observations, each representing seven days).
#If we simply modeled the raw event counts, Hospital 2 would naturally tend to have larger counts because each observation covers a longer period of time.

set.seed(636961739)

num_weeks <- 8 # weeks of the experiment
num_days <- 8*7 # days of the experiment

hospital1 <- rpois(num_days, 1.5) # 1.5 events per day

hospital2 <- rpois(num_weeks, 0.5*7) # 0.5 events per day scaled to a week

events <- c(hospital1, hospital2)
exposure <- c(rep(1,num_days), rep(7,num_weeks))
hospital_id <- c(rep("Aalbjerg",num_days), rep("Esborg",num_weeks))
hospital_data <- data.frame(
  events=events,
  days=exposure,
  hospital_id= as.factor(hospital_id)
)

hospital_data$log_days = log(hospital_data$days)

hospital_data

sim_m1 <- glm( events ~ hospital_id,
               family = poisson,
               data = hospital_data
)

broom::tidy(sim_m1,exponentiate = TRUE)

sim_m2 <- glm( events ~ hospital_id,
               family = poisson,
               offset = log_days, # here we added the expoure time as an offset.
               data = hospital_data
)

broom::tidy(sim_m2,exponentiate = TRUE)


#Making sound inference:

hospital_data <- hospital_data |>
  mutate(log_week = log(days/7))

sim_m3 <- glm(
  events ~ hospital_id,
  family = poisson,
  offset = log_week, # here we added the expoure time as an offset.
  data = hospital_data
)

broom::tidy(sim_m3,exponentiate = TRUE)


#Logistic vs Poisson for binary outcomes
#As you might have noted in the simulation above, the outcome was actually binary
#(event happened or not) — shouldn’t we use logistic regression for such outcomes?
#Så man kan vælge om man vil bruge logistisk eller poisson til binære outcomes
#Hvornår vil man bruge logistisk og hvornår vil man bruge poisson?

#Logistisk - mean is Np (probability scale), variance is Np(1-p) (no assumption), outcome is odds ratio OR
#Poisson - mean is lambda (count scale), variance is , outcome is incidence rate ratio/risk ratio IRR/RR

#Hvad er RR? -
#Hvad er IRR? -

#Poisson variance is not robust for binomial outcomes
#men selvom data ikke overholder antagelserne om poisson varians, kan man godt justere
#så det bliver mere robust?

#16. Poisson-regression på binært udfald
#Redegør for, hvilket effektmål der opnås ved anvendelse af Poisson-regression for a binary outcome (not time-toevent)
#a. How does this affect the confidence intervals, and why?

#17. Poisson-regression på tid-til-event udfald
#Redegør for Poisson-regression anvendt på et tid-til-event udfald i et effektstudie.
#
