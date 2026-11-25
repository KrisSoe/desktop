###Repeated measures ANOVA###

#Between - alm t-test
#Within - parret t-test
#hvad er forskellen ellers på between og within subject factors?

#One way ANOVA, undersøger først om en kontinuær variabel er forskellig i mere end 2 grupper (hvis der kun er 2 grupper, laver man bare en t-test)
#fik ikke helt fat i hvad det gik ud på, men man kiggede vidst både på between og within subject factors

#Two-factor ANOVA, regner kun med between subject factors
#1. calculate cell means
#2. calculate the total mean
#3. calculate tratment means - for hver faktor vil man beregne middelværdi eks. for treatment og no treatment og deriblandt mænd og kvinder
#4. calculate total SS
#5. calculate the cell SS
#6. calculate within-cells error/residual SS - det man ikke kan forklare
#7. calcucalte factor A SS - middelværdi af eks. no treatment og treatment fratrukket total mean
#8. calculate factor B SS - middelværdi af eks. mænd og kvinder fratrukket total mean
#9. calculate A*B interaction SS

#når man beregner total/grand mean mister man en frihedsgrad, hvad end det betyder
#slide 9, viser også nogle andre faktorer hvor man mister frihedsgrader

#Den kumultative f-værdi afhænger af frihedsgraderne og siger også noget om signifikans tror jeg?

#slide 12 visualiserer 2-factor ANOVA.

###Eksempel 1###


###Eksempel 2###

###Load data###
library(haven)
library(dplyr)
library(ggplot2)
library(Hmisc)
example2WANOVA <- read_dta("data/example2WANOVA.dta")

#Convert Sex and Hormone factors with labels
example2WANOVA <- example2WANOVA %>%
  mutate(
    Sex = factor (Sex, levels = c(1,2), labels = c("Female", "Male")),
    Hormone = factor(Hormone, levels = c(1,2), labels = c("No", "Yes"))
  )

#Two-way ANOVA
anova_result <- aov(Calcium ~ Sex* Hormone, data = example2WANOVA)
summary(anova_result)

             Df Sum Sq Mean Sq F value   Pr(>F)
Sex          1   70.3    70.3   3.733   0.0713 .
Hormone      1 1386.1  1386.1  73.585 2.22e-07 ***
Sex:Hormone  1    4.9     4.9   0.260   0.6170
Residuals   16  301.4    18.8
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Interaction plot with 95% confidence intervals
interaction_plot <- ggplot(example2WANOVA, aes(x = Hormone, y = Calcium, color = Sex, group = Sex)) +
  stat_summary(fun.data = mean_cl_normal, geom = "line", size = 1)+
  stat_summary(fun.data = mean_cl_normal, geom = "point", size = 3)+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)+
  labs(title = "Interaction Plot of Calcium by Sex and Hormone with 95% CI",
       x = "Hormone",
       y = "Sex") +
  theme_minimal()


###Exercise 2?###

###Load data###

library(haven)
exersise2WANOVA <- read_dta("data/exersise2WANOVA.dta")
View(exersise2WANOVA)

#A study is made of amino acids in the hemolymph of millipedes. For a sample of 4 males and 4 females of each of 3 species,
#the concentrations if the amino acid alanine (in mg/100ml) are listed in “exersise12-1.sav”
#Test the hypothesis that there is no difference in mean hemolymph alanine concentration among the 3 species.

#Det er en klassisk one-way ANOVA, fordi:
#Du har én kategorisk, uafhængig variabel: Species (3 arter).
#Du har én numerisk, afhængig variabel: AAA (alanin-koncentration).

Nulhypotese (H₀):

  Der er ingen forskel i den gennemsnitlige alanin-koncentration mellem de tre arter.

Alternativ hypotese (H₁):

  Mindst én art har en anden gennemsnitlig alanin-koncentration.

#Gør variablerne til faktorer
library(dplyr)

exersise2WANOVA <- exersise2WANOVA %>%
  mutate(
    Species = factor(Species),
    Sex = factor(Sex)
  )

anova_result1 <- aov(AAA ~ Species, data = exersise2WANOVA)

summary(anova_result1)

            Df Sum Sq Mean Sq F value Pr(>F)
Species      2  55.26  27.630    3.16 0.0631 .
Residuals   21 183.63   8.744
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Test the hypothesis that there is no difference between males and females in mean hemolymph alanine.
anova_result1.2 <- aov(AAA ~ Sex, data = exersise2WANOVA)

summary(anova_result1.2)

            Df Sum Sq Mean Sq F value   Pr(>F)
Sex          1  138.7  138.72   30.47 1.51e-05 ***
Residuals   22  100.2    4.55
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Test the hypothesis that there is no interaction between sex and species in the mean hemolymph concentration.
anova_result1.3 <- aov(AAA ~ Species * Sex, data = exersise2WANOVA)

summary(anova_result1.3)

            Df Sum Sq Mean Sq F value   Pr(>F)
Species      2  55.26   27.63  13.082  0.00031 ***
Sex          1 138.72  138.72  65.679 2.04e-07 ***
Species:Sex  2   6.89    3.45   1.631  0.22331
Residuals   18  38.02    2.11
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# p = 0,22 -> ingen signifikant interaktion

#Prepare a graph to depict your results.
library(ggplot)
ggplot(exersise2WANOVA,
       aes(x = Species, y = AAA, fill = Species)) +
  geom_boxplot() +
  labs(
    title = "Alanine concentration across three species",
    x = "Species",
    y = "Alanine concentration (mg/100 ml)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

library(ggplot2)

ggplot(exersise2WANOVA,
       aes(x = Species,
           y = AAA,
           color = Sex,
           group = Sex)) +

  stat_summary(fun = mean,
               geom = "line",
               linewidth = 0.8) +

  stat_summary(fun = mean,
               geom = "point",
               size = 2) +

  stat_summary(fun.data = mean_cl_normal,
               geom = "errorbar",
               width = 0.2) +

  labs(
    title = "Interaction Plot of AAA by Sex and Species with 95% CI",
    x = "Species",
    y = "Mean AAA",
    color = "Sex"
  ) +

  theme_minimal()
#If there is a difference between the species, use a Tukey test to assess the mean difference among species
TukeyHSD(anova_result1.3, which = "Species")
Tukey multiple comparisons of means
95% family-wise confidence level

Fit: aov(formula = AAA ~ Species * Sex, data = exersise2WANOVA)

$Species
         diff        lwr        upr     p adj
2-1   -3.7125 -5.5670304 -1.8579696 0.0002068
3-1   -2.0125 -3.8670304 -0.1579696 0.0322560
3-2    1.7000 -0.1545304  3.5545304 0.0756331

#Repeated measures ANOVA
#normality?
#homogeneity?
#sphericity - man bruger greenhouse geisser og repeated ANOVA

#slide 33, data is positively skewed, kan ses fordi medianen ligger langt nede ift halen, spredningen overestimeres i en retning og underestimeres i en anden
#derfor log transformerer man det og så kan man vurdere hvilken model der er bedst

###Exercise###

#rmANOVA:
# The dataset “repeatedANOVAAnxiety.sav” contains the number of errors made by students in repeated tests.

###Load data###
library(haven)
exercise_rmANOVA <- read_dta("data/exercise_rmANOVA.dta")
View(exercise_rmANOVA)

# Is there a time effect i.e. are the students learning?
library(tidyverse)
library(afex)
exercise_rmANOVA <- exercise_rmANOVA %>%
  mutate(
    subject = factor(subject),
    trial = factor(trial),
    anxiety = factor(anxiety, levels = c(0,1), labels = c("Yes", "No"))
  )

anova_result2 <- aov_ez(
  id = "subject",
  dv = "errors",
  data = exercise_rmANOVA,
  between = "anxiety",
  within = "trial",
  type = 3,
  anova_table = list(
    correction = "GG",
    es = "pes"
  )
)

anova_result2

Anova Table (Type 3 tests)

Response:errors
         Effect          df   MSE          F  pes p.value
1       anxiety       1, 10 16.82       0.66 .061    .437
2         trial 1.75, 17.51  4.61 123.36 *** .925   <.001
3 anxiety:trial 1.75, 17.51  4.61       1.06 .096    .358
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘+’ 0.1 ‘ ’ 1

Sphericity correction method: GG

#Kig på rækken trial:

#F = 123.36
#p < 0.001
#partial eta squared (pes) = 0.925

#Da p-værdien er mindre end 0,05, er der en statistisk signifikant effekt af tid.

#Det betyder, at antallet af fejl ændrer sig signifikant på tværs af de fire trials.

#Det er meget sandsynligt, men ANOVA-tabellen fortæller kun, at antallet af fejl ændrer sig.
#For at undersøge, om de faktisk bliver bedre, kan du køre:

aggregate(errors ~ trial,
          data = exercise_rmANOVA,
          mean)
#Hvis gennemsnittet af fejl falder fra trial 1 til trial 4, tyder det på, at de studerende lærer af gentagelserne.

trial   errors
1     1 16.50000
2     2 11.58333
3     3  7.75000
4     4  4.25000

#Det bekræfter, at de studerende bliver markant bedre gennem de fire trials.
#De bliver bedre fordi de i gennemsnit laver mindre fejl for hver "trial" de skal igennem.

# Is there an effect of Anxiety?

         Effect          df   MSE          F  pes p.value
1       anxiety       1, 10 16.82       0.66 .061    .437

# p = 0,437 -> Ingen signifikant effekt

# Is the possible time effect the same for the two anxiety groups?
          Effect          df   MSE          F  pes p.value
3 anxiety:trial 1.75, 17.51  4.61       1.06 .096    .358

# p = 0,358 -> ingen signifikant interaktion

# Make a figure depicting the results.

library(ggplot2)

ggplot(exercise_rmANOVA,
       aes(x = trial,
           y = errors,
           color = anxiety,
           group = anxiety)) +

  stat_summary(fun = mean,
               geom = "line",
               linewidth = 1) +

  stat_summary(fun = mean,
               geom = "point",
               size = 3) +

  labs(
    title = "Change in errors across trials by anxiety group",
    x = "Trial",
    y = "Mean number of errors",
    color = "Anxiety group"
  ) +

  theme_minimal()

# Make a posthoc test of the possible time effect.

library(emmeans)

posthoc_time <- emmeans(
  anova_result2,
  ~ trial
)

pairs(posthoc_time, adjust = "bonferroni")

contrast estimate    SE df t.ratio p.value
X1 - X2      4.92 0.708 10   6.944  0.0002
X1 - X3      8.75 0.827 10  10.574 <0.0001
X1 - X4     12.25 0.920 10  13.320 <0.0001
X2 - X3      3.83 0.450 10   8.513 <0.0001
X2 - X4      7.33 0.545 10  13.451 <0.0001
X3 - X4      3.50 0.394 10   8.874 <0.0001

Results are averaged over the levels of: anxiety
P value adjustment: bonferroni method for 6 tests

TukeyHSD(anova_result2, which = "trial")

# Estimated marginal means
emm <- emmeans(anova_result, ~ trial)

# Pairwise comparisons
pairs(emm, adjust = "tukey")
confint(pairs(emm, adjust = "tukey"))

contrast estimate    SE df t.ratio p.value
X1 - X2      4.92 0.708 10   6.944  0.0002
X1 - X3      8.75 0.827 10  10.574 <0.0001
X1 - X4     12.25 0.920 10  13.320 <0.0001
X2 - X3      3.83 0.450 10   8.513 <0.0001
X2 - X4      7.33 0.545 10  13.451 <0.0001
X3 - X4      3.50 0.394 10   8.874 <0.0001

Results are averaged over the levels of: anxiety
P value adjustment: tukey method for comparing a family of 4 estimates

#Det betyder, at:

#Trial 2 har signifikant færre fejl end trial 1.
#Trial 3 har signifikant færre fejl end trial 2.
#Trial 4 har signifikant færre fejl end trial 3.

#Og tilsvarende er alle de øvrige sammenligninger også signifikante.
