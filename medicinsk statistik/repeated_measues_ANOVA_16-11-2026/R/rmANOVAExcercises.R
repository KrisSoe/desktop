#Installing the packages if needed
pkgs <- c("haven", "readxl","ggplot2","tidyr","dplyr", "car","afex","emmeans")
options(repos = c(CRAN = "https://cloud.r-project.org"))
to_get <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_get)) install.packages(to_get, dependencies = TRUE)

library(haven)
library(readxl)
library(dplyr)
library(ggplot2)
library(car) 
library(afex)
library(emmeans)

data <- read_dta("exersise2WANOVA.dta")

# Convert Sex and Species to factors with labels
data <- data %>%
  mutate(
    Sex = factor(Sex, levels = c(1, 2), labels = c("Male", "Female")),
    Species = factor(Species, levels = c(1, 2, 3), labels = c("Species1", "Species2", "Species3"))
  )

# Two-way ANOVA
anova_result <- aov(AAA ~ Sex * Species, data = data)
summary(anova_result)

### Making a post-hoc test to test which species are different from each other
TukeyHSD(anova_result, which = "Species")

# Interaction plot with 95% confidence intervals
ggplot(data, aes(x = Species, y = AAA, color = Sex, group = Sex)) +
  stat_summary(fun.data = mean_cl_normal, geom = "line", size = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Interaction Plot of AAA by Sex and Species with 95% CI",
       x = "Species",
       y = "Mean AAA") +
  theme_minimal()




####repeated measures ANOVA
data <- read_dta("exercise_rmANOVA.dta")

data <- data %>%
  mutate(
    subject = factor(subject),
    trial = factor(trial),
    anxiety = factor(anxiety, levels = c(0, 1), labels = c("Low", "High"))
  )

# Mixed-design repeated-measures ANOVA
anova_result <- aov_ez(
  id = "subject",
  dv = "errors",
  data = data,
  between = "anxiety",
  within = c("trial"),
  type = 3,
  anova_table = list(
    correction = "GG",
    es = "pes"
  )
)
summary(anova_result)


# Estimated marginal means
emm <- emmeans(anova_result, ~ trial)

# Pairwise comparisons
pairs(emm, adjust = "tukey")
confint(pairs(emm, adjust = "tukey"))

##### Making a plot #####
# Estimated marginal means
emm_plot <- emmeans(anova_result, ~ trial * anxiety)

# Convert to dataframe
emm_df <- as.data.frame(emm_plot)

# Interaction plot
ggplot(emm_df,
       aes(x = trial, y = emmean, color = anxiety, group = anxiety)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15,
    linewidth = 0.7
  ) +
  labs(
    title = "Interaction between Trial and Anxiety Level",
    x = "Trial",
    y = "Estimated marginal mean Errors",
    color = "Anxiety Level"
  ) +
  theme_minimal(base_size = 13)






