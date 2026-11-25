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


### Loading the data from the local dir
data <- read_dta("example2WANOVA.dta")

# Convert Sex and Hormone to factors with labels
data <- data %>%
  mutate(
    Sex = factor(Sex, levels = c(1, 2), labels = c("Female", "male")),
    Hormone = factor(Hormone, levels = c(1, 2), labels = c("No", "Yes"))
  )

# Two-way ANOVA
anova_result <- aov(Calcium ~ Sex * Hormone, data = data)
summary(anova_result)

# Interaction plot with 95% confidence intervals
ggplot(data, aes(x = Hormone, y = Calcium, color = Sex, group = Sex)) +
  stat_summary(fun.data = mean_cl_normal, geom = "line", size = 1) +
  stat_summary(fun.data = mean_cl_normal, geom = "point", size = 3) +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(title = "Interaction Plot of Calcium by Sex and Hormone with 95% CI",
       x = "Hormone",
       y = "Sex") +
  theme_minimal()


#### Repreted Measures ANOVA #########

data <- read_dta("example_rmANOVA.dta")

# Convert variables to factors
data <- data %>%
  mutate(
    subject = factor(subject),
    field   = factor(field, levels = c(1, 2), labels = c("Independent", "Dependent")),
    Wtype   = factor(Wtype, levels = c(1, 2), labels = c("Form", "Color")),
    Cue     = factor(Cue, levels = c(1, 2, 3), labels = c("Neutral", "Conguent","Inconguent"))
  )

# Mixed-design repeated-measures ANOVA
anova_result <- aov_ez(
  id = "subject",
  dv = "Time",
  data = data,
  between = "field",
  within = c("Wtype", "Cue"),
  type = 3,
  anova_table = list(
    correction = "GG",
    es = "pes"
  )
)

# Display results
summary(anova_result)

# Estimated marginal means
emm <- emmeans(anova_result, ~ Cue)

# Pairwise comparisons
pairs(emm, adjust = "tukey")
confint(pairs(emm, adjust = "tukey"))

##### Making a plot #####
# Estimated marginal means
emm_plot <- emmeans(anova_result, ~ Cue * Wtype * field)

# Convert to dataframe
emm_df <- as.data.frame(emm_plot)

# Interaction plot
ggplot(emm_df,
       aes(x = Cue, y = emmean, color = Wtype, group = Wtype)) +
  geom_line(linewidth = 1) +
  geom_point(size = 3) +
    geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL),
    width = 0.15,
    linewidth = 0.7
  ) +
    facet_wrap(~ field) +
    labs(
    title = "Interaction between Cue, Wtype and Field",
    x = "Cue",
    y = "Estimated marginal mean Time",
    color = "Wtype"
  ) +
    theme_minimal(base_size = 13)





