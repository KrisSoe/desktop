Data visualisering
Deskriptive data, boks, violin og bar plots
13-10-2026

jv.f. session 12

hd_data er indsat

Among the variables in hd_data are:

ExAng: whether a patient experienced exercises-induced angina.

Age: how old a patient was at the time of the measurement (in years).

MaxHR: the max heart rate value we measured.

hd_data

hd_data (ctrl+shift+m)

library(ggplot2)

hd_data |>
  ggplot(aes(x=MaxHR))+
  geom_histogram()

Our plot shows that MaxHR is not normally distributed
— it has a wider tail to the left.
This implies that the data might be composed of sub-groups.

hd_data |>
  ggplot(aes(x=MaxHR)) +
  geom_histogram() +
coord_flip()


hd_data |>
  ggplot(aes(x=MaxHR)) +
  geom_boxplot()


hd_data |>
  ggplot(aes(x=MaxHR)) +
  geom_boxplot() +
  coord_flip()

hd_data |>
  ggplot(aes(x = ExAng, y = MaxHR)) +
  geom_boxplot()

"Advarselsbeskeder:
1: Orientation is not uniquely specified when both the x and y
aesthetics are continuous. Picking default orientation 'x'.
2: Continuous x aesthetic
ℹ did you forget `aes(group = ...)`?"

" Let’s change the type as a factor using
the factor function to correct this mistake."

hd_data$ExAng <- factor(
  hd_data$ExAng,
  levels = c(0, 1),
  labels = c("no", "yes")
)

hd_data |>
  ggplot(aes(x = ExAng, y = MaxHR)) +
  geom_boxplot()

hd_data |>
  ggplot(aes(ExAng, MaxHR)) +
  geom_col()

"Hmm… I’ve never seen a human being with a max heart rate of +30.000
- this must be a mistake!

It turns out that the geom_col takes the sum of all observations.
We wanted the mean!"

hd_data |>
  ggplot(aes(ExAng, MaxHR)) +
  stat_summary(geom = "col", fun = mean)


hd_data |>
  ggplot(aes(ExAng, MaxHR)) +
  stat_summary(geom = "col", fun = mean) +
  geom_jitter()

Konklusion - barplots er dårlige!!!!!

hd_data |>
  ggplot(aes(Age, MaxHR)) +
  geom_point()

hd_data |>
  ggplot(aes(Age, MaxHR, color = ExAng)) +
  geom_point()

age_maxHR_Exang <- hd_data |>
  ggplot(aes(Age, MaxHR, color = ExAng)) +
  geom_point() +
  labs(x = "Age (years)", y = "Max heart rate (BPM)") +
  geom_smooth()

ggsave(filename = "figures/subgroup-association.png",plot=age_maxHR_Exang)
