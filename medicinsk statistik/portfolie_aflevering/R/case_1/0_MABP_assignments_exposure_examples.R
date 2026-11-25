



library(lubridate)
library(dplyr)
library(ggplot2)
library(medisr)
library(VGAM)


data <- medisr::mabp
data <- tibble::tibble(data)



data <- data %>%
  mutate(
    below70mmHg_cum = rowSums(70 > select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    above120mmHg_cum = rowSums(120 < select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE),
    below70mmHg = 2 < below70mmHg_cum,
    above120mmHg = 2 < above120mmHg_cum,
    outs70120mmHg = 2 < above120mmHg_cum | 2 < below70mmHg_cum,
    good_functional_outcome = mrs_3m < 4
  )










