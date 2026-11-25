hd_data <- medisr::hd_data

SUA_data <- readr::read_csv(
  "https://zenodo.org/records/8292712/files/SUA_CVDs_risk_factors.csv"
)
readr::write_csv(SUA_data, "data-raw/SUA_data.csv")

view(SUA_data)

mean(SUA_data$Age)

library(dplyr) #for at bruge "pull"

SUA_data |> 
  pull(Age) |> 
  mean()

#Both yield the same result. The first is quick but can be difficult to extend or even read.
#The dplyr-version is easy to read and more flexible when we want to do more operations.

