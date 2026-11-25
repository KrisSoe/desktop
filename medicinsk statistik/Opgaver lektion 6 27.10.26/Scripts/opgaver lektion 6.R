###############
### Opgaver ###
###############

### Opagve 1: projekt setup ----
# 1) Lav en projekt mappe og folder (RawData, GeneratedData, Scripts, Results etc)
# 2) Opret et R-projekt
# 3) Download hd_data.csv dataene og læg dem i "RawData" folderen
# 4) Indlæs hd_data.csv i R

hd_data <- medisr::hd_data #data indlæst fra medis-pakken

saveRDS(object = hd_data, file = "./RawData/hd_data.Rdata") #derefter er de rå data gemt således

cv_hd_data <- readRDS(file = "RawData/hd_data.Rdata") #hvis man ville kunne man give datasætte et andet navn, her cv_hd_data

### Opgave 2: Data cleaning
# 1) Mange af variablene i datasættet er skrevet som numeriske værdier e.g. sex=0/1.
#Lav nogle nye character variable, e.g. sex_chr=Male/Female

library(tidyverse)

cv_hd_data <- cv_hd_data%>% #dette symbol %>% fortæller at dataene er binære
  mutate(
    #Sex: Male=1 og Female=0
    Sex_chr = case_when(
      Sex == 0 ~ "Female",
      Sex == 1 ~ "Male"
    ),
    ExAng_chr = case_when(
      ExAng == 0 ~ "No",
      ExAng == 1 ~ "Yes"
    ),
    HD_chr = case_when( # 2) Variablen "HD" tager værdierne "Yes" eller "no", lav denne som en ny character variabel e.g. Yes=HDP eller no=NHD.
      HD == "Yes" ~ "HDP",
      HD == "No" ~ "NHD"
    ),
    chol_gL = Chol * 0.01, # 4) Chol er angivet i mg/dL udregn denne som g/L
  )


# 2) Variablen "HD" tager værdierne "Yes" eller "no", lav denne som en ny character variabel e.g. Yes=HDP eller no=NHD.

#Lavet i forrige

# 3) variablen "Chestpain", "Thal", er skrevet med små bogstaver, lav en ny varibel med stort start bogstav.



# 4) Chol er angivet i mg/dL udregn denne som g/L

#Lavet i forrige

# 5) Gem de cleanede data some en ".RDS" fil i GeneratedData mappen

saveRDS(object = cv_hd_data, file = "./GeneratedData/cv_hd_data.Rdata")

# Ekstra opgaver
# 1) Er der en tydelig alders forskel mellem mænd og kvinder?

library(tidyverse)
library(readr)

cv_hd_data <- readRDS(file="GeneratedData/cv_hd_data.Rdata")

view(cv_hd_data)

library(tableone)

CreateTableOne(data = cv_hd_data)

myvars <- c("Age","Sex_chr","ExAng_chr","HD_chr","chol_gL","ChestPain","RestECG","MaxHR","Thal")
CreateTableOne(data = cv_hd_data,
               vars = myvars)

cat_vars <- c("Sex_chr","ExAng_chr","HD_chr")

tab1 <- CreateTableOne(data = cv_hd_data,
               vars = myvars,
               factorVars = cat_vars)
tab1

print(tab1,showAllLevels = T)

library(dplyr)

cv_hd_data %>%
  group_by(Sex_chr) %>%
  summarise(
    n = n(),
    mean_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE)
  ) #Gennemsnitsalderen for mænd er 53,8 og for kvinder 55,7, så ikke en stor forskel nej.

#Der optræder betydeligt flere mænd (68%) end kvinder (32%) i datasættet.

# 2) Hvad er gennemsnits alderen på de forskellige typer af "ChestPain"


cv_hd_data %>%
  group_by(ChestPain) %>%
  summarise(
    n = n(),
    mean_age = mean(Age, na.rm = TRUE),
    median_age = median(Age, na.rm = TRUE),
    sd_age = sd(Age, na.rm = TRUE)
  )

#asympotomatic = 55,7
#nonanginal = 53,7
#nontypical = 51,4
#typical = 55,9

### Opgave 3: Table 1 ----
# 0) Lav et nyt script kaldet e.g "tabel1" og gem dette script i "scripts" folderen
# 1) Lav en tabel 1 som beskriver populationen

tab1 <- CreateTableOne(data = cv_hd_data)

# 1.0) Udvælg hvilke variable som skal med i tabellen

myvars2 <- c("Age","Sex_chr","ExAng_chr","HD_chr","chol_gL","ChestPain","RestBP","MaxHR","Thal")
CreateTableOne(data = cv_hd_data,
               vars = myvars2)

# 1.1) Udvælg hvilke varible som skal angive med Median værdier

Median_var <- c("Age","chol_gL","RestBP","MaxHR")

# 1.2) Vælg hvilke variable der er categorisk

cat_vars <- c("Sex_chr","ExAng_chr","HD_chr","ChestPain")

tab1 <- CreateTableOne(data = cv_hd_data,
                       vars = myvars2,
                       factorVars = cat_vars)
tab1

# 1.3) Angiv median værdierne med min-Max
print(tab1,
      showAllLevels = T,
      nonnormal =Median_var,
      minMax = T)

#Age = 29, 77
#chol_gL = 1.26, 5.64
#RestBP = 94, 200
#MaxHR = 71, 202

# 1.4) Få tabellen til at vise alle grupper
print(tab1,
      showAllLevels = T)

# 2) Lav en Tabel 1 strateficeret på Køn

# 3) Lav en Tabel 1 strateficeret på Heart disease status
# 4) Ser i nogle forskelle mellem grupperne? (Diskuter dette)
# 5) Gem tabellerne som en Csv fil i Results mappen

