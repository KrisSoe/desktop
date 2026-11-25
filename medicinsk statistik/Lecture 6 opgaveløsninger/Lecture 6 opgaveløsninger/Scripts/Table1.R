######################################
### Opgave løsninger til lecture 6 ###
######################################

#########################
### Opgave 3: Table 1 ###
#########################
# 0) Lav et nyt script kaldet e.g "tabel1" og gem dette script i "scripts" folderen

###############
### Library ###
###############
library(tidyverse)
library(tableone)

#################
### Load data ###
#################
hd_data <- readRDS(file = "./GeneratedData/hd_clean.rds")

###############
### Opgaver ###
###############
# 1) Lav en tabel 1 som beskriver populationen
# 1.0) Udvælg hvilke variable som skal med i tabellen
myvars <- c("Age","Sex_chr","ChestPain_new","RestBP","Thal_new","Chol_g_L","MaxHR","FBS_chr","RestECG_chr","Ca")

# 1.1) Udvælg hvilke varible som skal angive med Median værdier
Median_var <- c("Age","RestBP","Chol_g_L")

# 1.3) Vælg hvilke variable der er categorisk  
cat_vars <- c("Sex_chr","ChestPain_new","Thal_new","FBS_chr","RestECG_chr","Ca")


tab1_overall <- CreateTableOne(data = hd_data,
                       vars = myvars,
                       factorVars = cat_vars)

# 1.3) Angiv median værdierne med min-Max 
# 1.4) Få tabellen til at vise alle grupper
tab1_overall <- print(tab1_overall,
                      nonnormal = Median_var,
                      minMax = TRUE,
                      showAllLevels = TRUE)


# 2) Lav en Tabel 1 strateficeret på Køn
tab1_sex <- CreateTableOne(data = hd_data,
                               vars = myvars,
                               factorVars = cat_vars,
                           strata = "Sex_chr")
tab1_sex <- print(
  tab1_sex,
  nonnormal = Median_var,
  minMax = TRUE,
  showAllLevels = TRUE
)

# 3) Lav en Tabel 1 strateficeret på Heart disease status
tab1_HDS <- CreateTableOne(data = hd_data,
                           vars = myvars,
                           factorVars = cat_vars,
                           strata = "HD_new")
tab1_HDS <- print(
  tab1_HDS,
  nonnormal = Median_var,
  minMax = TRUE,
  showAllLevels = TRUE
)

# 4) Ser i nogle forskelle mellem grupperne? (Diskuter dette)
# 5) Gem tabellerne som en Csv fil i Results mappen
library(htmlTable)
write.csv(tab1_overall,
          file = "./Results/Tables/table_overall.csv")
write.csv(tab1_sex,
          file = "./Results/Tables/table_sex.csv")
write.csv(tab1_HDS,
          file = "./Results/Tables/table_HDS.csv")
