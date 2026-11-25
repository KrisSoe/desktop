######################################
### Opgave løsninger til lecture 6 ###
######################################

###############################
### Opgave 2: Data cleaning ###
###############################

###############
### Library ###
###############
library(readr)
library(tidyverse)


#################
### Load data ###
#################
hd_data <- read_csv("RawData/hd_data.csv")
View(hd_data)

#Dropping this colloumn as it is meaningless
hd_data$...1 <- NULL
##############
### Opgaver###
##############
# 1) Mange af variablene i datasættet er skrevet som numeriske værdier e.g. sex=0/1. Lav nogle nye character variable, e.g. sex_chr=Male/Female
hd_data <- hd_data %>% 
  mutate(
    Sex_chr=ifelse(Sex==0,yes="Female",no="Male"),
    
    FBS_chr=ifelse(Fbs==0,yes="Normal",no="Fasting glucose > 120 mg/DL"),
    
    RestECG_chr =case_when(
      RestECG==0~"Normal",
      RestECG==1~"ST-T wave abnormality",
      RestECG==2~"LVH") %>% factor(levels = c("Normal","ST-T wave abnormality","LVH")),
    
    ExAng_chr=ifelse(ExAng==0,yes="No chest pain",no="Chest pain during exercise"),
    
  )

# 2) Variablen "HD" tager værdierne "Yes" eller "no", lav denne som en ny character variabel e.g. Yes=HDP eller no=NHD.     
hd_data <- hd_data %>% 
  mutate(
    HD_new=ifelse(HD=="Yes",yes="HDP",no="NHD")
  )

# 3) variablen "Chestpain", "Thal", er skrevet med små bogstaver, lav en ny varibel med stort start bogstav.
hd_data <- hd_data %>% 
  mutate(
    ChestPain_new=case_when(
      ChestPain=="asymptomatic"~"Asymptomatic",
      ChestPain=="nonanginal"~"Nonanginal",
      ChestPain=="nontypical"~"Nontypical",
      ChestPain=="typical"~"Typical"),
    
    Thal_new=case_when(
      Thal=="fixed"~"Fixed",
      Thal=="normal"~"Normal",
      Thal=="reversible"~"Reversible"
    )
    
  )

# 4) Chol er angivet i mg/dL udregn denne som g/L
hd_data <- hd_data %>% 
  mutate(
    #1mg=0.001g
    #1dL=0.1L
    Chol_g_L=Chol/100
  )

# 5) Gem de cleanede data some en ".RDS" fil i GeneratedData mappen
saveRDS(hd_data,file = "./GeneratedData/hd_clean.rds")

# Ekstra opgaver

# 1) Er der en tydelig alders forskel mellem mænd og kvinder?
hd_data %>% 
  group_by(Sex_chr) %>% 
  summarise(mean_age=mean(Age),
            median_age=median(Age)
            )

# 2) Hvad er gennemsnits alderen på de forskellige typer af "ChestPain"
hd_data %>% 
  group_by(ChestPain_new) %>% 
  summarise(mean_age=mean(Age)
            )

