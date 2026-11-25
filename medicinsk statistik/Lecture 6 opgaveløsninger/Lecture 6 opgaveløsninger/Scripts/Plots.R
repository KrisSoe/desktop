######################################
### Opgave løsninger til lecture 6 ###
######################################

#######################
### Opgave 4: plots ###
#######################
# 0) Lav et nyt script

###############
### Library ###
###############
library(tidyverse)
library(ggplot2)

######################
### Specifications ###
######################
height <- 1680
width <- 1920
res <- 300

#################
### Load data ###
#################
hd_data <- readRDS(file = "./GeneratedData/hd_clean.rds")

### Opgave 4: Plots ----
# 1) Udvælg en variabel som i fandt interresant fra opgave 3.4 
# 1.1) Lav et boxplot for denne varibel og sammenligner mænd og kvinder 
ggplot(data = hd_data,
       mapping = aes(x = Sex_chr,y = Chol_g_L))+
  geom_boxplot() 


# 1.2) (Ekstra opgave) Lav et flot plot og gå i dybden med nogle af funktionerne fra lecture 6.1 
ggplot(hd_data,mapping = aes(x = Sex_chr, y = Chol_g_L, fill = Sex_chr),) +
  geom_boxplot(width = 0.6, outlier.shape = NA, alpha = 0.8) +
  
  #tilføj nogle punkter til box plottet, og "jitter" disse punkter så de ikke er oven på hinanden
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) + #geom_points= punkterne står på en vertikal linje
  
  #Skifte farver 
  scale_fill_manual(values =  c("Male"="#0073C2FF",
                                "Female"="#CD534CFF") 
  )+
  
  #Ændre navngivningen på akserne
  labs(
    x = "",
    y = "Serum Cholesterol level (g/L)"
  ) +
  #Baggrund tema: theme_classic() = hvid baggrund
  theme_classic() + 
  theme(
    #fjerne legend
    legend.position = "none",
    #akse titlerne bliver i fed
    axis.title = element_text(face = "bold"),
    
    #teksten på begge akser bliver i størrelse 12
    axis.text = element_text(size = 20)
  )

# 2) Lav et violin plot 
ggplot(data = hd_data,
       mapping = aes(x = Sex_chr,y = Chol_g_L))+
  geom_violin() 

ggplot(hd_data,mapping = aes(x = Sex_chr, y = Chol_g_L, fill = Sex_chr),) +
  geom_violin(width = 0.6, outlier.shape = NA, alpha = 0.8) +
  
  #tilføj nogle punkter til box plottet, og "jitter" disse punkter så de ikke er oven på hinanden
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) + #geom_points= punkterne står på en vertikal linje
  
  #Skifte farver 
  scale_fill_manual(values =  c("Male"="#0073C2FF",
                                "Female"="#CD534CFF") 
  )+
  
  #Ændre navngivningen på akserne
  labs(
    x = "",
    y = "Serum Cholesterol level (g/L)"
  ) +
  #Baggrund tema: theme_classic() = hvid baggrund
  theme_classic() + 
  theme(
    #fjerne legend
    legend.position = "none",
    #akse titlerne bliver i fed
    axis.title = element_text(face = "bold"),
    
    #teksten på begge akser bliver i størrelse 12
    axis.text = element_text(size = 20)
  )

# 3) Viser violin plottet noget andet end boxplottet

# 4) Gem plots


