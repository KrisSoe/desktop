########################
### Lecture 6 part 1 ###
########################
#Data manipulation og plot

###############
### Library ###
###############
library(tidyverse)
library(survival)
library(ggplot2)
library(ggsci)

######################
### Specifications ###
######################
height <- 1680
width <- 1920
res <- 300

#################
### Load data ###
#################
#I "survival" pakken ligger der et lunge cancer datasæt
?cancer


#Vi laver vores egen definition
Lung_cancer_df <- readRDS(file = "RawData/Lung_cancer_df.rds")


#####################
### Data Cleaning ###
#####################
#Lad os lave de forskellige variable lidt mere læselige
Lung_cancer_df <- Lung_cancer_df %>% #dette symbol %>% forætller at dataene er binære
  mutate(
    #Sex: Male=1 og Female=2
    sex_chr=ifelse(sex==1,yes="Male",no="Female"), #kommaet er vigtigt, ellers ved den ikke hvornår den skal lave en ny søjle
    
    #wt.loss=pounds (konverter til kilo) : https://www.calculatorsoup.com/calculators/conversions/pounds-to-kilograms.php
    wt_loss_kilo = wt.loss * 0.4536,
    
    #Outcomes, 1=censored og 2=dead
    status_new=ifelse(status==1,yes=0,no=1), #by convention (in todays standards) 0=censored and 1=dead
    
    #Grupering af variable
    ECOG = case_when(
      ph.ecog==0 | ph.ecog==1~ "0-1", # "|"= or. is ph.ecog=0 "or" is ph.ecog =1 
      ph.ecog %in% c(2,3)~"2-3"),
     )

#######################
### Saving our data ###
#######################
#Gem vores data som et R-Data object 
saveRDS(object = Lung_cancer_df,file = "GeneratedData/Lung_cancer_df.rds")


###########################
### Stratified analysis ###
###########################
#Gennemsnitlig alder og vægt tab hos mænd
Lung_cancer_df %>% 
  filter(sex_chr=="Male") %>% #man kan tilføje værdier ved at skrive f.eks. "male | age>50" og den vil vise data på mænd og alle over 50. Eller man kunne skrive "male & age>50" og den vil finde data på de, der både er mænd og over 50.
  summarise(
    #Alder
    Average_age=mean(age),
    #Vægt tab        
    Average_wt_loss_kilo=mean(wt_loss_kilo ), # nu får vi en "NA" værdi
          
    #vægt tab nu uden missing value  
    Average_wt_loss_kilo=mean(wt_loss_kilo,na.rm = TRUE ) # na.rm=NA remove, 
            
            )

#På både mænd og kvinder
Lung_cancer_df %>% 
  group_by(sex_chr) %>% 
  summarise(
    #Alder
    Average_age=mean(age,na.rm=T),
    Median_age=median(age,na.rm=T),
    
    #Vægt tab
    Average_wt_loss_kilo=mean(wt_loss_kilo,na.rm = TRUE )
    
  )
# Det ser ud til at kvinderne er yngre og taber sig mindre end mændene

#####################
### Visualizing   ###
#####################
#Boxplot: visualisering a alder for mænd og kvinder 
ggplot(data = Lung_cancer_df,
       mapping = aes(x = sex_chr,y = age))+
  geom_boxplot() #very simple plot

#Lad os lave et pænt plot 
ggplot(data=Lung_cancer_df,
       mapping = aes(x = sex_chr, y = age, fill = sex_chr)) +
  geom_boxplot(width = 0.7, outlier.shape = NA, alpha = 0.8) +
  
  #tilføj nogle punkter til box plottet, og "jitter" disse punkter så de ikke er oven på hinanden
  geom_jitter(width = 0.05, alpha = 0.25, size = 1) + #geom_points= punkterne står på en vertikal linje
  
  #Skifte farver 
  scale_fill_manual(values =  c("Male"="#0073C2FF",
                                "Female"="#CD534CFF"))+ #JCO farver
                    
  #Ændre navngivningen på akserne
  labs(
    x = "Sex",
    y = "Age (years)"
  )+
  
  #Baggrund tema: theme_classic() = hvid baggrund
  theme_classic() + 
  theme(
    #fjerne legend
    legend.position = "none",
    #akse titlerne bliver i fed
    axis.title = element_text(face = "bold"),
    
    #teksten på begge akser bliver i størrelse 12
    axis.text = element_text(size = 12)
  )

dev.off()
#####################################
### Making a violin plots instead ### det her virker ikke !!!
#####################################
ggplot(data = Lung_cancer_df,
       mapping = aes(x = sex_chr,y = age))+
  geom_violin() #very simple plot


p2 <- ggplot(Lung_cancer_df,mapping = aes(x = sex_chr, y = age, fill = sex_chr),) +
  geom_violin(width = 0.6, alpha = 0.8) + #directly swithc the geom_boxplot() to geom_violin()
  
  #tilføj nogle punkter til box plottet, og "jitter" disse punkter så de ikke er oven på hinanden
  geom_jitter(width = 0.15, alpha = 0.25, size = 1) + #geom_points= punkterne står på en vertikal linje
  
  #Skifte farver 
  scale_fill_manual(values =  c("Male"="#0073C2FF",
                                "Female"="#CD534CFF") 
  )+
  
  #Ændre navngivningen på akserne
  labs(
    x = "Sex",
    y = "Age (years)"
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
p2

#Saving a plot
png(filename = "./Results/Figures/Violin_sex_Age.png")
p2
dev.off()

#############
### PAUSE ###
#############



 #noter
#Lad nogle OS tider, håndtering af dato variable osv. 
#






  












