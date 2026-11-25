########################
### Lecture 6 part 2 ###
########################
#Data manipulation og plot

###############
### Library ###
###############
library(tidyverse)
library(readr)
library(ggplot)


######################
### Specifications ###
######################
height <- 1680
width <- 1920
res <- 300

#################
### Load data ###
#################
#indlæs lunge cancer datasættet fra forrige lektion
Lung_cancer_df <- readRDS(file = "GeneratedData/Lung_cancer_df.rds")

#################
### Table one ###
#################
#Table one
library(tableone)
?CreateTableOne()
#Main function, create a table 1 or summary table
CreateTableOne(data = Lung_cancer_df)


#question: does this table present the data to a sufficient degree? Nej, der er mange binære værdier hvortil gennemsnittet ikke giver mening


#We then manually writes the variables that the table should contain
myvars <- c("age","ph.ecog","ph.karno","pat.karno","meal.cal","sex_chr","wt_loss_kilo","ECOG")
CreateTableOne(data = Lung_cancer_df,
               vars = myvars)
#Why is "ph.ecog" presented as an average? where as the "ECOG" variable is shown as a categorical?
#some are categorical some are continous.


#Lets define the categorical variables
cat_vars <- c("ph.ecog","sex_chr","ECOG")

tab1 <- CreateTableOne(data = Lung_cancer_df,
                       vars = myvars,
                       factorVars = cat_vars) #Why does it not show all categories of the "ECOG" variable? and it only shows "males" as well?
tab1
#We can use the print.TableOne() function 
?print.TableOne #look at "showAllLevels"
print(tab1,showAllLevels = T)

#The print functions know that we want to use the print.Tableone function, it is very smart..
print(tab1,
      showAllLevels = T) #Shows all levels for factors that has more than two levels.
print(tab1,
      cramVars = c("ECOG","sex_chr"))#Works if we only want to show some 2-factor levels


#We want some of the continuous variables to use median instead of mean
#Lets define what varaibles we want
Median_var <- c("age","meal.cal","wt_loss_kilo")

#Now we have some with mean and some variables with median and some are presented as factors
print(tab1,showAllLevels = T,
      nonnormal = Median_var,
      test = F)

#range=Min-max instead of IQR= Interquantile range.
print(tab1,
      showAllLevels = T,
      nonnormal =Median_var,
      minMax = T)

#The create table one functions works nicely with html
#There are alot of methods to output a nice table
library(htmlTable)
library(gridExtra)
tab1 <- CreateTableOne(data = Lung_cancer_df,
                       vars = myvars,
                       factorVars = cat_vars,
                       strata = "sex_chr") #Stratefied table on sex 

tab1_final <- print(tab1,
                    showAllLevels = T,
                    nonnormal =Median_var,
                    minMax = T,
                    missing = T)# It shows the percentage of missing, but i want the number of patients with missing instead
#I therefore update the variable "ECOG" to now treat "NA" as a separate category called "Missing"
Lung_cancer_df$ECOG <- ifelse( is.na(Lung_cancer_df$ECOG),
                               yes="Missing",
                               no= Lung_cancer_df$ECOG)

grid.table(tab1_final)
htmlTable(tab1_final)

#Saving your  table as a CSV file
write.csv(tab1_final,
          file = "./Results/Tables/table_final.csv")


### Opgaver
#Gennemgang af opgaver og datasættet
###########################
### Information om data ###
###########################
#https://danmazjen.github.io/medicinsk-statistik/exercises/data.html
#Information omkring datasættet kan finde i sektion 17.3 (Cleveland Hear Disease)


