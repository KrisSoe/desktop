#Case 2: Translating the PEmb-QoL questionnaire to quality of life measured by EQ-5D-5L

###Objective###
#The study aims to train and test a prediction model for translating information from the
#PEmb-QoL questionnaire to the total EQ-5D-5L summary score.

###Data###
#The data have been divided into training and test data based on time of response. Test
#and training data can be loaded into R using the following code:

data_train <- medisr::eq5d_train
data_train <- tibble::tibble(data_train)
data_test <- medisr::eq5d_test
data_test <- tibble::tibble(data_test)

#Data documentation is found by executing the following code:
  ?medisr::eq5d

#Both data frames contain the following variables:

#eq5d
#Total EQ-5D-5L summary score.

#AL
#Daily activity limitations domain of the PEmb-QoL questionnaire, reduced to a summary score of 7 items.

#EC
#Emotional complaints domain of the PEmb-QoL questionnaire, reduced to a summary score of 9 items.

#IC
#Intensity of complaints domain of the PEmb-QoL questionnaire.

#age
#Age in years.

#sex
#Sex (0 = Female, 1 = Male).

#eq5d_train is the training set and eq5d_test is the test set. The data are split by time.

#The dataset is a subset of the variables from the original study data. The values have been augmented with noise for use in methodological applications.

###Outcome###
#Quality of life measured by EQ-5D-5L

###Prediction model###
#Linear regression model

###Potential predictors###
#The data contains age and sex as well as the daily activity limitations, emotional
#complaints, and intensity of complaints domain of the PEmb-QoL questionnaire.
#Second order interaction terms that include sex could also be investigated as potential
#predictors.

###Workshop 2###
#Use the training data to train a linear prediction model that predicts quality of life
#measured by EQ-5D-5L. Use appropriate techniques to avoid overfitting.

###Further work in upcoming lectures###
#Validate the model developed in workshop 2 using the test dataset. Discuss the test
#retest reliability of the EQ-5D-5L in comparisons to the results of the validation.
