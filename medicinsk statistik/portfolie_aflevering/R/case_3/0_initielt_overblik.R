#Case 3: Forecasting number of COVID-19 related deaths 4 weeks ahead using an autocorrelation model

###Objective###
#The study aims to train and test a prediction model for forecasting the number of
#COVID-19 related deaths 4 weeks into the future.

###Data###
#Data can be loaded into R using the following code:
data <- medisr::covid_data
data <- tibble::tibble(data)

#Data documentation is found by executing the following code:
  ?medisr::covid_data

###Outcome###
#Number of COVID-19 related deaths.

###Workshop 2###
#Divide the data into a training set and a test set based on geography, ensuring that all
#observations from a given country are assigned to the same dataset. Use the training
#data to train a linear prediction model that predicts number of COVID-19 related deaths
#four weeks ahead of baseline. Use appropriate techniques to avoid overfitting.

###Further work in upcoming lectures##
#Validate the model developed in workshop 2 using the test dataset.
