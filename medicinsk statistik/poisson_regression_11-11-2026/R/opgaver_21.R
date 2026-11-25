###########################################
###21  Exercises: A fishy counting trick###
###########################################

#21.1.1 A: Let’s start off by considering when in general to use Poisson, logistic or normal regression
#Exercise 1A: Poisson vs. Binomial vs. Gaussian

#You are designing a study to analyze the number of hospital readmissions for heart failure patients within 30 days of discharge.
#1: Should you use a Poisson, Binomial, or Gaussian model? Justify your choice.
#
#2: What are the key assumptions of the distribution you chose? How would you check them?
#3: If the outcome were the probability of readmission (yes/no), which model would you use? Why?
#4: If the outcome were the length of stay (in days), which model would you use? Why?

#Exercise 2A: Reviewing a colleagues paper.

#Baggrundsinformation kan findes på hjemmesiden

#1: Identify all errors you can — you may ask for tips.

#21.1.2 B: Analyzing the historic “Doctors” dataset
#Exercise 1B: Approach the dataset naïvely by simply counting up the number of deaths for smokers and non-smokers.

#How many died?

#Exercise 2B:Check the balance in the groups.
#How many years in total did the researchers follow the smoking group and how many the non-smoking group?

#Exercise 3B: Finally, check whether the age groups are composed evenly and whether they are comparable between the smoking groups.

#1: Which group is nearly half of the participants between 35 to 44 years old?
#2: In the other group, how many of the age categories has higher percent distribution?

#Tip: For each group, compute the percent of all participants belonging to each of the five age groups and visually compare them one by one.

#Exercise 4B: Now, run the model naïvely, without accounting for these differences.
#Report the (incorrect) estimand for risk ratio for smoking vs. non-smoking.

#Exercise 5B:Let’s try to correct for these issues.
#Provide the scientific regression equation we will run. In other words: You should write, with pen an paper, the mathematical equation for the Poisson model we’ll fit.

#Exercise 6B:Now, we implement the mathematical equation from 5B.
#You ask a colleague to do it. Now, we receive the code and review it.
#Unfortunately, the R code contains two mistakes. Identify and fix them:'

m5b <- glm(deaths ~ smoking + age,
           family = poisson(link = "logit"),
           offset = personyears,
           data = doctors)

#21.1.3 C: Writing a Clinical Report
#Exercise 1C: The final model is provided in Exercise 6B. If you didn’t make that exercise, go ahead and see the solution and copy the code. You will not make inference based on the model.
#You will write a clincal report based on the inference we got from the model.

#1: How would you explain the incidence rate ratio (IRR) to a clinician who is not familiar with statistics?
#2: Write a 1-paragraph summary of the findings from your Poisson model for a clinical audience.
#3: Compare these results to the naïve model from Exercise 4B
#4: How would you visually present the effect of smoking on mortality in a figure for the report? Describe the plot.
#5: Now implement the plot using {ggplot2}.
#6: Make a legend to the plot so that others can read it with the intention you had.
