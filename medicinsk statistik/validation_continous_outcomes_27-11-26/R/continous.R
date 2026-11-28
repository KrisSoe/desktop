library(blandr)
library(psychTools)
library(dplyr)
library(ggplot2)

?epi.bfi

data <- tibble::tibble(psychTools::epi.bfi)
data <- data[complete.cases(data),]

set.seed(123)

# Random split: 80% train, 20% test
idx <- sample(nrow(data), size = floor(0.8 * nrow(data)))
data_train <- data[idx, ]
data_test  <- data[-idx, ]

N <- nrow(data_train)
limits <- range(data$traitanx)

full_model <- lm(traitanx ~ epiS + epiImp + epilie + epiNeur + bfagree + bfcon + bfext + bfneur + bfopen,data=data_train)
null_model <- lm(traitanx ~ 1,data=data_train)
forward_model <- step(null_model,
                      scope = list(lower = null_model, upper = full_model),
                      direction = "forward",k=log(N))


predictions <- predict(forward_model, newdata = data_test, type = "response")
sqrt(mean((data_test$traitanx - predictions)^2))

plot(predictions,data_test$traitanx,xlim=limits,ylim=limits)
abline(a=0,b=1)

stats <- blandr.statistics(data_test$traitanx, predictions)
blandr.plot.ggplot(stats)
