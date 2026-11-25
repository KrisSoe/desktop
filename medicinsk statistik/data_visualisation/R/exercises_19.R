19.1
#Data visualisering om data vedr. højde og vægt.

#1A: Import the Height dataset into R by inserting the following in the console

height_data <- medisr::height_data

#2A:View the dataset.

#View(height_data) i konsollen, åbner et nyt vindue.

#How many observations do we have? - 544
#How many variables? - 4
#What type are they? - height, weight and age are numeric while gender is categoric

#3A:Are we happy with going strait to plotting or
#do we need to wrangle the dataset a bit with dplyr first?

#Male is obviously not continuous but rather a yes/no factor.
#So we should consider this later before comparing males and females.

#1B:The variable male is coded as 0 and 1.
#Before plotting, convert it into a factor with meaningful labels:

#Insert the following in the console:

height_data$male <- factor(
  height_data$male,
  levels = c(0, 1),
  labels = c("Female", "Male")
)

#And then insert the following to see if it works:

height_data$male

#To change the name of the x-axis, the data set must be loaded again
#and then insert the following

height_data <- medisr::height_data

height_data$sex <- factor(
  height_data$male,
  levels = c(0, 1),
  labels = c("Female", "Male")
)

height_data$sex

#2B:Create a plot that proofs to yourself that the median age of
#males and females are similar within the dataset.

#What will you put on the x-axis? - Gender
#What will you put on the y-axis? - Age
#Any color coding? - Not significant
#Which geom_? - boxplot

height_data |>
  ggplot(aes(x=sex,y=age,color=sex)) +
  geom_boxplot()

#3B:Investigate whether the relationship between height and weight
#appears to be linear.

#Create a scatter plot with:

#What will you put on the x-axis? - Height
#What will you put on the y-axis? - Weight
#Any color coding? - Not significant
#Which geom_? - geom_point()

#First thought was to load the dataset again, but it is not nescesarry.
height_data |>
  ggplot(aes(x=height,y=weight,color=height)) +
  geom_point()

#prøv evt. at flippe akserne

height_data |>
  ggplot(aes(x=height,y=weight,color=height)) +
  geom_point() +
  coord_flip()

#det giver ikke lige så god mening

