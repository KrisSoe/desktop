#18.2.1 A: Orientering i datasættet

#1A: importér Cleveland Heart Disease datasættet til R

hd_data <- medisr::hd_data

view(hd_data)

#hvad er navnet på datasættet?

hd_data

#hvor mange rækker og kolonner har det?

303 rækker, 15 kolonner

#hvor mange variabler?

15

#hvor mange deltagere?

303

#ses der et mønster?

obviously


#2A: kig på datasættet

#list 3 variabler du tror er numeriske
kunne være alder, restBP, maxHR og chol

#list 2 variabler du tror er kategoriske
køn, cheastpain, fbs?, restECG, ExAng?, Slope?, Ca?, Thal?, HD (yes/no)

#why does this distinction matter when calculating a mean?
Man kan kun beregne en middelværdi på numeriske data. Til kategoriske data vil man oftere benytte sig af medianer (den midterste observation) eller typetal (den kategori der optræder oftest).

#3A: What happens when you take the mean of a character string? 
mean("hi")

Advarselsbesked:
  I mean.default("hi") :
  argument er ikke numerisk eller logisk: returnerer NA

#error to protect you — don’t get scared from the red text warning 😄

#4A:Extract one variable (e.g., Age or Cholesterol)
age<-hd_data$Age

#Print it to see all it’s values
print(age)

#Calculate its mean
mean(age) -> 54.43894

library(dplyr)

hd_data |> 
  pull(Age) |> 
  mean()

#18.2.2 1B:Calculate the mean and standard deviation for:

Age
hd_data |> 
  pull(Age) |> 
  mean()

hd_data |> 
  pull(Age) |> 
  sd()

Chol
hd_data |> 
  pull(Chol) |> 
  mean()

hd_data |> 
  pull(Chol) |> 
  sd()

RestBP
hd_data |> 
  pull(RestBP) |> 
  mean()

hd_data |> 
  pull(RestBP) |> 
  sd()

MaxHR
hd_data |> 
  pull(MaxHR) |> 
  mean()

hd_data |> 
  pull(MaxHR) |> 
  sd()

#2B Check if there are missing values in the dataset

#How many missing values and for which variables?
is.na(hd_data) #udspytter en stor mængde uoverskuelig data

sum(is.na(hd_data)) #fortæller hvor mange felter der er i datasættet som mangler en værdi

colSums(is.na(hd_data)) #fortæller hvor værdierne mangler, i dette tilfælde mangler der 4 i kolonnen "Ca" og 2 i kolonnen "Thal"

Man kan også se, hvor der mangler værdier henne, når man går ind og kigger på datasæt fanen. 

#What happens if you try to calculate the mean when missing values are present?

Vi kan prøve at kalkulere middelværdien for Ca (selvom det ikke giver mening statistisk, da det er en katagorisk værdi)

hd_data |> 
  pull(Ca) |> 
  mean()

[1] NA #man får dette output

#How do you fix the issue above?

hd_data |> 
  pull(Ca) |> 
  mean(na.rm=TRUE) #man skriver na.rm=TRUE i det felt man vil kalkulere. Dette fjerner manglende værdier.

[1] 0.6722408 #output fra ovenstående

#3B: List all the variable types the mean() function can take the mean of. There should be 5. Seek help inside RStudio.
 ?mean()

fra ?mean(): "Currently there are methods for numeric(1)/logical(2) vectors and date(3), date-time(4) and time interval(5) objects."

#18.2.3 1C: Consider the age mean and standard deviation you previously calculated (in exercise 1B). How can you interpret this? What can we say from this?

Age mean and standard deviation from 1B:
  
  Age
hd_data |> 
  pull(Age) |> 
  mean() #[1] 54.43894

hd_data |> 
  pull(Age) |> 
  sd() #[1] 9.038662

(mean) fortæller noget om gennemsnitsalderen for de deltagerne
(sd) fortæller om hvor langt den højeste og laveste alder ligger fra dette gennemsnit i 68% (1 SD) af observationerne

#2C: A colleague suggested calculating the mean of Sex.

hd_data |> 
  pull(Sex) |> 
  mean() #[1] 0.679868

#What would that number represent?
Mand/kvinde er repræsenteret som enten 0 eller 1, og derfor kan man få et nummer som output.
Men man kan ikke være 0,68 mand eller kvinde. Når der kun er 2 variabler, kan det dog godt sige noget om, om der er flest af det ene eller andet køn.

#Is it meaningful? Why or why not?
Definitivt er det procentandelen af det ene køn, i dette tilfælde mænd. Det vil sige, at der blandt undersøgelsens deltagere er 68% mænd. 
Ved ikke hvordan man ved, hvad der er mænd og hvad der er kvinder.

#3C: Why is it important to report both mean and standard deviation, and not just the mean? Think in a clinical context.
Standard afvigelsen siger noget og variabiliteten af dataene. Der kunne være signifikante outliers ift. gennemsnittet, og disse tages højde for ved inddragelse af standard afvigelse.
