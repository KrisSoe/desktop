#Data til casen:
data <- medisr::mabp
data <- tibble::tibble(data)

#Data dokumentation
?medisr::mabp

#kig på data


library(lubridate)
library(dplyr)
library(ggplot2)
library(medisr)
library(VGAM)


data <- medisr::mabp
data <- tibble::tibble(data) #gør det mere praktisk at arbejde med tabeller i R


#Der tilføres kolonner som kigger på og definerer de variabler der er relevante.
data <- data %>% #%>% betyder "send resultatet videre til næste funktion"
  mutate( #mutate() bruges til at oprette nye kolonner i et datasæt.
    below70mmHg_cum = rowSums(70 > select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE), #Her vælges alle kolonner, der starter med "map_".Derefter undersøges for hver række om værdien er over eller under 70. Hvis den er under 70 tildeles en TRUE værdi og over 70 tildeles en FALSE værdi.rowSums() tæller antallet af TRUE-værdier.
    above100mmHg_cum = rowSums(100 < select(., starts_with("map_", ignore.case = FALSE)),na.rm = TRUE), #Hvis den er under 100 tildeles en FALSE værdi og over 100 tildeles en TRUE værdi.rowSums() tæller antallet af TRUE-værdier.
    below70mmHg = 2 < below70mmHg_cum, #below70mmHg_cum = Antal MAP-målinger under 70 mmHg og below70mmHg = TRUE hvis der er mere end 2 målinger under 70
    above100mmHg = 2 < above100mmHg_cum, #above100mmHg_cum = Antal MAP-målinger over 100 mmHg og above100mmHg = TRUE hvis der er mere end 2 målinger over 100
    outs70100mmHg = 2 < above100mmHg_cum | 2 < below70mmHg_cum, #outs70100mmHg = TRUE hvis der er mere end 2 målinger uden for området og above100mmHg_cum = Antal MAP-målinger over 100 mmHg og below70mmHg_cum = Antal MAP-målinger under 70 mmHg
    good_functional_outcome = mrs_3m < 3 #good_functional_outcome = TRUE hvis mRS efter 3 måneder er mindre end 3
  )

