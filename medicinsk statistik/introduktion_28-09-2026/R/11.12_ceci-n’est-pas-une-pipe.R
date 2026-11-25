#piper er smarte når ens data bliver mere kompliceret, således at hver værdi eller funktion har hver deres linje i stedet for at skrive det hele ud i én linje
values<-c(1:10)
mean(values)
sqrt(values)

sqrt(mean(values)) #dette kan være rigtig forvirrende på sigt, derfor bruger man piper som nedenstående.

values |> #piperne giver samme resultat som ovenstående, men det er lettere at gennemskue hvilke funktioner der udføres og i hvilken rækkefølge
  mean() |> 
  sqrt()
