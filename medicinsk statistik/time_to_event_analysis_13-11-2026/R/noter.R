library(tidyverse)
library(survival)
library(patchwork)
library(broom)
library(survminer)
library(forestmodel)
library(haven)
library(reactable)

#Når man laver time to event plots er man ikke interesseret i kalender tid men i stedet tid observeret.
#Dertil vil man også rangere personerne  i rækkefølge ift. hvor længe de observeres.
#Efterfølgende laver man en trappe kurve, som viser overlevelseskurven. Den giver sandsynligheden for,
#om en patient vil overleve en specifik tidsperiode. Kurven på slide 9, viser, at 100% overlever med stor
#sandsynlighed i 2 år, men kun 40% overlever 14 år.

#Censorering - man ved ikke, hvad der sker i fremtiden. Derfor vil man censorere de patienter som ikke har haft event
#Man kan ikke ignorere dem, fordi så overestimerer man overlevelse, men tæller man dem som døde vil man underestimere overlevelse
#Hvad gør man så?
#Slide 13 - kaplan meier estimatoren, der er en funktion på det slide

#typer af censorering
#- administrativ censorering (højre censorering) er når man vælger en dato hvor man kigger på sit data og censorerer dem som ikke er døde
#- hvis man flytter ud af landet, ved man ikke, om patienterne dør, så disse vil censoreres, særligt hvis eventet sker før den administrative censorering
#- venstre censorering - lige gyldigt, tror det er noget med, at man ikke ved, hvornår pt blev diagnosticeret
#- interval-censorering, time of event er ukendt, men man ved det sker inden for 2 specifikker punkter i tid.

#husk at hatten betyder at noget er estimeret

#Brug af Kaplan-Meier estimatoren

#Først vises lige en simpel funktion

bmi <- function(height, weight) {
  weight/height^2
}

bmi(height = 1.80, weight = 60) #[1] 18.51852

50:70 #betyder intervallet fra 50-70

bmi(height = 1.80, weight = 60:80) #giver alle bmi værdier i vægt intervallet 60-80 (hele tal)

#[1] 18.51852 18.82716 19.13580 19.44444 19.75309 20.06173 20.37037
#[8] 20.67901 20.98765 21.29630 21.60494 21.91358 22.22222 22.53086
#[15] 22.83951 23.14815 23.45679 23.76543 24.07407 24.38272 24.69136

#indhenter integreret datasæt
lung <- lung

lung |>
  select(time,status) |>
  arrange(time) |> #arrangerer patienterne efter hvem der først havde event og hvem der sidst have event
  group_by(time) |>
  summarize(pt=n(),
            d=sum(status==2)) |> #kun unikke tidspunkter, d=sum synliggører de censorerede
  mutate(n=nrow(lung)-cumsum(pt)+pt,
         estimate = cumprod(1-d/n)) |> #den akkumolerede sum af pt, fratrukke antallet pt?, estimate er kaplan-meier funktionen og laver en kolonne, som viser overlevelsesraten
  view()

KM <- function(data,time,status){
  data |>
    select(time,status) |>
    arrange(time) |>
    group_by(time) |>
    summarize(pt=n(),
              d=sum(status==2)) |>
    mutate(n=nrow(lung)-cumsum(pt)+pt,
           estimate = cumprod(1-d/n)) |>
    view()
}

KM(data=lung, time=time, status=status)

survfit(Surv(time,status) ~1, data = lung)

#Call: survfit(formula = Surv(time, status) ~ 1, data = lung)
#
#       n events median 0.95LCL 0.95UCL
#[1,] 228    165    310     285     363

survfit(Surv(time,status) ~sex, data = lung)

#Call: survfit(formula = Surv(time, status) ~ sex, data = lung)
#
#n events median 0.95LCL 0.95UCL
#sex=1 138    112    270     212     310
#sex=2  90     53    426     348     550

fit <- survfit(Surv(time,status) ~ 1, data = lung)
summary(fit, times =c(10,100,500))

#Call: survfit(formula = Surv(time, status) ~ 1, data = lung)
#
#time n.risk n.event survival std.err lower 95% CI upper 95% CI
#10    227       1    0.996 0.00438        0.987        1.000
#100    196      30    0.864 0.02271        0.821        0.910
#500     41     107    0.293 0.03508        0.232        0.371

survfit(Surv(time,status) ~1, data = lung) |>summary() #viser alt data

survfit(Surv(time,status) ~1, data = lung) |> plot() #viser plottet naturligvis

survfit(Surv(time,status) ~1, data = lung) |>
  tidy() |> ggplot(aes(time, estimate)) +
  geom_step

#slide 22, gode standarder for KM kurver, prøv at kør koden når slidesene kommer ud
#slide 23 klassiske endepunkter, man kan undersøge på, evt. definer disse enkelt endepunkter, for de er ikke nødvendivis intuitive



nrow(lung) #[1] 228, tæller observationer

#    time status
#1    306      2
#2    455      2
#3   1010      1
#4    210      2
#5    883      2
#6   1022      1
#7    310      2
#8    361      2
#...
#228  177      1

#slide 28 download data fra project data sphere og prøv at køre koden

#slide 35 hvorfor er pfs kurven stejlere end os? fordi pfs kurver viser stats for både død og forværring af sygdom hvor os kun viser død
# hvorfor er pfs meget tydeligt trappeformet? fordi kontrollerne er prædefinerede (lidt et eksempel på interval censorering)

#man må og kan ikke betinge på fremtiden, immortal time bias?

#Hazard funktioner
#rate of failure - sandsynligheden for at dø, eller hvor hurtigt man dør
#cumulative hazard function - integralet for hazard funktionen og er direkte overførbar til survival function

#slide 48 hvordan tolker man en kontinuær coeffficient - alder f.eks. når coefficienten er 1 betyder det at der for hvert år er 1% højere hazard
#artikel hazard og hazard ratios
