#########################
### Opgaver lecture 6 ###
#########################

###########################
### Information om data ###
###########################
#https://danmazjen.github.io/medicinsk-statistik/exercises/data.html
#Information omkring datasættet kan finde i sektion 17.3 (Cleveland Hear Disease)

###############
### Opgaver ###
###############

### Opagve 1: projekt setup ---- 
# 1) Lav en projekt mappe og folder (RawData, GeneratedData, Scripts, Results etc)   
# 2) Opret et R-projekt
# 3) Download hd_data.csv dataene og læg dem i "RawData" folderen
# 4) Indlæs hd_data.csv i R

### Opgave 2: Data cleaning 
# 1) Mange af variablene i datasættet er skrevet som numeriske værdier e.g. sex=0/1. Lav nogle nye character variable, e.g. sex_chr=Male/Female
# 2) Variablen "HD" tager værdierne "Yes" eller "no", lav denne som en ny character variabel e.g. Yes=HDP eller no=NHD.     
# 3) variablen "Chestpain", "Thal", er skrevet med små bogstaver, lav en ny varibel med stort start bogstav.
# 4) Chol er angivet i mg/dL udregn denne som g/L
# 5) Gem de cleanede data some en ".RDS" fil i GeneratedData mappen
# Ekstra opgaver
# 1) Er der en tydelig alders forskel mellem mænd og kvinder?
# 2) Hvad er gennemsnits alderen på de forskellige typer af "ChestPain"


### Opgave 3: Table 1 ----
# 0) Lav et nyt script kaldet e.g "tabel1" og gem dette script i "scripts" folderen
# 1) Lav en tabel 1 som beskriver populationen
  # 1.0) Udvælg hvilke variable som skal med i tabellen
  # 1.1) Udvælg hvilke varible som skal angive med Median værdier
  # 1.2) Vælg hvilke variable der er categorisk  
  # 1.3) Angiv median værdierne med min-Max 
  # 1.4) Få tabellen til at vise alle grupper

# 2) Lav en Tabel 1 strateficeret på Køn
# 3) Lav en Tabel 1 strateficeret på Heart disease status
# 4) Ser i nogle forskelle mellem grupperne? (Diskuter dette)
# 5) Gem tabellerne som en Csv fil i Results mappen


### Opgave 4: Plots ----
# 0) Lav et nyt script
# 1) Udvælg en variabel som i fandt interresant fra opgave 3.4 (e.g. "Chol")
# 1.1) Lav et boxplot for denne varibel og sammenligner mænd og kvinder 
# 1.2) (Ekstra opgave) Lav et flot plot og gå i dybden med nogle af funktionerne fra lecture 6.1 
# 2) Lav et violin plot 
# 3) Viser violin plottet noget andet end boxplottet (diskussiones opgave)
# 4) Gem plotsene

