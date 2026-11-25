Statistisk og stoatiske variabler

Definitionen på sandsynlighed

SumP(X=j)=1


Bayes formel

P(A,B) = P(A|B)*P(B) = P(B|A)*P(A)

Kan også se sådan ud

P(A|B) = (P(B|A)*P(A))/P(B)

Eksempel med covid og deres PCR tests:

P(pos. test |inficeret) = 0.8

P(neg. test | uinficeret) = 0,95

P(inficeret | pos. test) = ?

P(inficeret | pos. test) = P(pos. test | inficeret) * P(inficeret) / P(pos. test)

P(inficeret) = 0,01 (lav infektionsrate)

P(pos.test) = P(pos.test , infeceret) + P(pos. test, uinficeret)

P(pos.test) = 0,8 * 0,01 + 0.05 * 0.99

P(inficeret | pos. test) = P(pos. test | inficeret) * P(inficeret) / P(pos. test)

P(inficeret | pos. test) = 0.8 * 0.01 / pos.test

Resultatet bør sige, at der er en lav risiko for at man er inficeret, selvom man har en positiv test, hvis infektionsraten er lav.

A = positiv test ja/nej
B = inficeret ja/nej

A |\_ (afhængig af) B

Ved afhængighed gælder:
P(A,B)=P(A|B)P(B)

Kan bliver uafhængig hvis man pålægger en 3. variabel (ubetinget afhængighed)
P(A,B|H)=P(A|H)P(B|H)

Hvis man har en gruppe patienter, der får UV stråling, udvikler celleskade og dernæst cancer.
UV er en variabel (A)
celleskade er en variabel (H)
cancer er en variabel (B)

Ser man kun på UV og cancer vil variablerne være afhængige.
Betinger man på celleskade bliver variablerne uafhængige, da man ikke nødvendigvis har cancer,
selvom man har celleskade f.eks.

Man kan se på et andet eksempel
Patienter med diabetes 2 har en tendens til at udvikle retino pati og dårlig vaskulation.
Retino pati er en variabel (A)
Diabetes 2 er en variabel (H)
Dårlig vaskulation er en variabel (B)

Generelt er retino pati og dårlig vaskulation afhængige af hinanden. Men hvis man betinger ved
diabetes 2, bliver de uafhængige generelt.

A = kast med terning 1
B = kast med tering 2

A ||_ (uafhængig af) B

Ved uafhængighed gælder:
P(A,B)=P(A)P(B)

og

P(A|B)=P(A)

Kan også blive afhængig hvis man pålægger en 3. variabel (betinget afhængighed)
P(A,B|H)=P(A|B,H)P(B|H)

Dette kan ses hvis man ser på 2 sygdomme som behandles med samme medikament.
Omvendt korrelation?
Hvis en patient har sygdom 1 og de bliver behandlet med medikamentet er der en lavere
sandsynlighed for, at de har eller udvikler sygdom 2. Variablerne bliver afhængige af hinanden
hvor sygdommene før var fuldstændig uafhængige.

Hvis man fører et studie, og man ser på, hvem der dropper ud. Man kan kigge på variablerne
intervention og alder >60 år er de 2 variabler i sig selv uafhængige. Men når man kigger på
hvem der dropper ud, er de pludseligt afhængige, da man ser, at disse 2 variabler er årsag til
at personerne dropper ud af studiet. Basal forståelse til at forstå kausale sammenhænge.

Når man betinger på diabetes 2 bliver det den begrænsende faktor. Når man betinger på drop ud raten, bliver det den begrænsende faktor.

Estimater:

Estimanten af en slå en sekser med et terningekast er 1/6

Hvis man kaster med en terning n antal gange
{X_j}_j=1^n

Dette kan man tage summen af
n/pi = 1/n nsumj=1 x_j

n/pi = estimatet
1/n nsumj=1 x_j = estimatoren

pi = 1/6 - estimanden man prøver at finde

Resultatet bør tilnærme sig estimanden. Jo flere gange man slår med terningen
desto tættere når man estimanden og eventuelle ekstremer eller afvigelser vil have mindre betydning.

Dette gør sig gældende ved det store tals sætning (ikke pensum at kunne men godt at forstå)

ppois(130, lambda = 133.5962)
