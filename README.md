# Vanligste navn
Denne siden inneholder kildekoden og data for å regne ut det vanligste navnet i Norge

Kildekoden heter `vanligste_kombinasjon_navn.R`

Alle data er hentet fra ssb.no

Det er tre input-filer
- `fornavn_gutter.csv` gir antall i befolkningen i 2020 som har et bestemt mannlig fornavn
- `fornavn_jenter.csv` gir antall i befolkningen i 2020 som har et bestemt kvinnlig fornavn
- `etternavn.csv` gir antall i befolkningen som har et etternavn, som de deler med minst 200 andre i befolkningen i 2020

Skriptet gir følgende output:
- `vanligste_fornavn.jpeg` gir graf over de vanligste fornavnene i Norge
- `vanligste_etternavn.jpeg` gir graf over de vanligste etternavnene i Norge
- `vanligst_navnkomb.jpeg` gir graf over vanligste navnekombinasjon i Norge.

Skriptet baserer seg på uniform sannsynlighetsmodell. Siden navnekombinasjoner er betinget (personer fra bestemte sosiale miljøer har høyere sannsynlighet for visse navnekombinasjoner enn andre), gir utregningene en riv ruskende gal sannsynlighet for navnekombinasjonene. Med andre ord: ikke bruk denne framgangsmåten til noe viktig. Men du kan la deg inspirere hvordan noe kan visualiseres.
