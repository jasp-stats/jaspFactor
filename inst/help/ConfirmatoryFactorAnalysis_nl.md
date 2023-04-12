## Confirmatieve Factoranalyse
==========================

Confirmatieve Factoranalyse (CFA) modelleert geobserveerde variabelen (indicatoren) als rumoerige manifestaties van onderliggende latente variabelen (factoren). JASP's CFA is gemaakt met `lavaan` (lavaan.org; Rosseel, 2012), een `R` package voor structural equation modeling. Zie Brown (2014) of Kline (2015) voor boeken over het onderwerp, CFA.

### Invoerveld
-------
In het toewijzingsvak kunnen continue en ordinale variabelen in uw dataset worden toegewezen aan verschillende factoren. Er is een minimum van één factor, en elke factor moet minstens twee indicatoren hebben. U kunt factoren toevoegen door op de (+)-knop te drukken en factoren verwijderen door op de (-)-knop te drukken. U kunt factoren hernoemen door de naam boven de toewijzingsvakjes te veranderen. Zowel schaal- als ordinale variabelen zijn toegestaan. 

Indien u ordinale variabelen gebruikt, is de gekozen schatter standaard "DWLS" en worden de teststatistiek en de fit-maten geschaald en verschoven (gemiddelde en variantie aangepast, zie *lavaan* documentatie). Andere mogelijke schatters zijn "WLS" en "ULS". Wanneer u ordinale variabelen gebruikt, kan het zinvol zijn robuuste standaardfouten te kiezen.

### Tweede-orde factor
-------
JASP staat toe dat factoren op hun beurt indicatoren worden van een tweede-orde factor. Dit kan gespecificeerd worden door de factornamen naar het tweede orde invoerveld te slepen. Alle factoren die geen indicatoren van de tweede-orde factor zijn, kunnen covariëren met elkaar en met de tweede-orde factor, maar niet met de indicatoren van de tweede-orde factor.

### Model opties
-------
- Voeg gemiddelde structuur toe: Toon gemiddeldes van de indicatoren en, in the geval van multi-groep CFA, de gemiddeldes van de latente variabelen.
- Neem ongecorreleerde factoren aan: Zet de correlatie tussen verschillende latente variabelen op 0.
- Factor schalen: Factoren kunnen op drie manier geschaald zijn:
  - Factor varianties (standaardoptie): De factor heeft een vaste variantie van 1.
  - Marker variabele: De factor heeft dezelfde schaal als zijn eerste indicator aangezien zijn factorlading 1 is.
  - Effecten coderen: Het gemiddelde van de factorlading is vastgezet op 1. Voor meer informatie over de interpretatie van effecten coderen, zie Veen (2018).
- Residu covarianties: Om ook covariantie toe te staan tussen indicatoren die niet verklaard worden door hun respectieve factor, bijvoorbeeld omdat vragen in een vergelijkbare manier verwoord zijn, sleep twee variabelen naar het rechter invoerveld. 

### Aanvullende uitvoer
-------
- Extra pasmaten: Kies deze om de waarde van verschillende model pasmaten te tonen in de resultaten.
- Kaiser-Meyer-Olkin (KMO) test: Toon Kaiser-Meyer-Olkin (KMO) test resultaten voor steekproef geschiktheid (MSA).
- Bartlett's test voor sfericiteit: Toon Bartlett's test voor sfericiteit.
- R-Squared: het deel van de variantie in de indicatoren dat wordt verklaard door alle voorspellers (factoren)
- Gemiddelde variantie geëxtraheerd (AVE): een maat voor de hoeveelheid variantie die wordt vastgelegd door een construct in relatie tot de hoeveelheid variantie als gevolg van meetfouten
- Heterotrait-monotrait ratio (HTMT): maat voor de overeenkomst tussen latente variabelen, beoordeling van discriminantvaliditeit. Als de HTMT duidelijk kleiner is dan één, kan discriminantvaliditeit als vastgesteld worden beschouwd.
- Betrouwbaarheid: een maat voor de relatieve hoeveelheid testscorevariantie die wordt verklaard door de factor(en). De uitvoer bevat de coëfficiënt omega (McDonald's) voor elke indicatorgroep, in totaal, en geeft, als een factor van de tweede orde is opgegeven, omega_hiërarchisch. Biedt ook coëfficiënt alfa (Cronbach's).
- Geïmpliceerde covariantiematrix: Toon de covariantiematrix die het model impliceert.
- Residu covariantiematrix: Toon de covarianties tussen indicatoren die behouden blijft met het model. Een perfect model toont enkel 0-en hier. 
- Modificatie indices: Toont MIs met een minimum grens. Een MI toont hoeveel de chi-square waarde van de passing zou veranderen als de gespecificeerde parameter vrij zou zijn. EPC toont de verwachten verandering van de parameter zelf.
- Toon lavaan syntax: Toon de lavaan modeleer syntax die nodig zou zijn om het model in R weer te geven.

### Multigroep CFA
------
- Groepen: Selecteer hier een categorische variabele om CFA modellen voor iedere groep te creëren. 
- Invariantie testen: Selecteer een niveau van beperkende parameters over de verschillende groepen. 
  - Configureel: De verschilllende groepen hebben dezelfde CFA structuur.
  - Metrisch: De factorladingen van de groepen zijn gelijk.
  - Scalar: De factorladingen en gemiddeldes van de indicatoren van de groepen zijn gelijk.
  - Strikt: De factorladingen, gemiddeldes van de indicatoren, residu varianties, en residu covarianties van de groepen zijn gelijk.
  
### Grafieken
-------
- Misfit grafiek: Visualisatie van de residu correlaties (gestandaardiseerde residu covariantiematrix) van de indicatoren.
- Model grafiek: Visualisatie van de geschatte modelstructuur.

### Geavanceerd
-------
- Emulatie: Emuleer resultaten van verschillende software.
- Foutberekening: Wijzigt de manier waarop de standaardfout wordt berekend.
- Estimator: Wijzigt de estimator voor de CFA. (Auto: ML als alleen "scale" variabelen gebruikt worden, anders WLS)
- Standaardisatie: Toon gestandardiseerde parameters voor verschillende standaardisatieschema's.
- Behandeling van missende data: Wijzigt de methode waarmee missende data worden behandeld.

### Referenties
-------
- Brown, T. A. (2014). *Confirmatory factor analysis for applied research*. Guilford Press. 
- Henseler, J., Ringle, C. M. & Sarstedt, M. (2015). A new criterion for assessing discriminant validity in variance-based structural equation modeling. *Journal of the Academy of Marketing Science, 43*, 115–135 (2015). https://doi.org/10.1007/s11747-014-0403-8
- Kline, R. B. (2015). *Principles and practice of structural equation modeling* (4th ed.). Guilford Press.
- Rosseel, Y. (2012). lavaan: An R Package for Structural Equation Modeling. *Journal of Statistical Software, 48*(2), 1-36. https://doi.org/10.18637/jss.v048.i02

### R Packages
---
- ggplot2
- lavaan
- reshape2
- semPlot
- stats