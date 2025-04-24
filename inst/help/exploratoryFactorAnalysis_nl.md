Exploratieve Factoranalyse 
=== 

Met Exploratieve Factoranalyse kunt u één of meer onderliggende factoren van de data identificeren. De factoren zijn dusdanig gekozen dat zij gemeenschappelijke variantie dekken. 

### Assumpties (Yong & Pearce, 2013)
- De variabelen in de analyse zijn continu. 
- De data heeft een multivariate normale verdeling. 
- Er is een lineaire relatie tussen de variabelen en de factoren. 
- Er is geen multicollineariteit en singulariteit in de data. 

### Invoer 
---
#### Invoerveld 
- Ingevoegde Variabelen: In dit veld selecteert u de variabelen waarmee u de exploratieve factoranalyse uitvoert. 

#### Aantal Factoren
_NB: eigenwaarden voor EFA zijn anders dan eigenwaarden voor PCA. Meer informatie hierover is te vinden in Dinno (2014)._
- Hier specificeert u het aantal factoren waar de rotatie op wordt toegepast. Er zijn verschillende methoden om te bepalen hoe dit wordt gedaan:   
  - Parallel Analyse: Factoren worden gekozen op basis van parallel analyse. Met deze methode worden factoren geselecteerd met een eigenwaarde die hoger is dan de parallel gemiddelde willekeurige eigenwaarde. Dit is de standaardoptie. 
  - Eigenwaardes: Factoren worden gekozen indien zij een bepaalde eigenwaarde hebben. Als standaardoptie worden factoren met een eigenwaarde van 0 of hoger gekozen. Dit wordt het Kaiser criterium genoemd. 
  - Handmatig: Het aantal factor kan handmatig gespecificeerd worden. De standaardoptie is 1. 

#### Rotatie 
- De rotatiemethode die wordt toegepast op de factoren, kan hier gespecificeerd worden.
  - Orthogonaal: Deze methode produceert ongecorreleerde factoren. Diverse mogelijkheden zijn beschikbaar: 
      - Geen: Geen rotatiemethode wordt geselecteerd. 
      - varimax: Orthogonale rotatiemethode varimax. Deze rotatie is gebaseerd op het maximaliseren van de variantie van de ladingen. 
      - quartimax: Orthogonale rotatiemethode quartimax. Voor deze methode worden het aantal factoren die nodig zijn om iedere variabele te verklaren geminimaliseerd.
      - bentlerT: Orthogonale rotatiemethode bentlerT. 
      - equamax: Orthogonale rotatiemethode equamax. Dit is een combinatie van varimax en quartimax. 
      - varimin: Orthogonale rotatiemethode varimin. 
  - Oblique: Deze methode produceert factorendat correlaties tussen factoren toestaat. Dit is de standaardoptie. Diverse mogelijkheden zijn beschikbaar: 
      - promax: Oblique rotatiemethode promax. Dit is de standaardoptie. 
      - oblimin: Oblique rotatiemethode oblimin. 
      - simplimax: Oblique rotatiemethode simplimax. 
      - bentlerQ: Oblique rotatiemethode bentlerQ. 
      - biquartimin: Oblique rotatiemethode biquartimin. 
      - cluster: Oblique rotatiemethode cluster. 

#### Basis decompositie op
- Correlatie: Baseert de PCA op de correlatiematrix van de gegevens
- Covariantie: Baseert de PCA op de covariantiematrix van de gegevens
- Polychorisch/tetrachorisch: Baseert de PCA op de poly/tetrachorische (gemengde) correlatiematrix van de gegevens. Dit is soms onstabiel wanneer de steekproefomvang klein is en wanneer sommige variabelen niet alle antwoordcategorieën bevatten.

### Uitvoeropties 
- Markeer: Deze optie zet de waarde vanaf waar de paden schalen in breedte. Paden met absolute gewichten hoger dan deze waarde zullen steeds breder worden terwijl waardes eronder een vaste dunne breedte hebben. Alle paden krijgen een sterkere of zwakkere kleurintensiteit naarmate ze een sterker gewicht hebben. Als de waarde op 0 gezet wordt zullen alle paden een verschillende breedte krijgen.
- Voeg Tabellen Toe: 
    - Factorcorrelaties: Bij het selecteren van deze optie, wordt een tabel met de correlaties tussen factoren weergegeven. 
    - Aanvullende fit indices: Deze optie toont de wortel van de gemiddelde kwadraatsom fout (RMSEA) met 90% betrouwbaarheidsinterval, de Tucker Lewis Index (TLI), en de Bayesian Information Criterion (BIC) om de model fit te testen.
- Plots:
    - Paddiagram: Bij het selecteren van deze optie wordt een visuele representatie van de richting en de sterkte van de relatie tussen de variabele en de factor weergegeven. 
    - Screeplot: Bij het selecteren van deze optie, wordt een screeplot getoond. De screeplot geeft informatie over hoeveel variantie in de data, aangegeven door de eigenwaarde, wordt verklaard door elke factor. Een screeplot kan gebruikt worden om te beslissen over de hoeveelheid van de factoren. 
        - Resultaten van parallelle analyse: Toont de resultaten van de parallelle analyse in de scree plot. De parallelle analyse wordt gebaseerd op PC of FA, zoals bepaald door de optie voor de parallelle analysetabel.
- Aannamecontroles:
   - Kaiser-Meyer-Olkin Test (KMO): Bepaalt hoe goed variabelen geschikt zijn voor factoranalyse door het aandeel gemeenschappelijke variantie tussen variabelen te berekenen.
   - Bartlett's Test (van sfericiteit): Bepaalt of de correlatiematrix van de gegevens de identiteitsmatrix is, d.w.z. of de variabelen aan elkaar gerelateerd zijn of niet.
   - Mardia's test van multivariate normaliteit: Beoordeelt de mate van afwijking van de multivariate normaliteit van de opgenomen variabelen in termen van multivariate scheefheid en kurtose. De Mardia's test omvat altijd de lijstvolledige gevallen.
- Ontbrekende waarden: 
    - Sluit paarsgewijs uit: Indien 1 observatie van een variabele mist, worden de observaties van de andere variabelen nog wel gebruikt voor de analyse. In dit scenario is het niet nodig om voor elke variabele een observatie te hebben. Dit is de standaardoptie. 
    - Sluit lijstgewijs uit: Indien 1 observatie van een variabele mist, wordt de gehele casus (dus alle andere variabelen van dezelfde casus) uitgesloten voor analyse. In dit scenario zijn is voor elke variabele een observatie nodig. 

### Uitvoer 
--- 
#### Aannamecontroles
- Kaiser-Meyer-Olkin Test (KMO): Maat voor steekproeftoereikendheid (MSA) als het aandeel van de gemeenschappelijke variantie tussen variabelen wordt berekend voor alle variabelen; waarden dichter bij 1 zijn gewenst.
- Bartlett's Test (of sphericity): Een significant resultaat betekent dat de correlatiematrix afwijkt van de identiteitsmatrix.
- Mardia's Test van multivariate normaliteit:
	- Tests: In de eerste kolom staan alle uitgevoerde testen.
	- Waarde: De waarden van `b1p` (multivariate scheefheid) en `b2p` (multivariate kurtosis), zoals aangegeven in Mardia (1970).
	- Statistiek: De twee chi-kwadraat teststatistieken van multivariate scheefheid (zowel standaard als gecorrigeerd voor kleine steekproeven) en de standaard normale teststatistiek van multivariate kurtosis.
	- df: Vrijheidsgraden.
	- p: P-waarde. 

#### Exploratieve Factoranalyse
Factorladingen:  
- Variabelen: De eerste kolom toont alle variabelen die zijn meegenomen in de analyse. 
- PC (1, 2, 3, etc.): Deze kolom toont de factorladingen op de variabele. 
- Uniciteit: Het percentage van de variantie van elke variabele dat niet verklaard wordt door de factor.

Factor Correlaties:  
- De correlaties tussen de factoren. 

Chi-squared Toets: 
De fit van het model wordt getoetst. Als de toets significant is, dan wordt het model verworpen. Onthoud dat een chi-kwadraat schatting onbetrouwbaar kan zijn voor kleine steekproeven, en bij hele grote steekproeven kan de chi-kwadraattoets het model te snel verwerpen. Aanvullende informatie over de fit van het model kan verkregen worden door de optie `Aanvullende pas indexen` onder `Uitvoeropties` te selecteren. Voor een verdere discussie over fit indices kan bijvoorbeeld Saris, Satorra, & van der Veld (2009) geraadpleegd worden. 
- Model: Het verkregen model van de exploratieve factoranalyse. 
- Value: De chi-squared toetsstatistiek.  
- vg: Vrijheidsgraden. 
- p: P-waarde. 

Aanvullende Fit Indices: 
Deze fit indices geven informatie over de fit van het model. 
- Model: Het verkregen model van de exploratieve factoranalyse. 
- RMSEA: De wortel van de gemiddelde kwadraatsom fout van de schatting (RMSEA). Corrigeert voor spaarzaamheid. Wanneer een model hetzelfde presteert, maar het model 1 vrijheidsgraad dan model 2, wordt model 1 aangeraden. Browne and Cudeck (1993) benoemt een waarde kleiner dan 0.08 als acceptabele model fit, kleiner dan 0.05 een goede model fit, en adviseert om modellen met een waarde van 0.1 of hoger te verwerpen. Er is echter geen overeenstemming over deze grens. 
- RMSEA 90% betrouwbaarheidsinterval: Het 90% betrouwbaarheidsinterval van de wortel van de gemiddelde kwadraatsom fout van de schatting. 
- TLI: Tucker-Lewis Index. Evalueert de fit vergeleken met een striktere, genestelde baseline model. Hopwood and Donnallan (2010) suggereerde dat een waarde hoger dan .9 een goede fit aangeeft. Er is echter geen consensus over deze grens. 
- BIC: Bayesian Information Criterion. Deze maat is nuttig voor het vergelijken van de prestatie van verschillende modellen op dezelfde data. Een lage waarde impliceert een betere fit. 

#### Paddiagram 
- F(1,2,3,...): De factoren in het model zijn weergegeven als cirkels.  
- Variabelen: De variabelen zijn weergegeven als rechthoeken. 
- Pijlen: Gaan van de factoren naar de variabelen, toont de lading van de factor op de variabele. Rood betekent een negatieve lading, groen een positieve lading. Hoe wijder de pijlen, hoe hoger de lading. Deze markering kan aangepast worden bij `Markeren` in de `Uitvoeropties`. 

#### Screeplot 
De screeplot geeft informatie over hoeveel variantie in de data, aangegeven door de eigenwaarde, wordt verklaard door elke factor. Een screeplot kan gebruikt worden om te beslissen over de hoeveelheid van de factoren. 
- Factors: Op de x-as, alle mogelijke factoren. 
- Eigenvalue: Op de y-as, de eigenwaarde die de verklaarde variantie van elke factor aangeeft. 
- Data: De gestippelde lijn staat voor de data. 
- Gesimuleerd: De driehoekslijn staat voor de gesimuleerde data. Deze lijn is indicatief voor de parallel analyse. Als de punten van de gestippelde lijn (werkelijke data) boven deze lijn zijn, worden deze factoren meegenomen in het model door parallel analyse. 
- Kaiser criterium: De horizontale lijn op de eigenwaarde van 1 staat voor het Kaiser criterium. Volgens dit criterium dienen enkel factoren met waarden boven deze lijn (eigenwaarde van 1) mee te worden genomen in het model. 

### Referenties 
---
- Bandalos, D. L., & Finney, S. J. (2018). Factor analysis: Exploratory and confirmatory. In G. R. Hancock, L. M. Stapleton, & R. O. Mueller, *The reviewer’s guide to quantitative methods in the social sciences* (pp. 98-122). Routledge. https://doi.org/10.4324/9781315755649
- Dinno, A. (2014) Gently clarifying the application of Horn’s parallel analysis to principal component analysis versus factor analysis. *Working paper*. https://alexisdinno.com/Software/files/PA_for_PCA_vs_FA.pdf
- Dziuban, C. D., & Shirkey, E. C. (1974). When is a correlation matrix appropriate for factor analysis? Some decision rules. *Psychological Bulletin, 81*(6), 358–361. https://doi.org/10.1037/h0036316
- Golino, H., Shi, D., Christensen, A. P., Garrido, L. E., Nieto, M. D., Sadana, R., ... & Martinez-Molina, A. (2020). Investigating the performance of exploratory graph analysis and traditional techniques to identify the number of latent factors: A simulation and tutorial. *Psychological Methods*, *25*(3), 292. https://doi.org/10.1037/met0000255
- Hayton, J. C., Allen, D. G., & Scarpello, V. (2004). Factor retention 
    decisions in exploratory factor analysis: A tutorial on parallel analysis. *Organizational Research Methods, 7*(2), 191-205. https://doi.org/10.1177/1094428104263675
- Hopwood, C. J., & Donnellan, M. B. (2010). How should the internal structure 
    of personality inventories be evaluated? *Personality and Social Psychology Review, 14*(3), 332–346. https://doi.org/10.1177/1088868310361240
- Horn, J. L. (1965). A rationale and test for the number of factors in factor analysis. *Psychometrika, 30*(2), 179–185. https://doi.org/10.1007%2Fbf02289447
- Hu, L.-t., & Bentler, P. M. (1998). Fit indices in covariance structure modeling: Sensitivity to underparameterized model misspecification. *Psychological Methods, 3*(4), 424–453. https://doi.org/10.1037/1082-989X.3.4.424
- Mardia, K. V. (1970). Measures of multivariate skewness and kurtosis with applications. *Biometrika*, *57*(3), 519-530. https://doi.org/10.2307/2334770
- Osborne, J. W., Costello, A. B., & Kellow, J. T. (2008). Best practices in 
    exploratory factor analysis. In J. Osborne (Ed.), *Best practices in quantitative methods* (pp. 86-99). SAGE Publications, Inc. https://doi.org/10.4135/9781412995627.d8
- Saris, W. E., Satorra, A., & Van der Veld, W. M. (2009). Testing structural equation models or detection of misspecifications?. *Structural Equation Modeling: A Multidisciplinary Journal, 16*(4), 561-582. https://doi.org/10.1080/10705510903203433
- Timmerman, M. E., & Lorenzo-Seva, U. (2011). Dimensionality assessment of ordered polytomous items with parallel analysis. *Psychological Methods*, *16*(2), 209. https://doi.org/10.1037/a0023353
- Yong, A. G., & Pearce, S. (2013). A beginner’s guide to factor analysis: Focusing on exploratory factor analysis. *Tutorials in Quantitative Methods for Psychology*, *9*(2), 79-94. https://doi.org/10.20982/tqmp.09.2.p079

### R Packages 
--- 
- ggplot2
- psych
- qgraph
- stats

### Voorbeeld 
---
- Voor een voorbeeld ga naar `File`-->`Data library`-->`Factor`-->`G Factor`. 
- Voor meer details over Exploratieve Factoranalyse in JASP, zie <a href="https://www.youtube.com/watch?v=dUPzMBqcMjo&feature=youtu.be">video</a>. 
