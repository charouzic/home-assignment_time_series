########################## Úkol č.3 ##########################

#1) Importujte .csv soubor: AKM_CR_dcv.csv; soubor obsahuje udaje za realne HDP pro 6 zemi. Transformujte rady podle potreby.
#2) Prevedte rady na rozumny format tak, abyste s nimi mohli dale pracovat. Jaky objekt volite a proc?
#3) Zjistete, o jakou delku a frekvenci rad se jedna.
#4) Jake jsou popisne charakteristiky rad? Jaka je jejich intepretace?
#5) Otestujte, zda se v radach vyskytuje sezonnost. Jaka byla podle Vas pouzita metoda na jeji ocisteni?
#6) Bez ohledu na bod 5) dekomponujte rady a pouzijte aditivní a multiplikativni dekompozici. Interpretuje vysledky a rozdily.
#7) Nahrajte balicek ggplot2 a vytvorte srozumitelny graf pro vsechny rady.
#8) Otestujte stacionaritu vsech rad pro hodnoty a prvni diference. Jsou rady I(1)? Jaká transformace povede ke stacionarite?
#9) Po transformaci vytvorte jednoduchy regresni model za pouziti jakychkoli dvou rad a interpretujte co nejpresneji vysledky.
#10) Otestujte robustnost modelu podle libovolnych dvou kriterii.
#11) Nasimulujte si libovolnou radu (mimo soubor), která bude mit formu nahodne prochazky, a opet otestujte stacionaritu
#12) Nadefinujte si nahodnou prochazku s driftem a srovnejte s Vasi puvodni nahodnou prochazkou. V cem se lisi a proc?

install.packages("readr")
library(readr)
install.packages("forecast")
library(forecast)


#1) import souboru - načteme data "AKM_CR_dcv.csv", vynecháme první řádek (manuálně odstraněn)
data = read.csv("AKM_CR_dcv.csv")
View(data)


#2) transformace řady
IGBR=ts(data$United.Kingdom,frequency = 4,start=c(1996,1),end = c(2018,4))
IESP=ts(data$Spain,frequency = 4,start=c(1996,1),end = c(2018,4))
IITA=ts(data$Italy,frequency = 4,start=c(1996,1),end = c(2018,4))
IDEU=ts(data$Germany,frequency = 4,start=c(1996,1),end = c(2018,4))
ICZE=ts(data$Czech.Republic,frequency = 4,start=c(1996,1),end = c(2018,4))
IRUS=ts(data$Russian.Federation,frequency = 4,start=c(1996,1),end = c(2018,4))
# rozdělil jsem si data podle jednotlivých sloupců pro budoucí práci, máme kvartální HDP -> frekvence 4


#3) zjištění délky a frekvence
rady=ts(data=data,frequency = 4,start = c(1996,1)) 
summary(rady)
View(rady)
# volím time series o frekvenci 4 protože máme kvartální údaje, počátek první kvartál 1996 a konec 4 kvartál 2018 jako poslední kvartál


#4) popisné charakteristiky řad
library(foreign)
library(pastecs)
options(scripen=999)
options(digits = 1)
stat.desc(rady)

#nbr.val = počet hodnot -> 92 (u všech stejné)
#nbr.null = počet prázdných/nevyplněných polí/hodnot -> 0 (u všech stejné)
#nbr.na = počet nedostupných polí/hodnot -> 0 (u všech stejné)
#min = nejnižší hodnota  -> různé u každé řady
#max = nejvyšší hodnota -> různé u každé řady      
#range = rozdíl nevyšší a nejnižší hodnoty -> různé u každé řady   
#sum = součet všech hodnot -> různé u každé řady      
#median = prostřední hodnota při seřazení hodnot od nejnižší po nejvyšší -> různé u každé řady
#mean = průměr hodnot -> různé u každé řady
#SE.mean standartní chyba průměru -> různé u každé řady
#CI.mean.0.95 = interval spolehlivosti pro 95% -> 5% hodnot bude mít menší průměr než tato hodnota -> různé u každé řady
#var = rozptyl -> různé u každé řady    
#std.dev směrodatná odchylka -> popisuje nakolik se od sebe liší jednotlivé hodnoty v souboru zkoumaných hodnot -> různé u každé řady
#coef.var = rozptylový koeficient = podíl směrodatné odchylky a medianu -> různé 


#5) sezonnost
plot.ts(rady)
# z grafů lze usoudit že se zde sezonnost neobjevuje
# pro očištění se dají použít lineární filtry, nejčastěji použitím klouzavých průměrů

# samostatná sezonnost UK
sez_koef_IGBR=data.frame(IGBR)
sez_koef_IGBR=cbind(sez_koef_IGBR, seq(1:4))
sez_koef_IGBR[order(-sez_koef_IGBR[,1]),]
plot.ts(sez_koef_IGBR)

# samostatná sezonnost Španělska
sez_koef_IESP=data.frame(IESP)
sez_koef_IESP=cbind(sez_koef_IESP, seq(1:4))
sez_koef_IESP[order(-sez_koef_IESP[,1]),]
plot.ts(sez_koef_IESP)


#6a) dekompozice aditivní
IGBR_Adekompozice= decompose(IGBR,type="additive")
plot(IGBR_Adekompozice)
# observed i trend mají rostoucí tendenci, pokles okolo roku 2008 kvůli krizi a poté růst
# seasonal má roční pravidelnou tendenci kdy vždy na začátku dosahuje maxima poté pokled o minima, v polovině roku růst do lokálního maxima, pokles a poté růst
# random má nepravidelný průběh, největší pokles  v roce 2009

IESP_Adekompozice= decompose(IESP,type="additive")
plot(IESP_Adekompozice)
# observed i trend mají téměř stejný průběh, konstantní růst do roku 2008, poté propad až do roku 2014 vidíme pokles a poté opět růst
# seasonal má roční pravidelnou tendenci, kde dosahuje maxima na začátku roku roku poté pokles do minima, opět rust, lehký pokles a poté růst 
# random má nepravidelné výkyvy, největší vývyk jsme zaznamenali v roce 2009, druhý největší pokles nastal v roce 2013

IITA_Adekompozice= decompose(IITA,type="additive")
plot(IITA_Adekompozice)
# observed i trend mají téměř stejný průběh,  růst do roku 2008 poté několikrát propad až do roku 2014 kdy se opšt graf vrací k rostoucímů trendu
# seasonal má roční pravidelný průběh, začátek roku jsme na minimu poté zezačátku mírný růst poté prudký růst do maxima
# random má nepravidelný kolísavý průběh, největší výkyv je okolo roku 2009 kde se dostáváme na nejnižší hodnotu

IDEU_Adekompozice= decompose(IDEU,type="additive")
plot(IDEU_Adekompozice)
# observed a trend mají téměř stejný rostoucí trend, u obou můžeme pozorovat lehký propad v roce 2009, poté dochází opšt k růstu
# seasonal má stejný roční průběh kdy hodnota pravidelně kolísá od nejnižší hodnoty na začátku roku po nejvyšší hodnotu uprostřed roku
# random má nepravidelný průběh, největší propad v roce 2009

ICZE_Adekompozice= decompose(ICZE,type="additive")
plot(ICZE_Adekompozice)
# observed a trend mají rostoucí trend, lokální max v roce 2009, lokální minimum v roce 2009 kde se hodnota drží až do rkoli 2014 kde začínají křivky opět růst
# seasonal má stejný roční průběh kdy hodnota pravidelně kolísá od nejnižší po nejvyšší hodnotu,rok začíná v minimu, poté se dostáváme do maxima kde se pár měsíců udrží a opět pokles do minima
# random má nepravidelný průběh, největší propad v roce 2009

IRUS_Adekompozice= decompose(IRUS,type="additive")
plot(IRUS_Adekompozice)
# observed i trend mají podobný průběh, křivka roste až do roku 2009, poté lehký pokles a následná pozvolný růst
# seasonal má stejný roční průběh kdy hodnota pravidelně kolísá od nejnižší po nejvyšší hodnotu, na minimu se hodnota drží pár měsíců
# random má nepravidelný průběh, největší pokles v roce 2009


#6b) dekompozice multiplikativní
IGBR_Mdekompozice= decompose(IGBR,type="multiplicative")
plot(IGBR_Mdekompozice)
# u multiplikativní d. můžeme vidět nižší amplitudu u seassonal

IESP_Mdekompozice= decompose(IESP,type="multiplicative")
plot(IESP_Mdekompozice)
# opšt jen lehká změna amplitudy u seassonal u mult.d.

IITA_Mdekompozice= decompose(IITA,type="multiplicative")
plot(IITA_Mdekompozice)
# téměř totožné

IDEU_Mdekompozice= decompose(IDEU,type="multiplicative")
plot(IDEU_Mdekompozice)
# téměř totožné

ICZE_Mdekompozice= decompose(ICZE,type="multiplicative")
plot(ICZE_Mdekompozice)
#téměř totožné

IRUS_Mdekompozice =decompose(IRUS, type="multiplicative")
plot(IRUS_Mdekompozice)
# téměř totožné


#7) nahrání balíčku ggplot2
install.packages("ggplot2")
install.packages("scales")
library(ggplot2)
library(scales)
ggplot(IGBR,IESP,IITA,IDEU,ICZE,IRUS)

#graf
ts.plot(IGBR,IESP,IITA,IDEU,ICZE, IRUS)

#jednotlivé grafy pro větší a lepčí přehlednost
ts.plot(IGBR)
ts.plot(IESP)
ts.plot(IITA)
ts.plot(IDEU)
ts.plot(ICZE)
ts.plot(IRUS)


#8) testování stacionartity
install.packages("tseries")
require(tseries)
kpss.test(IGBR) 
kpss.test(IESP)
kpss.test(IITA)
kpss.test(IDEU)
kpss.test(ICZE)
kpss.test(IRUS)
# ve všech šesti případech je p-value menší než 0,1 => zamítáme nulovou hypotézu, že je řada trendivě stacionární

diff_IGBR=diff(IGBR)
diff_IESP=diff(IESP)
diff_IITA=diff(IITA)
diff_IDEU=diff(IDEU)
diff_ICZE=diff(ICZE)
diff_IRUS=diff(IRUS)
kpss.test(diff_IGBR)
kpss.test(diff_IESP)
kpss.test(diff_IITA)
kpss.test(diff_IDEU)
kpss.test(diff_ICZE)
kpss.test(diff_IRUS)
# první diference mají vyšší p-hodnotu -> potvrzení nulové hypotézy že je řada trendově stacionární


#9) regresní model 
regrese=lm(ICZE~IRUS)
summary(regrese)
# zápis modelu ICZE = 1085,38 + 0,04*IRUS + e
# když se změní HDP Ruska o jednotku, české HDP vzroste cca o 0,04
# t-value je nižší než kritická hodnota
# vzhledem k p-hodnotě usuzuji že n 5% hladině významnosti jsou všechny parametr významné
#podařil se nám vysvětlit 93,3% modelu => model je vhodný


#10) testivání robustnosti modelu
#Breusch–Pagan test - heteroskedasticita 
install.packages("lmtest")
library(lmtest)
bptest(regrese)
# p-hodnota nízká (menší než 0,05) -> potvzuji H0 (model je robustní)

install.packages("car")
install.packages("sandwich")
install.packages("lmtest")
library(car)
library(sandwich)
library(lmtest)
coeftest(regrese,vcovHC)


#11) nasimulování libovolné řady formou náhodné procházky
n=500
promenna=rnorm(n)
x0=rep(0,n)
for (i in seq.int(2,n))
  x0[i]= x0[i-1]+promenna[i]
plot(ts(x0))

#11b) testování stacionarity 
require(tseries)
plot(ts(x0))
kpss.test(x0)
# p-value je 0,05 -> můžeme zamítnout nulovou hypotézu že je zde stacionarita


#12) náhodná procházka s driftem
drift = 2.5
x1 = rep(0,n)
for (i in seq.int(2,n))
  x1[i] = drift + x1[i-1] + promenna[i]
# test stacionarity u RW s driftem
require(tseries)
plot(ts(x1))
kpss.test(x1)
# drift dal grafu jasně viditelný rostoucí trend oproti RW bez driftu -> způsobeno vázaností na předchozí obodbí t-1


#### autor: Viktor Macek ####
