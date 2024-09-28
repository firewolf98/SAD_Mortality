library(dplyr)
library(readxl)

data <- read_xlsx("Mortality.xlsx")
names(data)[names(data)=="Variable"] <- "Kind of death"

summary(data)

data_infant <- read_xlsx("Infant_Mortality.xlsx")

summary(data_infant)

#Aggregazione dati Morti per anno
#Barplot Deaths in 2010
subset_2010 <- subset(data_infant, data_infant["Year"]==2010)
deaths_2010 <- aggregate(Value ~ Country, data = subset_2010, FUN = sum)
deaths_ordered <- deaths_2010[order(deaths_2010$Value),]
barplot(deaths_ordered$Value,names.arg = deaths_ordered$Country, las=2, col=rainbow(length(deaths_ordered$Value)),cex.names = 0.5,main="Barplot Deaths in 2010")

#Barplot Deaths in 2015
subset_2015 <- subset(data_infant, data_infant["Year"]==2015)
deaths_2015 <- aggregate(Value ~ Country, data = subset_2015, FUN = sum)
deaths_ordered <- deaths_2015[order(deaths_2015$Value),]
barplot(deaths_ordered$Value,names.arg = deaths_ordered$Country, las=2, col=rainbow(length(deaths_ordered$Value)),cex.names = 0.5,main="Barplot Deaths in 2015")

#Barplot Deaths in 2021
subset_2021 <- subset(data_infant, data_infant["Year"]==2021)
deaths_2021 <- aggregate(Value ~ Country, data = subset_2021, FUN = sum)
deaths_ordered <- deaths_2021[order(deaths_2021$Value),]
barplot(deaths_ordered$Value,names.arg = deaths_ordered$Country, las=2, col=rainbow(length(deaths_ordered$Value)),cex.names = 0.5,main="Barplot Deaths in 2021")

#Barplot Deaths in 2022
subset_2022 <- subset(data_infant, data_infant["Year"]==2022)
deaths_2022 <- aggregate(Value ~ Country, data = subset_2022, FUN = sum)
deaths_ordered <- deaths_2022[order(deaths_2022$Value),]
barplot(deaths_ordered$Value,names.arg = deaths_ordered$Country, las=2, col=rainbow(length(deaths_ordered$Value)),cex.names = 0.5,main="Barplot Deaths in 2022")

#Barplot esempio ( visualizzare le morti della colombia per anno (divisi per masure che sono 3 ))
subset_Colombia <- subset(data_infant, data_infant["Country"]== "Colombia" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Colombia <- aggregate(Value ~ Year, data = subset_Colombia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Colombia$Value,names.arg = deaths_Colombia$Year, las=2, col=rainbow(length(deaths_ordered$Value)),cex.names = 0.5,main="Barplot Deaths in Colombia")

#Barplot esempio ( visualizzare le morti del Messico per anno (divisi per masure che sono 3 ))
subset_Messico <- subset(data_infant, data_infant["Country"]== "Mexico" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Messico <- aggregate(Value ~ Year, data = subset_Messico, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Messico$Value,names.arg = deaths_Messico$Year, las=2, col=rainbow(length(deaths_ordered$Value)),cex.names = 0.5,main="Barplot Deaths in Mexico") 

#Barplot esempio ( visualizzare le morti della Costa Rica per anno (divisi per masure che sono 3 ))
subset_CostaRica <- subset(data_infant, data_infant["Country"]== "Costa Rica" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_CostaRica <- aggregate(Value ~ Year, data = subset_CostaRica, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_CostaRica$Value,names.arg = deaths_CostaRica$Year, las=2, col=rainbow(length(deaths_ordered$Value)),cex.names = 0.5,main="Barplot Deaths in Costa Rica") 

#Barplot esempio ( visualizzare le morti della Romania per anno (divisi per masure che sono 3 ))
subset_Romania <- subset(data_infant, data_infant["Country"]== "Romania" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Romania <- aggregate(Value ~ Year, data = subset_Romania, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Romania$Value,names.arg = deaths_Romania$Year, las=2, col=rainbow(length(deaths_Romania$Value)),cex.names = 0.5,main="Barplot Deaths in Romania") 

#Barplot esempio ( visualizzare le morti della Latvia per anno (divisi per masure che sono 3 ))
subset_Latvia <- subset(data_infant, data_infant["Country"]== "Latvia" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Latvia <- aggregate(Value ~ Year, data = subset_Latvia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Latvia$Value,names.arg = deaths_Latvia$Year, las=2, col=rainbow(length(deaths_Latvia$Value)),cex.names = 0.5,main="Barplot Deaths in Latvia") 

#Barplot esempio ( visualizzare le morti del Chile per anno (divisi per masure che sono 3 ))
subset_Chile <- subset(data_infant, data_infant["Country"]== "Chile" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Chile <- aggregate(Value ~ Year, data = subset_Chile, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Chile$Value,names.arg = deaths_Chile$Year, las=2, col=rainbow(length(deaths_Chile$Value)),cex.names = 0.5,main="Barplot Deaths in Chile") 

#Barplot esempio ( visualizzare le morti dell' India per anno (divisi per masure che sono 3 ))
subset_India <- subset(data_infant, data_infant["Country"]== "India" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_India <- aggregate(Value ~ Year, data = subset_India, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_India$Value,names.arg = deaths_India$Year, las=2, col=rainbow(length(deaths_India$Value)),cex.names = 0.5,main="Barplot Deaths in India") 

#Barplot esempio ( visualizzare le morti dell ' Ungaria per anno (divisi per masure che sono 3 ))
subset_Ungaria <- subset(data_infant, data_infant["Country"]== "Hungary" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Ungaria <- aggregate(Value ~ Year, data = subset_Ungaria, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Ungaria$Value,names.arg = deaths_Ungaria$Year, las=2, col=rainbow(length(deaths_Ungaria$Value)),cex.names = 0.5,main="Barplot Deaths in Hungary") 

#Barplot esempio ( visualizzare le morti del Türkiye per anno (divisi per masure che sono 3 ))
subset_Türkiye <- subset(data_infant, data_infant["Country"]== "Türkiye" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Türkiye <- aggregate(Value ~ Year, data = subset_Türkiye, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Türkiye$Value,names.arg = deaths_Türkiye$Year, las=2, col=rainbow(length(deaths_Türkiye$Value)),cex.names = 0.5,main="Barplot Deaths in Türkiye") 

#Barplot esempio ( visualizzare le morti del Sudafrica per anno (divisi per masure che sono 3 ))
subset_Sudafrica <- subset(data_infant, data_infant["Country"]== "South Africa" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Sudafrica <- aggregate(Value ~ Year, data = subset_Sudafrica, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Sudafrica$Value,names.arg = deaths_Sudafrica$Year, las=2, col=rainbow(length(deaths_Sudafrica$Value)),cex.names = 0.5,main="Barplot Deaths in South Africa") 

#Barplot esempio ( visualizzare le morti dell' Indonesia per anno (divisi per masure che sono 3 ))
subset_Indonesia <- subset(data_infant, data_infant["Country"]== "Indonesia" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Indonesia <- aggregate(Value ~ Year, data = subset_Indonesia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Indonesia$Value,names.arg = deaths_Indonesia$Year, las=2, col=rainbow(length(deaths_Indonesia$Value)),cex.names = 0.5,main="Barplot Deaths in Indonesia") 

#Barplot esempio ( visualizzare le morti della Francia per anno (divisi per masure che sono 3 ))
subset_Francia <- subset(data_infant, data_infant["Country"]== "France" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Francia <- aggregate(Value ~ Year, data = subset_Francia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Francia$Value,names.arg = deaths_Francia$Year, las=2, col=rainbow(length(deaths_Francia$Value)),cex.names = 0.5,main="Barplot Deaths in France") 

#Barplot esempio ( visualizzare le morti della Croazia per anno (divisi per masure che sono 3 ))
subset_Croazia <- subset(data_infant, data_infant["Country"]== "Croatia" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Croazia <- aggregate(Value ~ Year, data = subset_Croazia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Croazia$Value,names.arg = deaths_Croazia$Year, las=2, col=rainbow(length(deaths_Croazia$Value)),cex.names = 0.5,main="Barplot Deaths in Croatia") 

#Barplot esempio ( visualizzare le morti della Republica Slovacca per anno (divisi per masure che sono 3 ))
subset_RepSlo <- subset(data_infant, data_infant["Country"]== "Slovak Republic" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_RepSlo <- aggregate(Value ~ Year, data = subset_RepSlo, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_RepSlo$Value,names.arg = deaths_RepSlo$Year, las=2, col=rainbow(length(deaths_RepSlo$Value)),cex.names = 0.5,main="Barplot Deaths in Slovak Republic") 

#Barplot esempio ( visualizzare le morti di Isdraele per anno (divisi per masure che sono 3 ))
subset_Isdraele <- subset(data_infant, data_infant["Country"]== "Israel" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Isdraele <- aggregate(Value ~ Year, data = subset_Isdraele, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Isdraele$Value,names.arg = deaths_Isdraele$Year, las=2, col=rainbow(length(deaths_Isdraele$Value)),cex.names = 0.5,main="Barplot Deaths in Israel") 

#Barplot esempio ( visualizzare le morti del Brasile per anno (divisi per masure che sono 3 ))
subset_Brasile <- subset(data_infant, data_infant["Country"]== "Brazil" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Brasile <- aggregate(Value ~ Year, data = subset_Brasile, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Brasile$Value,names.arg = deaths_Brasile$Year, las=2, col=rainbow(length(deaths_Brasile$Value)),cex.names = 0.5,main="Barplot Deaths in Brazil") 

#Barplot esempio ( visualizzare le morti della Polonia per anno (divisi per masure che sono 3 ))
subset_Polonia <- subset(data_infant, data_infant["Country"]== "Poland" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Polonia <- aggregate(Value ~ Year, data = subset_Polonia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Polonia$Value,names.arg = deaths_Polonia$Year, las=2, col=rainbow(length(deaths_Polonia$Value)),cex.names = 0.5,main="Barplot Deaths in Poland") 

#Barplot esempio ( visualizzare le morti della Korea per anno (divisi per masure che sono 3 ))
subset_Korea <- subset(data_infant, data_infant["Country"]== "Korea" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Korea <- aggregate(Value ~ Year, data = subset_Korea, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Korea$Value,names.arg = deaths_Korea$Year, las=2, col=rainbow(length(deaths_Korea$Value)),cex.names = 0.5,main="Barplot Deaths in Korea") 

#Barplot esempio ( visualizzare le morti del Peru per anno (divisi per masure che sono 3 ))
subset_Peru <- subset(data_infant, data_infant["Country"]== "Peru" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Peru <- aggregate(Value ~ Year, data = subset_Peru, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Peru$Value,names.arg = deaths_Peru$Year, las=2, col=rainbow(length(deaths_Peru$Value)),cex.names = 0.5,main="Barplot Deaths in Peru") 

#Barplot esempio ( visualizzare le morti del Portogallo per anno (divisi per masure che sono 3 ))
subset_Portogallo <- subset(data_infant, data_infant["Country"]== "Portugal" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Portogallo <- aggregate(Value ~ Year, data = subset_Portogallo, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Portogallo$Value,names.arg = deaths_Portogallo$Year, las=2, col=rainbow(length(deaths_Portogallo$Value)),cex.names = 0.5,main="Barplot Deaths in Portugal") 

#Barplot esempio ( visualizzare le morti dell' Austria per anno (divisi per masure che sono 3 ))
subset_Austria <- subset(data_infant, data_infant["Country"]== "Austria" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Austria <- aggregate(Value ~ Year, data = subset_Austria, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Austria$Value,names.arg = deaths_Austria$Year, las=2, col=rainbow(length(deaths_Austria$Value)),cex.names = 0.5,main="Barplot Deaths in Austria") 

#Barplot esempio ( visualizzare le morti della Repubblica Ceca per anno (divisi per masure che sono 3 ))
subset_RepCez <- subset(data_infant, data_infant["Country"]== "Czech Republic" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_RepCez <- aggregate(Value ~ Year, data = subset_RepCez, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_RepCez$Value,names.arg = deaths_RepCez$Year, las=2, col=rainbow(length(deaths_RepCez$Value)),cex.names = 0.5,main="Barplot Deaths in Czech Republic") 

#Barplot esempio ( visualizzare le morti della Svizzera per anno (divisi per masure che sono 3 ))
subset_Svizzera <- subset(data_infant, data_infant["Country"]== "Switzerland" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Svizzera <- aggregate(Value ~ Year, data = subset_Svizzera, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Svizzera$Value,names.arg = deaths_Svizzera$Year, las=2, col=rainbow(length(deaths_Svizzera$Value)),cex.names = 0.5,main="Barplot Deaths in Switzerland") 

#Barplot esempio ( visualizzare le morti della Spagna per anno (divisi per masure che sono 3 ))
subset_Spagna <- subset(data_infant, data_infant["Country"]== "Spain" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Spagna <- aggregate(Value ~ Year, data = subset_Spagna, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Spagna$Value,names.arg = deaths_Spagna$Year, las=2, col=rainbow(length(deaths_Spagna$Value)),cex.names = 0.5,main="Barplot Deaths in Spain") 

#Barplot esempio ( visualizzare le morti dell' Australia per anno (divisi per masure che sono 3 ))
subset_Australia <- subset(data_infant, data_infant["Country"]== "Australia" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Australia <- aggregate(Value ~ Year, data = subset_Australia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Australia$Value,names.arg = deaths_Australia$Year, las=2, col=rainbow(length(deaths_Australia$Value)),cex.names = 0.5,main="Barplot Deaths in Australia") 

#Barplot esempio ( visualizzare le morti di Lussemburgo per anno (divisi per masure che sono 3 ))
subset_Lussemburgo <- subset(data_infant, data_infant["Country"]== "Luxembourg" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Lussemburgo <- aggregate(Value ~ Year, data = subset_Lussemburgo, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Lussemburgo$Value,names.arg = deaths_Lussemburgo$Year, las=2, col=rainbow(length(deaths_Lussemburgo$Value)),cex.names = 0.5,main="Barplot Deaths in Luxembourg") 

#Barplot esempio ( visualizzare le morti della Germania per anno (divisi per masure che sono 3 ))
subset_Germania <- subset(data_infant, data_infant["Country"]== "Germany" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Germania <- aggregate(Value ~ Year, data = subset_Germania, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Germania$Value,names.arg = deaths_Germania$Year, las=2, col=rainbow(length(deaths_Germania$Value)),cex.names = 0.5,main="Barplot Deaths in Germany") 

#Barplot esempio ( visualizzare le morti della Grecia per anno (divisi per masure che sono 3 ))
subset_Grecia <- subset(data_infant, data_infant["Country"]== "Greece" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Grecia <- aggregate(Value ~ Year, data = subset_Grecia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Grecia$Value,names.arg = deaths_Grecia$Year, las=2, col=rainbow(length(deaths_Grecia$Value)),cex.names = 0.5,main="Barplot Deaths in Greece") 

#Barplot esempio ( visualizzare le morti della Norvegia per anno (divisi per masure che sono 3 ))
subset_Norvegia <- subset(data_infant, data_infant["Country"]== "Norway" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Norvegia <- aggregate(Value ~ Year, data = subset_Norvegia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Norvegia$Value,names.arg = deaths_Norvegia$Year, las=2, col=rainbow(length(deaths_Norvegia$Value)),cex.names = 0.5,main="Barplot Deaths in Norway") 

#Barplot esempio ( visualizzare le morti di Netherlands per anno (divisi per masure che sono 3 ))
subset_Netherland <- subset(data_infant, data_infant["Country"]== "Netherlands" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Netherlands <- aggregate(Value ~ Year, data = subset_Netherland, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Netherlands$Value,names.arg = deaths_Netherlands$Year, las=2, col=rainbow(length(deaths_Netherlands$Value)),cex.names = 0.5,main="Barplot Deaths in Netherlands") 

#Barplot esempio ( visualizzare le morti di Denmark per anno (divisi per masure che sono 3 ))
subset_Danimarca <- subset(data_infant, data_infant["Country"]== "Denmark" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Danimarca <- aggregate(Value ~ Year, data = subset_Danimarca, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Danimarca$Value,names.arg = deaths_Danimarca$Year, las=2, col=rainbow(length(deaths_Danimarca$Value)),cex.names = 0.5,main="Barplot Deaths in Denmark") 

#Barplot esempio ( visualizzare le morti dell' Argentina per anno (divisi per masure che sono 3 ))
subset_Argentina <- subset(data_infant, data_infant["Country"]== "Argentina" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Argentina <- aggregate(Value ~ Year, data = subset_Argentina, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Argentina$Value,names.arg = deaths_Argentina$Year, las=2, col=rainbow(length(deaths_Argentina$Value)),cex.names = 0.5,main="Barplot Deaths in Argentina") 

#Barplot esempio ( visualizzare le morti del Regno Unito per anno (divisi per masure che sono 3 ))
subset_UK <- subset(data_infant, data_infant["Country"]== "United Kingdom" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_UK <- aggregate(Value ~ Year, data = subset_UK, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_UK$Value,names.arg = deaths_UK$Year, las=2, col=rainbow(length(deaths_UK$Value)),cex.names = 0.5,main="Barplot Deaths in United Kingdom") 

#Barplot esempio ( visualizzare le morti della Slovenia per anno (divisi per masure che sono 3 ))
subset_Slovenia <- subset(data_infant, data_infant["Country"]== "Slovenia" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Slovenia <- aggregate(Value ~ Year, data = subset_Slovenia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Slovenia$Value,names.arg = deaths_Slovenia$Year, las=2, col=rainbow(length(deaths_Slovenia$Value)),cex.names = 0.5,main="Barplot Deaths in Slovenia") 

#Barplot esempio ( visualizzare le morti dell' Estonia per anno (divisi per masure che sono 3 ))
subset_Estonia <- subset(data_infant, data_infant["Country"]== "Estonia" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Estonia <- aggregate(Value ~ Year, data = subset_Estonia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Estonia$Value,names.arg = deaths_Estonia$Year, las=2, col=rainbow(length(deaths_Estonia$Value)),cex.names = 0.5,main="Barplot Deaths in Estonia") 

#Barplot esempio ( visualizzare le morti del Giappone per anno (divisi per masure che sono 3 ))
subset_Giappone <- subset(data_infant, data_infant["Country"]== "Japan" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Giappone <- aggregate(Value ~ Year, data = subset_Giappone, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Giappone$Value,names.arg = deaths_Giappone$Year, las=2, col=rainbow(length(deaths_Giappone$Value)),cex.names = 0.5,main="Barplot Deaths in Japan") 

#Barplot esempio ( visualizzare le morti della Lituania per anno (divisi per masure che sono 3 ))
subset_Lituania <- subset(data_infant, data_infant["Country"]== "Lithuania" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Lituania <- aggregate(Value ~ Year, data = subset_Lituania, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Lituania$Value,names.arg = deaths_Lituania$Year, las=2, col=rainbow(length(deaths_Lituania$Value)),cex.names = 0.5,main="Barplot Deaths in Lithuania") 

#Barplot esempio ( visualizzare le morti della Finlandia per anno (divisi per masure che sono 3 ))
subset_Finlandia <- subset(data_infant, data_infant["Country"]== "Finland" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Finlandia <- aggregate(Value ~ Year, data = subset_Finlandia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Finlandia$Value,names.arg = deaths_Finlandia$Year, las=2, col=rainbow(length(deaths_Finlandia$Value)),cex.names = 0.5,main="Barplot Deaths in Finland") 

#Barplot esempio ( visualizzare le morti della Bulgaria per anno (divisi per masure che sono 3 ))
subset_Bulagria <- subset(data_infant, data_infant["Country"]== "Bulgaria" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Bulgaria <- aggregate(Value ~ Year, data = subset_Bulagria, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Bulgaria$Value,names.arg = deaths_Bulgaria$Year, las=2, col=rainbow(length(deaths_Bulgaria$Value)),cex.names = 0.5,main="Barplot Deaths in Bulgaria")

#Barplot esempio ( visualizzare le morti della Svezia per anno (divisi per masure che sono 3 ))
subset_Svezia <- subset(data_infant, data_infant["Country"]== "Sweden" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Svezia <- aggregate(Value ~ Year, data = subset_Giappone, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Svezia$Value,names.arg = deaths_Svezia$Year, las=2, col=rainbow(length(deaths_Svezia$Value)),cex.names = 0.5,main="Barplot Deaths in Sweden") 

#Barplot esempio ( visualizzare le morti dell' Irlanda per anno (divisi per masure che sono 3 ))
subset_Irlanda <- subset(data_infant, data_infant["Country"]== "Ireland" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Irlanda <- aggregate(Value ~ Year, data = subset_Irlanda, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Irlanda$Value,names.arg = deaths_Irlanda$Year, las=2, col=rainbow(length(deaths_Irlanda$Value)),cex.names = 0.5,main="Barplot Deaths in Ireland") 

#Barplot esempio ( visualizzare le morti dell' Iceland per anno (divisi per masure che sono 3 ))
subset_Iceland <- subset(data_infant, data_infant["Country"]== "Iceland" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Iceland <- aggregate(Value ~ Year, data = subset_Iceland, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Iceland$Value,names.arg = deaths_Iceland$Year, las=2, col=rainbow(length(deaths_Iceland$Value)),cex.names = 0.5,main="Barplot Deaths in Iceland") 

#Barplot esempio ( visualizzare le morti del Belgio per anno (divisi per masure che sono 3 ))
subset_Belgio <- subset(data_infant, data_infant["Country"]== "Belgium" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Belgio <- aggregate(Value ~ Year, data = subset_Belgio, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Belgio$Value,names.arg = deaths_Belgio$Year, las=2, col=rainbow(length(deaths_Belgio$Value)),cex.names = 0.5,main="Barplot Deaths in Belgium") 

#Barplot esempio ( visualizzare le morti dell' Italia per anno (divisi per masure che sono 3 ))
subset_Italia <- subset(data_infant, data_infant["Country"]== "Italy" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Italia <- aggregate(Value ~ Year, data = subset_Italia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Italia$Value,names.arg = deaths_Italia$Year, las=2, col=rainbow(length(deaths_Italia$Value)),cex.names = 0.5,main="Barplot Deaths in Italy") 

#Barplot esempio ( visualizzare le morti della Russia per anno (divisi per masure che sono 3 ))
subset_Russia <- subset(data_infant, data_infant["Country"]== "Russia" & data_infant$Measure == "Deaths per 1 000 live births")
deaths_Russia <- aggregate(Value ~ Year, data = subset_Russia, FUN = sum)
#deaths_ordered <- deaths_Colombia[order(deaths_Colombia$Value),]
barplot(deaths_Russia$Value,names.arg = deaths_Russia$Year, las=2, col=rainbow(length(deaths_Russia$Value)),cex.names = 0.5,main="Barplot Deaths in Russia") 

#Barplot per più morti per stato lungo tutto l'arco temporale di un tipo di morte, ossia Infant mortality, No minimum threshold... (2010-2022)
subset_temp <- subset(data_infant, data_infant["Variable"]== "Infant mortality, No minimum threshold of gestation period or birthweight"  )
deaths_total <- aggregate(Value ~ Country, data = subset_temp, FUN = sum)
barplot(deaths_total$Value,names.arg = deaths_total$Country, las=2, col=rainbow(length(deaths_total$Value)),cex.names = 0.5,main="Barplot for countries with fixed death type  ") 

#Barplot per più morti per stato lungo tutto l'arco temporale di un tipo di morte, ossia Infant mortality, No minimum threshold... (2019-2020)
subset_temp <- subset(data_infant, data_infant["Variable"]== "Infant mortality, No minimum threshold of gestation period or birthweight" & (data_infant$Year == 2019 | data_infant$Year == 2020) )
deaths_total <- aggregate(Value ~ Country, data = subset_temp, FUN = sum)
barplot(deaths_total$Value,names.arg = deaths_total$Country, las=2, col=rainbow(length(deaths_total$Value)),cex.names = 0.5,main="Barplot for countries with fixed death type  ") 

#Barplot per più morti per stato lungo tutto l'arco temporale di un tipo di morte, ossia Infant mortality, No minimum threshold... (tutti gli altri anni)
subset_temp <- subset(data_infant, data_infant["Variable"]== "Infant mortality, No minimum threshold of gestation period or birthweight" & (data_infant$Year != 2019 | data_infant$Year != 2020) )
deaths_total <- aggregate(Value ~ Country, data = subset_temp, FUN = sum)
barplot(deaths_total$Value,names.arg = deaths_total$Country, las=2, col=rainbow(length(deaths_total$Value)),cex.names = 0.5,main="Barplot for countries with fixed death type  ") 

