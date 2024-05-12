library(dplyr)
library(readxl)

data <- read_xlsx("Mortality.xlsx")
names(data)[names(data)=="Variable"] <- "Kind of death"

summary(data)

data_infant <- read_xlsx("Infant_Mortality.xlsx")

#Ho trasformato la prime 4 variabili categoriche in fattori (Year non è una variabile numerica)
data_infant$Variable <- as.factor(data_infant$Variable)
data_infant$Measure <- as.factor(data_infant$Measure)
data_infant$Country <- as.factor(data_infant$Country)
data_infant$Year <- as.factor(data_infant$Year)
#Ho eliminato la colonna Flags in quanto non è rilevante ai fini dell'analisi statistica
data_infant$Flags <- NULL

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
deaths_ordered <- deaths_2015[order(deaths_2013$Value),]
barplot(deaths_ordered$Value,names.arg = deaths_ordered$Country, las=2, col=rainbow(length(deaths_ordered$Value)),cex.names = 0.5,main="Barplot Deaths in 2013")

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

