data <- read_xlsx("Mortality.xlsx")
names(data)[names(data)=="Variable"] <- "Kind of death"

summary(data)

data_infant <- read_xlsx("Infant_Mortality.xlsx")

summary(data_infant)

subset_2010 <- subset(data_infant, data_infant["Year"]==2010)

library(dplyr)

deaths_2010 <- aggregate(Value ~ Country, data = subset_2010, FUN = sum)

deaths_ordered <- deaths_2010[order(deaths_2010$Value),]

barplot(deaths_ordered$Value,names.arg = deaths_ordered$Country, las=2, col=rainbow(length(deaths_ordered$Value)),cex.names = 0.5,main="Barplot Deaths in 2010")

summary(data_infant) 