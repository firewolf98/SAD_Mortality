library(dplyr)
library(readxl)

data <- read_xlsx("Mortality.xlsx")
names(data)[names(data)=="Variable"] <- "Kind of death"

summary(data)

data_infant <- read_xlsx("Infant_Mortality.xlsx")


# Definizione della funzione di mapping
map_values <- function(value, measure) {
  # Utilizziamo case_when per operare su vettori di valori
  mapped_value <- case_when(
    measure == "Deaths per 100 000 live births" ~ value * 1000 / 100000,
    measure == "Deaths per 1 000 live births" ~ value,
   
    TRUE ~ NA_real_  # Restituiamo NA se nessuna delle condizioni è soddisfatta
  )
  
  return(mapped_value)
}

data_infant$Value_transformed = data_infant$Value

# Applica la funzione di mapping al dataframe
mapped_data <- data_infant %>%
  mutate(Value_transformed = map_values(Value_transformed, Measure))

# Visualizza i risultati
print(mapped_data)

to_plot <- mapped_data[!is.na(mapped_data$Value_transformed), ]

summary(data_infant)

#Barplot per tutte le morti per tutti gli stati nell'arco temporale 2019-2020
subset_mapped_data <- subset(mapped_data, !is.na(Value_transformed))
deaths_total <- aggregate(Value_transformed ~ Country, data = subset_mapped_data, FUN = sum)
barplot(deaths_total$Value_transformed,names.arg = deaths_total$Country, las=2, col=rainbow(length(deaths_total$Value_transformed)),cex.names = 0.5,main="Morti totali in ogni paese") 


#Barplot per più morti per stato lungo tutto l'arco temporale di un tipo di morte, ossia Infant mortality, No minimum threshold... (2010-2022)
subset_temp <- subset(data_infant, data_infant["Variable"]== "Infant mortality, No minimum threshold of gestation period or birthweight" & (mapped_data$Year == 2019 | data_infant$Year == 2020) )
deaths_total <- aggregate(Value ~ Country, data = subset_temp, FUN = sum)
barplot(deaths_total$Value,names.arg = deaths_total$Country, las=2, col=rainbow(length(deaths_total$Value)),cex.names = 0.5,main="Barplot for countries with fixed death type  ") 

#Barplot per più morti per stato lungo tutto l'arco temporale di un tipo di morte, ossia Infant mortality, No minimum threshold... (2010-2022)
subset_temp <- subset(data_infant, data_infant["Variable"]== "Infant mortality, No minimum threshold of gestation period or birthweight" & (mapped_data$Year != 2019 | data_infant$Year != 2020) )
deaths_total <- aggregate(Value ~ Country, data = subset_temp, FUN = sum)
barplot(deaths_total$Value,names.arg = deaths_total$Country, las=2, col=rainbow(length(deaths_total$Value)),cex.names = 0.5,main="Barplot for countries with fixed death type  ") 

#bisogna rendere comparabili i value_trasformed perche nei due grafici non sono nella stessa scala.(minmaxscaling, riportare tutto tra 0 e 1)

#Barplot per più morti per stato lungo tutto l'arco temporale di un tipo di morte, ossia Infant mortality, No minimum threshold... (2019-2020)
subset_temp <- subset(data_infant, data_infant["Variable"]== "Infant mortality, No minimum threshold of gestation period or birthweight" & (data_infant$Year == 2019 | data_infant$Year == 2020) )
deaths_total <- aggregate(Value ~ Country, data = subset_temp, FUN = sum)
barplot(deaths_total$Value,names.arg = deaths_total$Country, las=2, col=rainbow(length(deaths_total$Value)),cex.names = 0.5,main="Barplot for countries with fixed death type  ") 

#Barplot per più morti per stato lungo tutto l'arco temporale di un tipo di morte, ossia Infant mortality, No minimum threshold... (tutti gli altri anni) CAMBIARE DATA_INFANT CON TO_PLOT
subset_temp <- subset(data_infant, data_infant["Variable"]== "Infant mortality, No minimum threshold of gestation period or birthweight" & (data_infant$Year != 2019 | data_infant$Year != 2020) )
deaths_total <- aggregate(Value ~ Country, data = subset_temp, FUN = sum)
barplot(deaths_total$Value,names.arg = deaths_total$Country, las=2, col=rainbow(length(deaths_total$Value)),cex.names = 0.5,main="Barplot for countries with fixed death type  ") 
abline(h=mean(deaths_total$Value), color="black", lwd=2)
#legend("topright", legend = paste("Media =", round(mean(deaths_total$Value), 2)), col = "red", lwd = 2, )
legend("topright", 
       legend = paste("Media =", round(mean(deaths_total$Value), 2)), 
       col = "red", 
       lwd = 2, 
       cex = 0.8,  # Imposta la grandezza del carattere a 80% del normale
       x = 49,   # Mantieni l'assegnazione automatica della posizione orizzontale
       y = max(deaths_total$Value))   # Mantieni l'assegnazione automatica della posizione verticale


