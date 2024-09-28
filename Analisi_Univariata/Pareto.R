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

# This function will create a Pareto diagram based on the array parameter
createPareto <- function (array_to_analyze, array_aggregation, title){
  # Sum all items
  mySum = sum(array_to_analyze)
  array_to_analyze <- array_to_analyze / mySum
  
  # Ordering 
  ari_ordered <- order(array_to_analyze, decreasing = TRUE)
  
  # Creating graphic
  bp <- barplot(array_to_analyze[ari_ordered],
                col=rainbow(length(array_to_analyze)), 
                names = array_aggregation[ari_ordered], las=2, ylim = c(0, 1.07), main = title)
  
  lines(bp, cumsum(array_to_analyze[ari_ordered]), 
        type = "b", pch = 16, col = "blue")
  
  text(bp - 0.3, cumsum(array_to_analyze[ari_ordered]) + 0.05,
       paste(format(cumsum(array_to_analyze[ari_ordered]) * 100,
                    digits = 3)), cex=0.6)
  
  legend(1, 0.9, legend=c("copertura in percentuale"),
         col=c("blue"), lty=1:2, cex=0.8)
}
selected_country <- "Colombia"

subset_temp <- subset(to_plot, to_plot["Country"]== selected_country & (to_plot$Year == 2019 | to_plot$Year == 2020) & !to_plot$Measure == "Deaths per 100 000 live births" )
deaths_total1 <- aggregate(Value_transformed ~ Variable, data = subset_temp, FUN = sum)

createPareto(deaths_total1$Value_transformed, deaths_total1$Variable, "Diagramma di pareto")

subset_temp <- subset(to_plot, to_plot["Country"]== selected_country & (to_plot$Year != 2019 | to_plot$Year != 2020) & !to_plot$Measure == "Deaths per 100 000 live births" )
deaths_total2 <- aggregate(Value_transformed ~ Variable, data = subset_temp, FUN = sum)

createPareto(deaths_total2$Value_transformed, deaths_total2$Variable, "Diagramma di pareto")