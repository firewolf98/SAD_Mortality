library(dplyr)
library(readxl)

data <- read_xlsx("Mortality.xlsx")
names(data)[names(data) == "Variable"] <- "Kind of death"

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

map_region <- function(df) {
  eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                    "Czech Republic", "Denmark", "Estonia", "Finland", 
                    "France", "Germany", "Greece", "Hungary", "Ireland", 
                    "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
                    "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", 
                    "Slovenia", "Spain", "Sweden")
  
  df %>% mutate(Region = ifelse(Country %in% eu_countries, "EU", "Non-EU"))
}

to_plot_EU_extraEU<- map_region(to_plot)

##############
# Distribuzione Cumulativa Continua dati EU

to_plot_EU <- to_plot_EU_extraEU[to_plot_EU_extraEU$Region == "EU", ]

percentile_95 <- quantile(to_plot_EU$Value_transformed, 0.95, na.rm = TRUE)
k = 0.8
intervals <- seq(0, max(percentile_95, na.rm = TRUE),by = k)
FcumValueTransformed <- cumsum(table(cut(to_plot_EU$Value_transformed,
                                          breaks=intervals, right=FALSE)))/length(to_plot_EU$Value_transformed)
FcumValueTransformed<-c(0,FcumValueTransformed)

plot(intervals, FcumValueTransformed, type="b", axes=FALSE,
     main="Funzione di distribuzione empirica continua di value_transformed",
     col="blue",xlab="Classi",ylab="Frequenze cumulate")
axis(1, intervals, cex.axis=0.8)
axis(2, FcumValueTransformed, cex.axis=0.80,las = 1)
box()

##############
# Distribuzione Cumulativa Continua dati Non UE
to_plot_NonEU <- to_plot_EU_extraEU[to_plot_EU_extraEU$Region == "Non-EU", ]

percentile_95 <- quantile(to_plot_NonEU$Value_transformed, 0.95, na.rm = TRUE)
k = 2
intervals <- seq(0, max(percentile_95, na.rm = TRUE),by = k)
FcumValueTransformed <- cumsum(table(cut(to_plot_NonEU$Value_transformed,
                                         breaks=intervals, right=FALSE)))/length(to_plot_NonEU$Value_transformed)
FcumValueTransformed<-c(0,FcumValueTransformed)

plot(intervals, FcumValueTransformed, type="b", axes=FALSE,
     main="Funzione di distribuzione empirica continua di value_transformed",
     col="blue",xlab="Classi",ylab="Frequenze cumulate")
axis(1, intervals, cex.axis=0.8)
axis(2, FcumValueTransformed, cex.axis=0.80,las = 1)
box()

#VALUTARE SE FARE LA DISTRIBUZIONE DISCRETA INVECE CHE CONTINUA