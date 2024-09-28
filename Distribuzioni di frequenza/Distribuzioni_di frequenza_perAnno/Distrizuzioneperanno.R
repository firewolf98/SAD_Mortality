library(dplyr)
library(readxl)

# Caricamento e preparazione dei dati
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

# Filtra i dati per gli anni 2010-2015 e 2016-2022
data_2010_2015 <- mapped_data %>%
  filter(Year >= 2010 & Year <= 2015)

data_2016_2022 <- mapped_data %>%
  filter(Year >= 2016 & Year <= 2022)

# Funzione per calcolare frequenze
calculate_frequencies <- function(data) {
  frequency_absolute <- data %>%
    group_by(Variable) %>%  # Assicurati di usare "Variable"
    summarise(Frequency_Absolute = n(),
              Total_Deaths = sum(Value_transformed, na.rm = TRUE),
              .groups = 'drop') %>%
    arrange(Frequency_Absolute)
  
  frequency_relative <- frequency_absolute %>%
    mutate(Frequency_Relative = Frequency_Absolute / sum(Frequency_Absolute))
  
  return(list(frequency_absolute, frequency_relative))
}

# Calcola le frequenze per entrambi i periodi
frequencies_2010_2015 <- calculate_frequencies(data_2010_2015)
frequencies_2016_2022 <- calculate_frequencies(data_2016_2022)

# Funzione per spezzare le etichette lunghe
break_labels <- function(labels, width) {
  sapply(labels, function(label) {
    if (nchar(label) > width) {
      strsplit(label, " ")[[1]] %>% 
        paste(collapse = "\n")  # Unisci con a capo
    } else {
      label
    }
  })
}

# Imposta i margini per dare più spazio alle etichette
par(mar = c(8, 4, 4, 2))  # Maggiore margine inferiore

# Grafico della distribuzione di frequenza assoluta per anno 2010-2015
barplot(frequencies_2010_2015[[1]]$Frequency_Absolute,
        names.arg = break_labels(frequencies_2010_2015[[1]]$Variable, 20),  # Specifica larghezza
        main = "Distribuzione di Frequenza Assoluta (2010-2015)",
        ylab = "Frequenza Assoluta",
        col = "steelblue",
        las = 2,
        cex.names = 0.7,
        space = 0.5,
        ylim = c(0, max(frequencies_2010_2015[[1]]$Frequency_Absolute) * 1.1))

mtext("Tipo di Morte", side = 1, line = 8)

# Grafico della distribuzione di frequenza relativa per anno 2010-2015
barplot(frequencies_2010_2015[[2]]$Frequency_Relative,
        names.arg = break_labels(frequencies_2010_2015[[1]]$Variable, 20),  # Specifica larghezza
        main = "Distribuzione di Frequenza Relativa (2010-2015)",
        ylab = "Frequenza Relativa",
        col = "lightgreen",
        las = 2,
        cex.names = 0.7,
        space = 0.5,
        ylim = c(0, max(frequencies_2010_2015[[2]]$Frequency_Relative) * 1.1))

mtext("Tipo di Morte", side = 1, line = 8)

# Ripeti per il periodo 2016-2022
par(mar = c(8, 4, 4, 2))  # Maggiore margine inferiore per il secondo grafico

# Grafico della distribuzione di frequenza assoluta per anno 2016-2022
barplot(frequencies_2016_2022[[1]]$Frequency_Absolute,
        names.arg = break_labels(frequencies_2016_2022[[1]]$Variable, 20),  # Specifica larghezza
        main = "Distribuzione di Frequenza Assoluta (2016-2022)",
        ylab = "Frequenza Assoluta",
        col = "steelblue",
        las = 2,
        cex.names = 0.7,
        space = 0.5,
        ylim = c(0, max(frequencies_2016_2022[[1]]$Frequency_Absolute) * 1.1))

mtext("Tipo di Morte", side = 1, line = 8)

# Grafico della distribuzione di frequenza relativa per anno 2016-2022
barplot(frequencies_2016_2022[[2]]$Frequency_Relative,
        names.arg = break_labels(frequencies_2016_2022[[1]]$Variable, 20),  # Specifica larghezza
        main = "Distribuzione di Frequenza Relativa (2016-2022)",
        ylab = "Frequenza Relativa",
        col = "lightgreen",
        las = 2,
        cex.names = 0.7,
        space = 0.5,
        ylim = c(0, max(frequencies_2016_2022[[2]]$Frequency_Relative) * 1.1))

mtext("Tipo di Morte", side = 1, line = 8)
