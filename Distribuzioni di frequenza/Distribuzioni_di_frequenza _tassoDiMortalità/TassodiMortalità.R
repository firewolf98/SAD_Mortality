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

summary(data_infant)

# Calcolo della distribuzione di frequenza assoluta aggregata per ogni tipo di morte (tutti i paesi assieme)
frequency_absolute <- to_plot %>%
  group_by(Variable) %>%  # Raggruppa solo per 'Variable' (tipo di morte)
  summarise(Frequency_Absolute = n(),
            Total_Deaths = sum(Value_transformed, na.rm = TRUE),
            .groups = 'drop') %>%
  arrange(Frequency_Absolute)  # Ordina per frequenza crescente

# Calcolo della distribuzione di frequenza relativa per ciascun tipo di morte
frequency_relative <- frequency_absolute %>%
  mutate(Frequency_Relative = Frequency_Absolute / sum(Frequency_Absolute))

# Visualizza i dati risultanti
print(frequency_absolute)
print(frequency_relative)


# Funzione per creare etichette multilinea
create_multiline_labels <- function(labels, width = 20) {
  sapply(labels, function(x) {
    if (nchar(x) > width) {
      return(paste(strwrap(x, width = width), collapse = "\n"))  # Collassa le righe in una stringa
    } else {
      return(x)
    }
  })
}

# Crea le etichette multilinea
multiline_labels <- create_multiline_labels(frequency_absolute$Variable, width = 20)

# Imposta i margini per dare più spazio alle etichette
par(mar = c(12, 5, 5, 2))  # Maggiore margine inferiore

# Grafico della distribuzione di frequenza assoluta per ogni tipo di morte
barplot(frequency_absolute$Frequency_Absolute,
        names.arg = multiline_labels,  # Usa le etichette multilinea
        main = "Distribuzione di Frequenza Assoluta per Tipo di Morte (Tutti i Paesi)",
        ylab = "Frequenza Assoluta",
        col = "steelblue",
        las = 2,  # Ruota le etichette di 90 gradi (verticale)
        cex.names = 0.6,  # Dimensione delle etichette
        space = 0.5,  # Aggiungi spazio tra le barre
        ylim = c(0, max(frequency_absolute$Frequency_Absolute) * 1.1))  # Aggiungi un po' di spazio in alto

# Aggiungi un mtext per l'etichetta dell'asse x
mtext("Tipo di Morte", side = 1, line = 10)  # Sposta l'etichetta più vicina

# Grafico della distribuzione di frequenza relativa per ogni tipo di morte
barplot(frequency_relative$Frequency_Relative,
        names.arg = multiline_labels,  # Usa le etichette multilinea
        main = "Distribuzione di Frequenza Relativa per Tipo di Morte (Tutti i Paesi)",
        ylab = "Frequenza Relativa",
        col = "lightgreen",
        las = 2,  # Ruota le etichette di 90 gradi (verticale)
        cex.names = 0.6,  # Dimensione delle etichette
        space = 0.5,  # Aggiungi spazio tra le barre
        ylim = c(0, max(frequency_relative$Frequency_Relative) * 1.1))  # Aggiungi un po' di spazio in alto

# Aggiungi un mtext per l'etichetta dell'asse x
mtext("Tipo di Morte", side = 1, line = 10)  # Sposta l'etichetta più vicina
