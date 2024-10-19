library(dplyr)
library(readxl)

# Caricamento dei dati di mortalità
data <- read_xlsx("Mortality.xlsx")
names(data)[names(data) == "Variable"] <- "Kind of death"

# Visualizza un sommario dei dati
summary(data)

# Funzione per mappare i paesi EU e Non-EU
map_region <- function(df) {
  eu_countries <- c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
                    "Czech Republic", "Denmark", "Estonia", "Finland", 
                    "France", "Germany", "Greece", "Hungary", "Ireland", 
                    "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta", 
                    "Netherlands", "Poland", "Portugal", "Romania", "Slovakia", 
                    "Slovenia", "Spain", "Sweden")
  
  df %>% mutate(Region = ifelse(Country %in% eu_countries, "EU", "Non-EU"))
}

# Caricamento dei dati di mortalità infantile
data_infant <- read_xlsx("Infant_Mortality.xlsx")

# Applicazione della funzione di mappatura dei paesi
data_infant <- map_region(data_infant)

# Definizione della funzione di mapping per uniformare i dati
map_values <- function(value, measure) {
  mapped_value <- case_when(
    measure == "Deaths per 100 000 live births" ~ value * 1000 / 100000,
    measure == "Deaths per 1 000 live births" ~ value,
    TRUE ~ NA_real_  # Restituiamo NA se nessuna delle condizioni è soddisfatta
  )
  return(mapped_value)
}

# Applica la funzione di mapping ai dati
data_infant <- data_infant %>%
  mutate(Value_transformed = map_values(Value, Measure))

# Filtra i dati con valori trasformati non nulli
to_plot <- data_infant %>%
  filter(!is.na(Value_transformed))

# Calcolo della distribuzione di frequenza assoluta per tipo di morte, separata per regioni (UE e Non-UE)
frequency_absolute <- to_plot %>%
  group_by(Region, Variable) %>%
  summarise(Frequency_Absolute = n(),
            Total_Deaths = sum(Value_transformed, na.rm = TRUE),
            .groups = 'drop') %>%
  arrange(Region, Frequency_Absolute)  # Ordinamento in ordine crescente

# Calcolo della distribuzione di frequenza relativa
frequency_relative <- frequency_absolute %>%
  group_by(Region) %>%
  mutate(Frequency_Relative = Frequency_Absolute / sum(Frequency_Absolute))

# Funzione per creare etichette multilinea
create_multiline_labels <- function(labels, width = 20) {
  sapply(labels, function(x) {
    if (nchar(x) > width) {
      return(paste(strwrap(x, width = width), collapse = "\n"))
    } else {
      return(x)
    }
  })
}

# Crea etichette multilinea per i grafici
multiline_labels <- create_multiline_labels(frequency_absolute$Variable, width = 20)

# Imposta margini per dare più spazio alle etichette
par(mar = c(12, 5, 5, 2))

# Grafico della distribuzione di frequenza assoluta per i paesi UE
barplot(frequency_absolute$Frequency_Absolute[frequency_absolute$Region == "EU"],
        names.arg = multiline_labels[frequency_absolute$Region == "EU"],  
        main = "Distribuzione di Frequenza Assoluta per Tipo di Morte (Paesi UE)",
        ylab = "Frequenza Assoluta",
        col = "steelblue",
        las = 2,
        cex.names = 0.6,
        space = 0.5,
        ylim = c(0, max(frequency_absolute$Frequency_Absolute) * 1.1))

mtext("Tipo di Morte", side = 1, line = 10)

# Grafico per paesi Non-UE
barplot(frequency_absolute$Frequency_Absolute[frequency_absolute$Region == "Non-EU"],
        names.arg = multiline_labels[frequency_absolute$Region == "Non-EU"],  
        main = "Distribuzione di Frequenza Assoluta per Tipo di Morte (Paesi Non-UE)",
        ylab = "Frequenza Assoluta",
        col = "lightblue",
        las = 2,
        cex.names = 0.6,
        space = 0.5,
        ylim = c(0, max(frequency_absolute$Frequency_Absolute) * 1.1))

mtext("Tipo di Morte", side = 1, line = 10)

# Crea etichette multilinea per i grafici
multiline_labels <- create_multiline_labels(frequency_absolute$Variable, width = 20)

# Calcolo della distribuzione di frequenza assoluta per anno (2010-2015)
data_infant_2010_2015 <- to_plot %>%
  filter(Year >= 2010 & Year <= 2015) %>%
  group_by(Region, Variable) %>%
  summarise(Frequency_Absolute = n(),
            Total_Deaths = sum(Value_transformed, na.rm = TRUE),
            .groups = 'drop') %>%
  arrange(Region, Frequency_Absolute)

# Ordinamento dei tipi di morte per la colonna Variable in base alla frequenza assoluta
data_infant_2010_2015$Variable <- factor(data_infant_2010_2015$Variable, 
                                         levels = data_infant_2010_2015[data_infant_2010_2015$Region == "EU", ]$Variable[order(data_infant_2010_2015[data_infant_2010_2015$Region == "EU", ]$Frequency_Absolute)])

# Grafico della distribuzione di frequenza assoluta per tipo di morte per i paesi UE (2010-2015)
barplot(Frequency_Absolute ~ Variable, 
        data = data_infant_2010_2015[data_infant_2010_2015$Region == "EU", ],
        names.arg = multiline_labels[frequency_absolute$Region == "EU"],
        main = "Distribuzione di Frequenza Assoluta per Tipo di Morte (Paesi UE, 2010-2015)",
        ylab = "Frequenza Assoluta",
        col = "steelblue",
        las = 2,
        cex.names = 0.6,
        space = 0.5,
        ylim = c(0, max(data_infant_2010_2015$Frequency_Absolute) * 1.1))

# Grafico per paesi Non-UE (2010-2015)
data_infant_2010_2015$Variable <- factor(data_infant_2010_2015$Variable, 
                                         levels = data_infant_2010_2015[data_infant_2010_2015$Region == "Non-EU", ]$Variable[order(data_infant_2010_2015[data_infant_2010_2015$Region == "Non-EU", ]$Frequency_Absolute)])

barplot(Frequency_Absolute ~ Variable, 
        data = data_infant_2010_2015[data_infant_2010_2015$Region == "Non-EU", ]
        names.arg = multiline_labels[frequency_absolute$Region == "Non-EU"],
        main = "Distribuzione di Frequenza Assoluta per Tipo di Morte (Paesi Non-UE, 2010-2015)",
        ylab = "Frequenza Assoluta",
        col = "lightblue",
        las = 2,
        cex.names = 0.6,
        space = 0.5,
        ylim = c(0, max(data_infant_2010_2015$Frequency_Absolute) * 1.1))

# Calcolo della distribuzione di frequenza assoluta per anno (2016-2022)
data_infant_2016_2022 <- to_plot %>%
  filter(Year >= 2016 & Year <= 2022) %>%
  group_by(Region, Variable) %>%
  summarise(Frequency_Absolute = n(),
            Total_Deaths = sum(Value_transformed, na.rm = TRUE),
            .groups = 'drop') %>%
  arrange(Region, Frequency_Absolute)

# Ordinamento dei tipi di morte per la colonna Variable in base alla frequenza assoluta
data_infant_2016_2022$Variable <- factor(data_infant_2016_2022$Variable, 
                                         levels = data_infant_2016_2022[data_infant_2016_2022$Region == "EU", ]$Variable[order(data_infant_2016_2022[data_infant_2016_2022$Region == "EU", ]$Frequency_Absolute)])

# Grafico della distribuzione di frequenza assoluta per tipo di morte per i paesi UE (2016-2022)
barplot(Frequency_Absolute ~ Variable, 
        data = data_infant_2016_2022[data_infant_2016_2022$Region == "EU", ],
        names.arg = multiline_labels[frequency_absolute$Region == "EU"],
        main = "Distribuzione di Frequenza Assoluta per Tipo di Morte (Paesi UE, 2016-2022)",
        ylab = "Frequenza Assoluta",
        col = "steelblue",
        las = 2,
        cex.names = 0.6,
        space = 0.5,
        ylim = c(0, max(data_infant_2016_2022$Frequency_Absolute) * 1.1))

# Grafico per paesi Non-UE (2016-2022)
data_infant_2016_2022$Variable <- factor(data_infant_2016_2022$Variable, 
                                         levels = data_infant_2016_2022[data_infant_2016_2022$Region == "Non-EU", ]$Variable[order(data_infant_2016_2022[data_infant_2016_2022$Region == "Non-EU", ]$Frequency_Absolute)])

barplot(Frequency_Absolute ~ Variable, 
        data = data_infant_2016_2022[data_infant_2016_2022$Region == "Non-EU", ],
        names.arg = multiline_labels[frequency_absolute$Region == "Non-EU"],
        main = "Distribuzione di Frequenza Assoluta per Tipo di Morte (Paesi Non-UE, 2016-2022)",
        ylab = "Frequenza Assoluta",
        col = "lightblue",
        las = 2,
        cex.names = 0.6,
        space = 0.5,
        ylim = c(0, max(data_infant_2016_2022$Frequency_Absolute) * 1.1))
