install.packages("sf")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("haven")
install.packages("readr")
install.packages("readxl")
install.packages("giscoR")


library(sf)
library(ggplot2)
library(dplyr)
library(haven)
library(readr)
library(readxl)
library(giscoR)


# Lade die Skalometer-Daten aus der SPSS-Datei
ZA6803_v4 <- read_sav("~/Uni/Masterthesis/ZA6803_v4.sav")

# Filtere die Befragungen vom 23.09.2017 (1 Tag vor der Bundestagswahl)
#intdatum <- ZA6803_v4 %>% filter(intdatum == "2017-09-23")

# Entferne negative Werte (sind nicht einem Wahlkreis zugeordnet) aus den Skalometerdaten und erstelle einen neuen Datensatz
filtered_survey_data <- ZA6803_v4 %>%
  filter(pre028g >= 0)  # Behalte nur Werte >= 0


# Berechne den durchschnittlichen Skalometerwert der AfD pro Wahlkreis
afd_survey_summary <- filtered_survey_data %>% #intdatum %>%
  group_by(elecdist17) %>%
  summarize(avg_skalometer_afd = mean(pre028g, na.rm = TRUE))

# Umbenennen, um eine Variabke zu haben, die zusammengefügt werden kann
colnames(afd_survey_summary)[colnames(afd_survey_summary) == "elecdist17"] <- "WKR_NR"


# Wahlkreis-Koordinaten müssen für die Darstellung geladen werden
wahlkreise_sf <- st_read("~/Uni/Masterthesis/Wahlkreisdaten_Koordinaten_2017/generalisiert")  # Shapefile-Datei der Wahlkreise

# Verknüpfe die Skalometerdaten mit den Geometriedaten über die gemeinsame Variable 'WKR_NR'
wahlkreise_data <- wahlkreise_sf %>%
  left_join(afd_survey_summary, by = "WKR_NR")

laender <- gisco_get_nuts(country = "DEU", nuts_level = 1, resolution = 20)

ggplot(data = wahlkreise_data) +  
  geom_sf(aes(fill = avg_skalometer_afd), color = "black", size = 0.1) +  # Schwarze Umrandung
  geom_sf(data = laender, aes(fill = NA), col = "white", fill = NA) +
  scale_fill_gradient(
    low = "lightblue", 
    high = "darkblue", 
    na.value = "white",  # Weiß für fehlende Werte
    name = "Durchschnittlicher\nSkalometer-Wert",
    limits = c(1, 10),  # Setzt die Skala von 1 bis 10
    trans = "sqrt"  # Betonung auf kleinere Werte durch Wurzeltransformation
  ) +
  labs(title = "Durchschnittlicher Skalometer-Wert für AfD-Unterstützung pro Wahlkreis (2017)",
       caption = "Quelle: GLES (2022), ZA6803, GESIS, Köln. https://doi.org/10.4232/1.13948") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),         # Entfernt Achsentitel
    axis.text = element_blank(),          # Entfernt Achsentexte
    axis.ticks = element_blank(),         # Entfernt Achsenticks
    panel.grid = element_blank(),         # Entfernt das Gitternetz
    panel.background = element_rect(fill = "white", color = NA),  # Weißer Hintergrund
    plot.background = element_rect(fill = "white", color = NA)    # Weißer Hintergrund des gesamten Plots
  )

# Speichern
ggsave("~/Uni/Masterthesis/Plots/AfD_Skalometer.png", plot = last_plot(), width = 10, height = 8, dpi = 300)

###########################################
# Laden der Wahlergebnisse
wahlergebnisse <- read_csv2("~/Uni/Masterthesis/btw17_wahlkreis.csv")

# Umbenennen der Spalten
colnames(wahlergebnisse) <- c("Nr", "Gebiet", "gehört zu", "Wähler_Endgültig",
                              "Gültige_Endgültig", "AfD_Endgültig", "AfD_Prozent")

# Wähle relevante Spalten und bereinige AfD-Prozent-Spalte
wahlergebnisse <- wahlergebnisse %>%
  select(WKR_NAME = Gebiet, AfD_Prozent = AfD_Prozent) %>%
  mutate(AfD_Prozent = as.numeric(sub("%", "", AfD_Prozent)))

# Lade die Geometriedaten aus der Shapefile-Datei
wahlkreise_sf <- st_read("~/Uni/Masterthesis/Wahlkreisdaten_Koordinaten_2017/generalisiert")

# Verknüpfe die Wahlergebnisse mit den Geometriedaten
wahlkreise_data <- wahlkreise_sf %>%
  left_join(wahlergebnisse, by = "WKR_NAME")  # Verknüpft über den Wahlkreisnamen

laender <- gisco_get_nuts(country = "DEU", nuts_level = 1, resolution = 20)

# Erstelle die Karte
ggplot(data = wahlkreise_data) +
  geom_sf(aes(fill = AfD_Prozent), color = "black", size = 0.1) +
  geom_sf(data = laender, aes(fill = NA), col = "white", fill = NA) +
  scale_fill_gradient(
    low = "lightblue", 
    high = "darkblue", 
    na.value = "white",
    name = "AfD Wahlergebnis (%)") +
  labs(title = "AfD Wahlergebnis (Zweitstimme) nach Wahlkreis",
       caption = "Quelle: Bundeswahlleiter, BTW 2017") +
  theme_minimal() +
  theme(
    axis.title = element_blank(),         # Entfernt Achsentitel
    axis.text = element_blank(),          # Entfernt Achsentexte
    axis.ticks = element_blank(),         # Entfernt Achsenticks
    panel.grid = element_blank(),         # Entfernt das Gitternetz
    panel.background = element_rect(fill = "white", color = NA),  # Weißer Hintergrund
    plot.background = element_rect(fill = "white", color = NA)    # Weißer Hintergrund des gesamten Plots
  )

# Speichern
ggsave("~/Uni/Masterthesis/Plots/AfD_Wahlergebnisse.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
