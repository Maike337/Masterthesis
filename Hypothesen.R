library(sf)
library(ggplot2)
library(dplyr)
library(haven)
library(readr)
library(readxl)
library(stats)

#Zusammen´hang zwischen Skalometewert und Religionszugehörigkeit (kath=1) ist nicht signifikant
# Lade die Skalometer-Daten aus der SPSS-Datei
ZA6803_v4 <- read_sav("~/Uni/Masterthesis/ZA6803_v4.sav")


#Dummy-Variable 'katholisch' basierend auf der Variable pre120
#1=röm-kath, 2,3,4,5,-98 und-99 nicht  
ZA6803_v4 <- ZA6803_v4 %>%
  mutate(katholisch = ifelse(pre120 == 1, 1, 0))

# Entferne negative Werte (sind nicht einem Wahlkreis zugeordnet) aus den Skalometerdaten und erstelle einen neuen Datensatz
filtered_survey_data <- ZA6803_v4 %>%
  filter(pre028g >= 0)  # Behalte nur Werte >= 0


# Berechne den durchschnittlichen Skalometerwert der AfD pro Wahlkreis
avg_skalometer <- filtered_survey_data %>%
  group_by(elecdist17) %>%
  summarize(avg_skalometer_afd = mean(pre028g, na.rm = TRUE))

# Verknüpfe die Skalometerdaten mit den katholisch-Informationen auf Wahlkreisebene
# Erstelle einen kombinierten Datensatz
combined_data <- avg_skalometer %>%
  left_join(filtered_survey_data %>% select(elecdist17, katholisch) %>% distinct(), 
            by = "elecdist17")

# OLS-Regression durch
ols_modell <- lm(avg_skalometer_afd ~ katholisch, data = combined_data)

# Zusammenfassung der OLS-Regression anzeigen
summary(ols_modell)


#############################################################################
#Skalometerwert der Katholiken in Bayern ist höher als der in NRW

# Lade die Skalometer-Daten aus der SPSS-Datei
ZA6803_v4 <- read_sav("~/Uni/Masterthesis/ZA6803_v4.sav")

# Filtere die Daten für Katholiken
katholiken_data <- combined_data %>% 
  filter(katholisch == 1)

# Berechne den durchschnittlichen Skalometerwert nur für Katholiken
katholiken_data <- ZA6803_v4 %>%
  filter(katholisch == 1) %>%  # Nur Katholiken
  filter(pre028g > 0) %>%  # Entferne negative Skalometerwerte
  group_by(elecdist17) %>%
  summarize(avg_skalometer_afd = mean(pre028g, na.rm = TRUE))

# Füge die Bundesland-Information hinzu basierend auf den Wahlkreisnummern
wahlkreise_data <- katholiken_data %>%
  mutate(Bundesland = case_when(
    elecdist17 >= 87 & elecdist17 <= 150 ~ "Nordrhein-Westfalen",
    elecdist17 >= 212 & elecdist17 <= 257 ~ "Bayern",
    TRUE ~ "Andere"
  ))

# Filtere nur die Wahlkreise aus Bayern und Nordrhein-Westfalen
katholiken_bundesland_data <- wahlkreise_data %>%
  filter(Bundesland %in% c("Bayern", "Nordrhein-Westfalen"))

# Führe die OLS-Regression durch
ols_modell_bundesland <- lm(avg_skalometer_afd ~ Bundesland, data = katholiken_bundesland_data)

# Zeige die Ergebnisse an
summary(ols_modell_bundesland)

##################################################################
library(car)  # Für Levene-Test (Vorraussetzung ANOVA)

# Lade die Skalometer-Daten aus der SPSS-Datei
ZA6803_v4 <- read_sav("~/Uni/Masterthesis/ZA6803_v4.sav")

# Filtere negative Werte und erstelle eine Dummy-Variable für Katholisch
filtered_data <- ZA6803_v4 %>%
  filter(pre028g >= 0) %>% # Behalte nur nicht-negative Skalometerwerte
  mutate(katholisch = ifelse(pre120 == 1, 1, 0)) # Dummy-Variable für Katholisch (1=katholisch, 0=andere Religionen)

# Füge Bundesland-Information basierend auf den Wahlkreisnummern hinzu
wahlkreise_data <- filtered_data %>%
  mutate(Bundesland = case_when(
    elecdist17 >= 87 & elecdist17 <= 150 ~ "Nordrhein-Westfalen",
    elecdist17 >= 212 & elecdist17 <= 257 ~ "Bayern",
    TRUE ~ "Andere"
  ))

# Filtere Daten für Bayern und NRW
bundesland_data <- wahlkreise_data %>%
  filter(Bundesland %in% c("Bayern", "Nordrhein-Westfalen"))

# T-Test: Vergleich der durchschnittlichen Skalometerwerte innerhalb Bayerns
t_test_bayern <- t.test(pre028g ~ katholisch, data = filter(bundesland_data, Bundesland == "Bayern"))
print(t_test_bayern)

# T-Test: Vergleich der durchschnittlichen Skalometerwerte innerhalb von NRW
t_test_nrw <- t.test(pre028g ~ katholisch, data = filter(bundesland_data, Bundesland == "Nordrhein-Westfalen"))
print(t_test_nrw)

# ANOVA: Vergleich zwischen Bayern und NRW
anova_result <- aov(pre028g ~ Bundesland, data = bundesland_data)
summary(anova_result)

# Visualisierung der Ergebnisse
ggplot(bundesland_data, aes(x = Bundesland, y = pre028g)) +
  geom_boxplot() +
  labs(title = "Vergleich der Skalometerwerte zwischen Bayern und NRW",
       x = "Bundesland", y = "Durchschnittlicher Skalometerwert")

# Speichern
ggsave("~/Uni/Masterthesis/Plots/AfD_ANOVA.png", plot = last_plot(), width = 10, height = 8, dpi = 300)


#####################################################################

# Lade die Daten
ZA6803_v4 <- read_sav("~/Uni/Masterthesis/ZA6803_v4.sav")

# Erstelle die Dummy-Variable 'katholisch' (1 = katholisch, 0 = nicht katholisch)
ZA6803_v4 <- ZA6803_v4 %>%
  mutate(
    katholisch = ifelse(pre120 == 1, 1, 0)
  )

# Filtere die Daten: nur katholische Bevölkerung und positive Skalometerwerte
katholiken_data <- ZA6803_v4 %>%
  filter(katholisch == 1, pre028g >= 0) %>%  # Behalte nur positive Skalometerwerte
  select(pre028g, pre102, pre101_alter, pre103)  # Behalte nur die relevanten Variablen

# Umbenennen der Variablen für Klarheit
katholiken_data <- katholiken_data %>%
  rename(
    afd_skalometer = pre028g,
    geschlecht = pre102,
    alter = pre101_alter,
    bildung = pre103
  )

# Multiple lineare Regression
ols_model <- lm(afd_skalometer ~ geschlecht + alter + bildung, data = katholiken_data)

# Zusammenfassung des Modells anzeigen
summary(ols_model)
