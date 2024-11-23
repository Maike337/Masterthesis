install.packages("haven")
install.packages("ggplot2")
install.packages(c("dplyr", "sf", "tmap"))
library(ggplot2)
library(haven)
library(dplyr)
library(sf)
library(tmap)

# Laden der Wahlkreisdaten von 2017
wahlkreise_2017 <- read_sav("~/Uni/Masterthesis/ZA6803_v4.sav")

# Laden der Koordinatendaten für Preußen 1866
preussen_1866 <- read_dta("~/Uni/Masterthesis/Dataverse_files_Haffert/coord_prussia.dta") %>%
  as.data.frame() %>%  # Umwandeln in Dataframe
  
  # Umbenennen der Koordinatenspalten für Klarheit
  rename(Längen = `_X`, Breitengrad = `_Y`) %>%
  
  # Entfernen von NA-Werten in den Koordinaten
  filter(complete.cases(Längen, Breitengrad))

# Konvertieren der Preußen-Daten in ein sf-Objekt für Geodatenanalyse
preussen_sf <- st_as_sf(preussen_1866, coords = c("Längen", "Breitengrad"), crs = 4326)

ggplot(data = preussen_sf) +
  geom_sf(color = "blue", fill = NA, size = 0.5) +
  labs(title = "Historische Preußengrenzen (1866)",
       x = "Longitude",
       y = "Latitude") +
  coord_sf() +
  theme_minimal()
###########################
preussen_df <- read_dta("~/Uni/Masterthesis/Dataverse_files_Haffert/coord_prussia.dta")
preussen_clean <- preussen_df %>%
  filter(!is.na(`_X`) & !is.na(`_Y`))

# Umbenennen der Spalten (optional, für mehr Klarheit)
names(preussen_clean)[names(preussen_clean) == "_X"] <- "Longitude"
names(preussen_clean)[names(preussen_clean) == "_Y"] <- "Latitude"
preussen_sf <- st_as_sf(preussen_clean, coords = c("Longitude", "Latitude"), crs = 25832)

# Transformiere das CRS auf WGS 84 (EPSG:4326), um eine korrekte Darstellung zu erzielen
preussen_sf_transformed <- st_transform(preussen_sf, crs = 4326)

# Visualisierung der Preußengrenzen mit ggplot2
ggplot(preussen_sf_transformed) +
  geom_sf(color = "blue", size = 0.5) +
  labs(title = "Historische Preußengrenzen (1866)",
       x = "Longitude",
       y = "Latitude") +
  theme_minimal()+
  theme(
    panel.background = element_rect(fill = "white", color = NA),  # Weißer Hintergrund
    plot.background = element_rect(fill = "white", color = NA)    # Weißer Rand um den Plot
  )
ggsave("~/Uni/Masterthesis/Plots/Preußengrenzen_1866.png", plot = last_plot(), width = 10, height = 8, dpi = 300)
############
#Brauch ich nicht, bzw. funktioniert nicht
# Laden der Wahlkreis-Shapefile mit Koordinaten von 2017
wahlkreise_shp <- st_read("~/Uni/Masterthesis/Wahlkreisdaten_Koordinaten_2017/generalisiert/Geometrie_Wahlkreise_19DBT.shp")

# Transformieren der CRS von Preußen-Daten auf die CRS der Wahlkreisdaten
preussen_sf <- st_transform(preussen_sf, crs = st_crs(wahlkreise_shp))

# (OPTIONAL) Überprüfen der Geometrietypen beider Datensätze
# st_geometry_type(wahlkreise_shp)
# st_geometry_type(preussen_sf)

# Spatial Join der Wahlkreise und Preußen-Daten, um Überschneidungen zu erkennen
wahlkreise_preussen <- st_join(wahlkreise_shp, preussen_sf, join = st_intersects) %>%
  
  # Hinzufügen einer neuen Spalte, die die Lage des Wahlkreises angibt
  mutate(preussen_flag = ifelse(!is.na(`_ID`), "innerhalb", "außerhalb"))

# Visualisierung der Wahlkreise in Bezug auf ihre Lage zu Preußen 1866
ggplot(data = wahlkreise_preussen) +
  geom_sf(aes(fill = preussen_flag), color = "black") +
  scale_fill_manual(values = c("innerhalb" = "blue", "außerhalb" = "red")) +
  labs(title = "Wahlkreise 2017 in Bezug auf Preußen 1866",
       fill = "Lage") +
  theme_minimal()






#######################################################################
# Installiere und lade notwendige Pakete
install.packages("MASS")
library(MASS)

# Hypothese 1: Unterschiede in der AfD-Unterstützung zwischen katholischer Bevölkerung in benachteiligten und nicht benachteiligten Gebieten
# Abhängige Variable: AfD-Unterstützung (Skalometer)
# Unabhängige Variablen: Katholisch (ja/nein), Wohnort (benachteiligtes Gebiet: ja/nein)
# Methode: Ordinale Regression

model1 <- polr(as.ordered(pre028g) ~ pre120 + disadvantaged_area, data = dataset, Hess=TRUE)
summary(model1)  # Ergebnisse anzeigen
exp(cbind(OR = coef(model1), confint(model1)))  # Konfidenzintervalle berechnen


# Hypothese 2: Unterschiede im Mittelwert und der Varianz der AfD-Unterstützung zwischen katholischer Bevölkerung und Gesamtbevölkerung (2017)
# Methode: t-Test für Mittelwertvergleich, ANOVA für Varianzvergleich nach Region

# Mittelwertvergleich (katholische vs. allgemeine Bevölkerung)
t_test_result <- t.test(pre028g ~ pre120, data = dataset)
print(t_test_result)

# Varianzvergleich nach Region
anova_result <- aov(pre028g ~ region, data = dataset)
summary(anova_result)


# Hypothese 3: Erklärung der AfD-Unterstützung innerhalb der katholischen Bevölkerung
# Methode: Multiple lineare Regression (OLS)
# Abhängige Variable: AfD-Unterstützung
# Unabhängige Variablen: Einkommen, Alter, Bildung

# Subset der Daten für katholische Bevölkerung
katholisch_data <- subset(dataset, pre120 == "Yes")

# OLS Regression
model2 <- lm(pre028g ~ income + pre101 + pre103, data = katholisch_data)
summary(model2)  # Ergebnisse anzeigen


# Hypothese 4: Katholisch sein als Faktor für AfD-Unterstützung
# Methode: Chi-Quadrat-Test
# Untersucht Zusammenhang zwischen Katholischsein und AfD-Unterstützungskategorie (Zustimmung/Ablehnung)

# Neue Variable für Zustimmung/Ablehnung basierend auf Skalometer-Werten
dataset$afd_support_category <- ifelse(pre028g > 0, "Zustimmung", "Ablehnung")

# Chi-Quadrat-Test für Katholischsein und AfD-Unterstützungskategorie
chi_test_result <- chisq.test(table(dataset$afd_support_category, dataset$pre120))
print(chi_test_result)


# Optional: Kruskal-Wallis-Test für nichtparametrische Varianzuntersuchung
# Falls Unterschiede in der AfD-Unterstützung zwischen Altersgruppen und Bildungsniveaus bestehen

# Unterschiede in der AfD-Unterstützung nach Altersgruppen
kruskal_test_age <- kruskal.test(pre028g ~ pre101, data = dataset)
print(kruskal_test_age)

# Unterschiede in der AfD-Unterstützung nach Bildungsniveau
kruskal_test_education <- kruskal.test(pre028g ~ pre103, data = dataset)
print(kruskal_test_education)

