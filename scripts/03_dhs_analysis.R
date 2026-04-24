#================================================
# Projet : Poverty Mapping Togo
# Script : 03 - Analyse données DHS
# Auteur : Womenyao Komla Sedzro EKLOU
# Date   : Avril 2026
# Objectif : Charger et préparer les données
#            DHS Togo 2017
#================================================

# Packages
library(tidyverse)
library(haven)
library(sf)
library(tmap)

#================================================
# 1. Chargement des données DHS
#================================================

# Données ménages
dhs_household <- haven::read_dta(
  "data/raw/dhs/TGHR71FL.DTA"
)

# Coordonnées GPS des clusters
dhs_gps <- sf::st_read(
  "data/raw/dhs/TGGE71FL.shp"
)

# Vérification
cat("Données DHS chargées :\n")
cat("- Ménages :", nrow(dhs_household), "\n")
cat("- Clusters GPS :", nrow(dhs_gps), "\n")


#================================================
# 2. Visualisation des clusters Togo
#================================================

tmap_mode("plot")

tm_shape(togo_prefectures_sf) +
  tm_fill(fill = "skyblue", fill_alpha = 0.5) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_shape(dhs_gps) +
  tm_dots(fill = "red", size = 0.1) +
  tm_title("Clusters DHS Togo 2017") +
  tm_compass(position = c("right", "top")) +
  tm_scalebar(position = c("left", "bottom"))


map_dhs_clusters <- tm_shape(togo_prefectures_sf) +
  tm_fill(fill = "skyblue", fill_alpha = 0.5) +
  tm_borders(col = "black", lwd = 0.5) +
  tm_shape(dhs_gps) +
  tm_dots(fill = "red", size = 0.1) +
  tm_title("Clusters DHS Togo 2017") +
  tm_compass(position = c("right", "top")) +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(
  map_dhs_clusters,
  filename = "outputs/maps/04_clusters_dhs_togo.png",
  dpi      = 300,
  width    = 8,
  height   = 10
)

cat("✓ Carte clusters DHS sauvegardée\n")






