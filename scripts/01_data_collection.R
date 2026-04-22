
#================================================
# Projet : Poverty Mapping Togo
# Script : 01 - Collecte des données
# Auteur : Komla Sedzro EKLOU
# Date   : Avril 2026
# Objectif : Télécharger les données géographiques
#            et satellitaires pour le Togo
#================================================

# Chargement des packages
library(tidyverse)
library(sf)
library(terra)
library(tmap)
library(geodata)
library(tidyterra)
library(viridis)


#================================================
# 1. Téléchargement des frontières du Togo
#================================================

# Frontières nationales (niveau 0)
togo_country <- geodata::gadm(
  country = "TGO",
  level   = 0,
  path    = "data/raw"
)

# Frontières des régions (niveau 1)
togo_regions <- geodata::gadm(
  country = "TGO",
  level   = 1,
  path    = "data/raw"
)

# Frontières des préfectures (niveau 2)
togo_prefectures <- geodata::gadm(
  country = "TGO",
  level   = 2,
  path    = "data/raw"
)

# Vérification
print(togo_country)
print(togo_regions)
print(togo_prefectures)





#================================================
# 2. Visualisation rapide de vérification
#================================================

# Convertir en sf pour tmap
togo_regions_sf     <- sf::st_as_sf(togo_regions)
togo_prefectures_sf <- sf::st_as_sf(togo_prefectures)
togo_country_sf     <- sf::st_as_sf(togo_country)

# Carte rapide
tmap_mode("plot")

tm_shape(togo_prefectures_sf) +
  tm_fill(col = "lightblue", alpha = 0.5) +
  tm_borders(col = "white", lwd = 0.5) +
  tm_shape(togo_regions_sf) +
  tm_borders(col = "darkblue", lwd = 1.5) +
  tm_shape(togo_country_sf) +
  tm_borders(col = "black", lwd = 2) +
  tm_layout(
    title          = "Préfectures du Togo",
    title.size     = 1.2,
    title.position = c("center", "top"),
    inner.margins  = c(0.05, 0.05, 0.1, 0.05)
  ) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))












