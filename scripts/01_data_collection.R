
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



#================================================
# 3. Téléchargement données climatiques
#    Précipitations WorldClim
#================================================

# Précipitations moyennes mensuelles (WorldClim)
togo_precip <- geodata::worldclim_country(
  country  = "TGO",
  var      = "prec",
  path     = "data/raw"
)

# Température moyenne
togo_temp <- geodata::worldclim_country(
  country  = "TGO",
  var      = "tavg",
  path     = "data/raw"
)

# Vérification
print(togo_precip)
print(togo_temp)


#================================================
# 4. Visualisation des données climatiques
#================================================

# Précipitations moyennes annuelles
togo_precip_annual <- mean(togo_precip)

# Carte
tmap_mode("plot")

map_precip <- tm_shape(togo_precip_annual) +
  tm_raster(
    palette = "Blues",
    title   = "Précipitations (mm)"
  ) +
  tm_shape(togo_regions_sf) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(
    main.title          = "Précipitations moyennes annuelles - Togo",
    main.title.size     = 1,
    main.title.position = "center",
    legend.outside      = TRUE
  ) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))

#================================================
# 5. Téléchargement données topographiques
#    Altitude SRTM
#================================================

# Altitude du Togo
togo_elevation <- geodata::elevation_30s(
  country = "TGO",
  path    = "data/raw"
)

# Vérification
print(togo_elevation)

# Carte

tmap_mode("plot")

map_elevation <- tm_shape(togo_elevation) +
  tm_raster(
    palette = "terrain",
    title  = "Altitude du Togo (m)"
  ) +
  tm_shape(togo_regions_sf)+
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(
    main.title          = "Topographie du Togo",
    main.title.size     = 1,
    main.title.position = "center",
    legend.outside      = TRUE
  ) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))


#================================================
# 6. Téléchargement données de populations
#    Année 2020
#================================================

# Téléchargement direct WorldPop
url <- "https://data.worldpop.org/GIS/Population/Global_2000_2020/2020/TGO/tgo_ppp_2020.tif"

download.file(
  url      = url,
  destfile = "data/raw/togo_population_2020.tif",
  mode     = "wb"
)

# Charger le fichier téléchargé
Togo_population <- terra::rast("data/raw/togo_population_2020.tif")

# Vérification

print(Togo_population)


#================================================
# . Visualisation des données de population
#================================================

tmap_mode("plot")

map_population <-tm_shape(Togo_population) +
  tm_raster(
    palette = "YlOrRd",
    title = "population du togo (2020)"
  ) +
  tm_shape(togo_regions_sf) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(
    main.title = "Répartition de la population du Togo",
    main.title.size = 1,
    main.title.position = "center",
    legend.outside = TRUE
  ) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom"))
  
  
  ## graphique avec échelle logarithmique 
  
tmap_mode("plot")
tm_shape(log1p(Togo_population)) +
  tm_raster(
    palette = "YlOrRd",
    title   = "Population log(habitants/km²)"
  ) +
  tm_shape(togo_regions_sf) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_layout(
    main.title          = "Répartition de la population du Togo (échelle log)",
    main.title.size     = 1,
    main.title.position = "center",
    legend.outside      = TRUE
  ) +
  tm_compass(position = c("right", "top")) +
  tm_scale_bar(position = c("left", "bottom")) 


#================================================
# 7. Export des cartes
#================================================

tmap_save(
  map_precip,
  filename = "outputs/maps/01_precipitations_togo.png",
  dpi      = 300,
  width    = 8,
  height   = 10
)


tmap_save(
  map_elevation,
  filename = "outputs/maps/02_elevation_togo.png",
  dpi      = 300,
  width    = 8,
  height   = 10
)

tmap_save(
  map_population,
  filename = "outputs/maps/03_population_togo.png",
  dpi      = 300,
  width    = 8,
  height   = 10
)



