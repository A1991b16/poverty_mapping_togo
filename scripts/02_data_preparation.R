#================================================
# Projet : Poverty Mapping Togo
# Script : 02 - Préparation des données
# Auteur : Womenyao Komla Sedzro EKLOU
# Date   : Avril 2026
# Objectif : Nettoyer et préparer les données
#            pour le modèle ML
#================================================

# Chargement des packages
library(tidyverse)
library(sf)
library(terra)
library(tmap)
library(tidyterra)
library(viridis)

#================================================
# 1. Chargement des données déjà téléchargées
#================================================

# Frontières
togo_country      <- geodata::gadm("TGO", level = 0, path = "data/raw")
togo_regions      <- geodata::gadm("TGO", level = 1, path = "data/raw")
togo_prefectures  <- geodata::gadm("TGO", level = 2, path = "data/raw")

# Conversion en sf
togo_country_sf     <- sf::st_as_sf(togo_country)
togo_regions_sf     <- sf::st_as_sf(togo_regions)
togo_prefectures_sf <- sf::st_as_sf(togo_prefectures)

# Données raster
togo_elevation  <- terra::rast("data/raw/elevation/TGO_elv_msk.tif")
togo_precip     <- terra::rast("data/raw/climate/wc2.1_country/TGO_wc2.1_30s_prec.tif")
togo_temp       <- terra::rast("data/raw/climate/wc2.1_country/TGO_wc2.1_30s_tavg.tif")
togo_population <- terra::rast("data/raw/togo_population_2020.tif")
# Vérification
cat("✓ Toutes les données chargées avec succès\n")




#================================================
# 2. Calcul des variables dérivées
#================================================

# Précipitations moyennes annuelles

togo_precip_annual <- mean(togo_precip)
names(togo_precip_annual) <- "precip_annual"

# Température moyenne annuelle

togo_temp_annual <- mean(togo_temp)
names(togo_temp_annual) <- "temp_annual"

# Renommer elevation et population
names(togo_elevation)  <- "elevation"
names(togo_population) <- "population"

# Vérification
cat("Variables dérivées calculées :\n")
cat("- Précipitations annuelles ✓\n")
cat("- Température annuelle ✓\n")
cat("- Elevation ✓\n")
cat("- Population ✓\n")


#================================================
# 3. Extraction des variables par préfecture
#================================================

# Reprojeter les préfectures dans le même CRS 
# que les rasters

togo_prefectures_sv <- terra::vect(togo_prefectures_sf)

# extraire les valeurs moyennes par prefecture

precip_by_pref <- terra::extract(
  togo_precip_annual,
  togo_prefectures_sv,
  fun = mean,
  na.rm = TRUE
)


temp_by_pref <- terra::extract(
  togo_temp_annual,
  togo_prefectures_sv,
  fun = mean,
  na.rm = TRUE
)

elev_by_pref <- terra::extract(
  togo_elevation,
  togo_prefectures_sv,
  fun = mean,
  na.rm = TRUE
)


pop_by_pref <- terra::extract(
  togo_population,
  togo_prefectures_sv,
  fun = mean,
  na.rm = TRUE
)


# verification

cat("Extraction terminée :\n")
cat("- Précipitations par préfecture ✓\n")
cat("- Température par préfecture ✓\n")
cat("- Elevation par préfecture ✓\n")
cat("- Population par préfecture ✓\n")




#================================================
# 4. Assemblage du dataset final
#================================================

# Créer le dataset en combinant toutes les variables

togo_data <- togo_prefectures_sf |>
  mutate(
    precip_annual = precip_by_pref$precip_annual,
    temp_annual   = temp_by_pref$temp_annual,
    elevation     = elev_by_pref$elevation,
    population    = pop_by_pref$population
  ) |>
  select(
    NAME_1,
    NAME_2,
    precip_annual,
    temp_annual,
    elevation,
    population,
    geometry
  )

# vérification

cat("Dataset final : \n")
print(head(togo_data))
cat("\nDimensions :", nrow(togo_data), "préfectures x", 
    ncol(togo_data), "variables\n")


#================================================
# 5. Sauvegarde du dataset
#================================================

# Sauvegarder en format shapefile

sf::st_write(
  togo_data,
  "data/processed/togo_prefectures_data.gpkg",
  delete_if_existS = TRUE
)



# Sauvegarder aussi en CSV sans géométrie

togo_data |>
  sf::st_drop_geometry() |>
  write.csv("data/processed/togo_prefectures_data.csv")


cat("✓ Dataset sauvegardé en GeoPackage et CSV\n")












