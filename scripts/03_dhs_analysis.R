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



#================================================
# 2. Extraction des variables pour chaque cluster
#================================================


# Convertir les clusters GPS en SpatVector

dhs_gps_sv <- terra::vect(dhs_gps)

# Extraire les variables pour chaque cluster

precip_clusters <- terra:: extract(
  togo_precip_annual,
  dhs_gps_sv
)

temp_clusters <- terra:: extract(
  togo_temp_annual,
  dhs_gps_sv
)

elev_clusters <- terra:: extract(
  togo_elevation,
  dhs_gps_sv
)

pop_clusters <- terra:: extract(
  togo_population,
  dhs_gps_sv
)


# Vérification
cat("Extraction par cluster :\n")
cat("- Précipitations :", nrow(precip_clusters), "clusters ✓\n")
cat("- Température :", nrow(temp_clusters), "clusters ✓\n")
cat("- Altitude :", nrow(elev_clusters), "clusters ✓\n")
cat("- Population :", nrow(pop_clusters), "clusters ✓\n")


#================================================
# 3. Extraction indice de richesse DHS
#================================================

# Explorer les variables disponibles

cat("Nombres de variables DHS:", ncol(dhs_household),"\n")

# La variable clé → indice de richesse
# hv271 = wealth index factor score
# hv270 = wealth index (catégoriel 1-5)

wealth_cluster <- dhs_household |>
  group_by(hv001) |>        # hv001 = numéro du cluster
  summarise(
    wealth_score = mean(hv271, na.rm = TRUE),  # score continu
    wealth_index = median(hv270, na.rm = TRUE), # catégorie 1-5
    n_menages    = n()                           # nb ménages
  )

cat("Résumé indice de richesse :\n")
print(summary(wealth_cluster))


#================================================
# 4. Assemblage du dataset ML final
#================================================

# Assembler toutes les variables extraites
dataset_ml <- dhs_gps |>
  mutate(
    # Numéro du cluster
    hv001         = DHSCLUST,
    # Variables satellitaires extraites
    precip_annual = precip_clusters$precip_annual,
    temp_annual   = temp_clusters$temp_annual,
    elevation     = elev_clusters$elevation,
    population    = pop_clusters$population
  ) |>
  # Joindre l'indice de richesse
  left_join(wealth_cluster, by = "hv001") |>
  # Garder seulement les variables utiles
  select(
    hv001,
    precip_annual,
    temp_annual,
    elevation,
    population,
    wealth_score,
    wealth_index,
    n_menages,
    geometry
  )

# Vérification
cat("Dataset ML final :\n")
print(head(dataset_ml))
cat("\nDimensions :", nrow(dataset_ml), 
    "clusters x", ncol(dataset_ml), "variables\n")
cat("\nValeurs manquantes :\n")
print(colSums(is.na(sf::st_drop_geometry(dataset_ml))))


#================================================
# 5. Nettoyage des valeurs manquantes
#================================================

# Identifier le cluster problématique
cat("Cluster avec valeurs manquantes :\n")
print(dataset_ml |> 
        sf::st_drop_geometry() |>
        filter(is.na(precip_annual)))

# Supprimer les lignes avec NA
dataset_ml_clean <- dataset_ml |>
  filter(!is.na(precip_annual),
         !is.na(temp_annual),
         !is.na(elevation),
         !is.na(population))

# Vérification
cat("\nDataset nettoyé :\n")
cat("- Clusters avant :", nrow(dataset_ml), "\n")
cat("- Clusters après :", nrow(dataset_ml_clean), "\n")
cat("- Valeurs manquantes :\n")
print(colSums(is.na(sf::st_drop_geometry(dataset_ml_clean))))






