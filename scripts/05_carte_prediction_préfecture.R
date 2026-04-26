#================================================
# Projet : Poverty Mapping Togo
# Script : 04 - Modèle deGradian boost (XGBoost)
# Auteur : Womenyao Komla Sedzro EKLOU
# Date   : Avril 2026
# Objectif : carte de prédiction par
# préfecture
#================================================

## Packages

library(sf)
library(terra)
library(tidyverse)
library(tidymodels)


#=============================================================
# 1 importer la base data_ml_clusters format geopackage
#===========================================================

clustergpk_data  <- sf::st_read("data/processed/dataset_ml_clusters.gpkg")


#=============================================================
# 2 Prédiction sur l'ensemble du dataset
#===========================================================

pred_by_prefecture <- augment(BestwkflowTgluster,clustergpk_data) |>
                            sf:: st_as_sf() ## Transformer en objet sf

#=============================================================
# 3 jointure spatiale avec la base préfecture
#===========================================================

## le but de cette jointure est de regrouper les cluster selon
## leur préfecture d'appartenance

pred_by_prefecture  <- pred_by_prefecture  |>
                   st_join(togo_prefectures_sf  |> select(NAME_2)) |>
                   sf::st_drop_geometry()


cat("Jointure éffectuée sur pred_by_prefecture \n")


#=============================================================
# 4 Calculer la moyenne des prédictions par cluster
#===========================================================

pred_by_prefecture <- pred_by_prefecture |>
                           group_by(NAME_2) |>
                           summarise(
                           wealth_pred_mean = mean(.pred, na.rm = TRUE),
                           n_clusters = n()
                           ) 

cat("Prédictions par préfecture :\n")
print(head(pred_by_prefecture))



#=============================================================
# 5 Joindre au shapefile des préfectures
#===========================================================

togo_pref_map <- togo_prefectures_sf |>
  left_join(pred_by_prefecture, by = "NAME_2")

cat("✓ Dataset carte prêt :\n")
cat("- Préfectures :", nrow(togo_pref_map), "\n")
cat("- NA :", sum(is.na(togo_pref_map$wealth_pred_mean)), "\n")


#=============================================================
# 6 Carte finale de pauvreté
#===========================================================

tmap_mode("plot")

tm_shape(togo_pref_map) +
  tm_fill(
    fill        = "wealth_pred_mean",
    fill.scale  = tm_scale_continuous(
      values    = "RdYlGn"
    ),
    fill.legend = tm_legend(
      title     = "Wealth Score prédit"
    )
  ) +
  tm_borders(col = "white", lwd = 0.5) +
  tm_shape(togo_regions_sf) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_title("Carte de pauvreté prédite — Togo 2017") +
  tm_compass(position = c("right", "top")) +
  tm_scalebar(position = c("left", "bottom"))



## Sauvegarde de la carte 

map_poverty <- tm_shape(togo_pref_map) +
  tm_fill(
    fill        = "wealth_pred_mean",
    fill.scale  = tm_scale_continuous(values = "RdYlGn"),
    fill.legend = tm_legend(title = "Wealth Score prédit")
  ) +
  tm_borders(col = "white", lwd = 0.5) +
  tm_shape(togo_regions_sf) +
  tm_borders(col = "black", lwd = 1.5) +
  tm_title("Carte de pauvreté prédite — Togo 2017") +
  tm_compass(position = c("right", "top")) +
  tm_scalebar(position = c("left", "bottom"))

tmap_save(
  map_poverty,
  filename = "outputs/maps/05_poverty_map_togo.png",
  dpi      = 300,
  width    = 8,
  height   = 10
)

cat("✓ Carte de pauvreté sauvegardée !\n")



