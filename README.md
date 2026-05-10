
# Poverty Mapping Togo TG

## Description
Ce projet prédit la pauvreté au Togo par préfecture 
en combinant des données d'enquête ménage (DHS 2017) 
avec des variables satellitaires via un modèle 
de Machine Learning (XGBoost).

## Objectif
Produire une carte de vulnérabilité économique 
par préfecture au Togo en utilisant des données 
satellitaires librement accessibles comme proxy 
de la pauvreté.

## Données utilisées
- **DHS Togo 2017** — Enquête ménage + coordonnées GPS
- **WorldClim** — Précipitations et température moyennes
- **SRTM** — Altitude (modèle numérique de terrain)
- **WorldPop** — Densité de population 2020

## Méthodologie
1. Téléchargement des données satellitaires (geodata, terra)
2. Extraction des variables par cluster DHS (terra::extract)
3. Construction du dataset ML (170 clusters × 4 variables)
4. Modèle XGBoost avec tuning (tidymodels)
5. Prédiction par préfecture + cartographie (tmap)

## Résultats
- **R² = 0.856** sur les données de test
- La densité de population est le principal prédicteur
- Gradient nord/sud de pauvreté confirmé

## Structure du projet

poverty_mapping_togo/
├── data/
│   ├── raw/          → données brutes
│   └── processed/    → données traitées
├── scripts/
│   ├── 01_data_collection.R
│   ├── 02_data_preparation.R
│   ├── 03_dhs_analysis.R
│   ├── 04_model_xgboost.R
│   └── 05_poverty_map.R
└── outputs/
├── figures/      → graphiques
└── maps/         → cartes


## Technologies
- **R** — tidyverse, tidymodels, sf, terra, tmap
- **Git/GitHub** — contrôle de version

## Auteur
Womenyao Komla Sedzro EKLOU  
Master Économie du Développement — CERDI  
Université Clermont Auvergne, France

## Références
- Jean et al. (2016) — *Combining satellite imagery 
  and machine learning to predict poverty* — Science
- Henderson et al. (2012) — *Measuring Economic Growth 
  from Outer Space* — AER












