#================================================
# Projet : Poverty Mapping Togo
# Script : 04 - Modèle deGradian boost (XGBoost)
# Auteur : Womenyao Komla Sedzro EKLOU
# Date   : Avril 2026
# Objectif : Prédire le taux de pauvreté avec 
#un modèle de gradian boost
#================================================

## Packages

library(tidyverse)
library(tidymodels)
library(rpart)
library(rpart.plot)

## Charger la base de données 

Tgcluster <- read.csv("data/processed/dataset_ml_clusters.csv")

#================================================================
#  1. Data Split 
#================================================================


set.seed(777)
SplitTgcluster <- Tgcluster |>
                  initial_split(prop = 0.7, strata = wealth_score, breaks = 3)

TgclusterTrain <- training(SplitTgcluster)
TgclusterTest  <- testing(SplitTgcluster)

head(TgclusterTrain)


#================================================================
#  2. The Recipe 
#================================================================

RecipeTgcluster <- recipe(wealth_score ~precip_annual + temp_annual + 
                            elevation + population, data = TgclusterTrain) |>
                   step_normalize(all_predictors())


#================================================================
#  3. The Model
#================================================================

ModelTgcluster <- boost_tree(trees = tune(), tree_depth = tune()) |>
                  set_engine("xgboost") |>
                  set_mode("regression")


#================================================================
#  4. The Workflow
#================================================================


WorkflowTgcluster <- workflow() |>
                     add_recipe(RecipeTgcluster) |>
                     add_model(ModelTgcluster)


#=========================================================================
#  5. Setting the tuning parameters and build the cross validation fold
#=========================================================================

set.seed(777)
TuneparTgcluster <- expand.grid(
                    trees      = c(5, 10 , 15, 30),
                    tree_depth = c(1, 2, 5, 10)
                    )


FoldTgcluster  <-  TgclusterTrain |>
                   vfold_cv(v = 5, strata = wealth_score, breaks = 3)


## Tune the workflow and train all the model

doParallel::registerDoParallel()

set.seed(777)
TuneResultTgcluster <-  tune_grid(
                        WorkflowTgcluster,
                        resamples = FoldTgcluster,
                        grid = TuneparTgcluster,
                        metrics = metric_set(mae)
                    )

autoplot(TuneResultTgcluster)

BestTgcluster <- select_best(TuneResultTgcluster, metric = "mae")
print(BestTgcluster)

cat("le modèle performe correctement avec ", BestTgcluster$trees, "trees",
    "et un tree_depth de", BestTgcluster$tree_depth  )


## Appliquons le workflow sur le meilleur modèle

BestwkflowTgluster <- WorkflowTgcluster |>
                     finalize_workflow(BestTgcluster) |>
                     fit(TgclusterTrain)

print(BestwkflowTgluster)

## Perfomance du modèle sur le Testing Data


TgclusterPrediction <- augment(BestwkflowTgluster,TgclusterTest)

MetricBestmodelTgcluster <- metrics(TgclusterPrediction, truth = wealth_score,
                                    estimate = .pred)

print(MetricBestmodelTgcluster)


#================================================
# 6. Visualisation des résultats
#================================================

# Graphique prédictions vs valeurs réelles
ggplot(TgclusterPrediction,
       aes(x = wealth_score, y = .pred)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 3) +
  geom_abline(slope = 1, intercept = 0,
              color = "red", linetype = "dashed", lwd = 1) +
  labs(
    title    = "Prédictions vs Valeurs réelles — XGBoost",
    subtitle = paste("R² =", round(0.856, 3),
                     "| RMSE =", round(29712, 0)),
    x        = "Wealth Score réel (DHS)",
    y        = "Wealth Score prédit (XGBoost)"
  ) +
  theme_minimal() +
  theme(
    plot.title    = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 11, color = "gray50")
  )

# Sauvegarder
ggsave(
  "outputs/figures/01_predictions_vs_real.png",
  dpi    = 300,
  width  = 8,
  height = 6
)

cat("✓ Graphique sauvegardé\n")

#================================================
# 7. Importance des variables
#================================================

# Extraire l'importance des variables
importance_vars <- BestwkflowTgluster |>
  extract_fit_parsnip() |>
  vip::vi()

# Graphique
ggplot(importance_vars,
       aes(x = reorder(Variable, Importance),
           y = Importance)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Importance des variables — XGBoost",
    subtitle = "Contribution de chaque variable satellite à la prédiction",
    x = "Variable",
    y = "Importance"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold")
  )

# Sauvegarder
ggsave(
  "outputs/figures/02_variable_importance.png",
  dpi   = 300,
  width = 8,
  height = 6
)

cat("✓ Graphique importance variables sauvegardé\n")
