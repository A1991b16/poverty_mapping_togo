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





