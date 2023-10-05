##### Sandbox script for fitting and initial assessment of model on point
##### dataset (Aggieair dataset)
##### 
##### SN - 5 Oct 2023
#####
##### This script fits one set of random forest models on a train/test split
##### Model specs are from one of the best performing combinations as found in
##### `deploy/tune-ranger-hyperparameters_point-data.R`
#####
##### For each model, crudely visualize feature importance and confusion matrix
##### 
##### Requires:
#####
##### - here, assertthat (for aux functions)
##### - terra (for handling spatial data)
##### - rsample, spatialsample, sf (for spatial sampling)
##### - dplyr, ggplot, tidyr for manipulating data and visualizations

##### Setup

# Clear namespace
rm(list = ls())

# Load in ggplot library
library(ggplot2)

# Load in necessary functions
# List of all files in the src/ folder-- make sure they are just .R files and
# get their full names!
src <- list.files(path = here::here("src"), full.names = TRUE)

# Loop through each file, provide an informative message, and call the source()
# function in order to read the function into the Global environment
for(i in seq_along(src)) {
  message("Sourcing ", 
          gsub(x = basename(src[i]), pattern = ".R", replacement = "()"), 
          " function...")
  source(src[i])
}

##### 

# Prepare survey polygons for analysis (downloads ortho and whole point survey
# dataset, and concatenates point survey dataset into a single data frame with
# mean and standard deviation of each band or index for each polygon in the
# dataset
source('deploy/sandbox/prepare-survey-points.R')

##### Get geometry for spatial sampling in split

# Add spatial geometry data needed for spatial sampling
points_sf <- point_bands |> 
  # Remove unnecessary columns
  # (ID was used in the extraction step, ClassUpdat was the classification in
  # the raw data)
  dplyr::select(-c(ID, ClassUpdat)) |> 
  # Convert class into a factor (for classification)
  dplyr::mutate(class = factor(class)) |>
  # Convert to sf for spatial split
  sf::st_as_sf(
    # Specify coordinates
    coords = c("x", "y"),
    # Get coordinate reference system - matches orthomosaic
    crs = terra::crs(ortho) 
  )

##### Run model

### Declare parameters

# (At some point, might want to do a slick way of extracting this from a tuning
# script)

# Number of trees to use
num.trees <- 100
# Number of variables to subsample per node
mtry <- 15
# minimum number of samples per node
min.node.size <- 1
# number of folds
num.folds <- 4

##### Split dataset into testing and training sets and fit models

# Set seed (for reproducibility)
set.seed(871)

# Split the data
data.split <- spatialsample::spatial_block_cv(
  data = points_sf,
  v = num.folds
)

# Fit models on each fold
split.mods <- lapply(
  X = data.split$splits,
  FUN = function(split.data) {
    mod <- ranger::ranger(
      formula = class ~ .,
      data = rsample::analysis(split.data) |> sf::st_drop_geometry(),
      num.trees = num.trees,
      mtry = mtry,
      min.node.size = min.node.size,
      importance = 'permutation'
    )
    list(
      mod = mod,
      pred = predict(
        object = mod,
        data = rsample::assessment(split.data) |> sf::st_drop_geometry()
      )
    )
  }
)

# Output is a list with one entry per fold:
# - $mod has the ranger model output
# - $pred has the prediction for that fold's holdout data

# E.g., 
split.mods

##### Visualize feature importances

# Get feature importances out of the model, put them into a data frame
feature.importances <- split.mods |> 
  lapply(function(x) x$mod) |>
  lapply(ranger::importance) |>
  do.call(what = cbind) |>
  as.data.frame()

# Add features column (originally they are row names)
# (wish there was a neater way to do this in a single pipe chain...)
feature.importances$feat <- row.names(feature.importances)

# Convert to long-form for plotting
feature.importances <- feature.importances |>
  tidyr::pivot_longer(
    cols = -feat,
    names_to = 'fold',
    values_to = 'importance'
  )

# Plot: features on x-axis, importance on y,
# each line corresponds to one fold
# (more-parallel lines mean more consistency among folds)
ggplot(feature.importances, aes(x = feat, y = importance)) +
  geom_line(aes(group = fold)) +
  theme(axis.text.x = element_text(angle = 90))

# Good - looks consistent across folds

##### Visualize confusion matrices

# Make a three-columned data frame

conf.tables <- cbind(
  # Column 1: model predictions
  pred = lapply(
    X = split.mods, 
    FUN = function(mod.output) ranger::predictions(mod.output$pred)
  ) |>
    do.call(what = c),
  # Column 2: observed classes from data
  obsv = lapply(
    X = data.split$splits, 
    FUN = function(split.data) rsample::assessment(split.data)$class
  ) |>
    do.call(what = c),
  # Column 3: which fold this comes from (might be useful)
  fold = tidyr::uncount(
    data = data.frame(
      fold = data.split$id,
      # n.sample is number of samples in each fold - used in uncount() to make
      # one row per sample
      n.sample = sapply(
        X = data.split$splits, 
        FUN = function(split.data) nrow(rsample::assessment(split.data)))
    ),
    weights = n.sample
  )
)

# Sneak peak at data frame
head(conf.tables)

# Confusion matrix in text form
with(conf.tables, table(obsv, pred))

# Make a plot using points
# (there are surely better ways to do this...
#   here, I'm doing points so I can visualize the folds)
ggplot(conf.tables, aes(x = obsv, y = pred)) +
  geom_point(
    aes(colour = fold, shape = factor(obsv == pred)),
    size = 3,
    position = position_jitter(height = 0.25, width = 0.25)
  ) +
  scale_shape_manual(values = c(1, 19), '')
