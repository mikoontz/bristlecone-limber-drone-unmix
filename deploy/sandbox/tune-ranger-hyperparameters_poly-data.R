##### Script for hyper-parameter tuning using grid-search on ranger random
##### forests using the polygon data (from Bentz crew surveys in 2023)
##### SN - finalized-ish 27 Sept 2023
#####
##### Requires:
##### here (loading in data), assertthat (data checks)
##### googledrive (loading google drive data)
##### terra (for handling ortho and survey points)
##### dplyr and tidyr (data manipulation)
##### spatialsample, rsample, sf (for splitting data into test/train)
##### ranger (for fitting models)
##### Optional:
##### parallel (run model fits in parallel)

##### Setup

# Clear namespace
rm(list = ls())

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
source('deploy/sandbox/prepare-survey-polygons.R')

##### Set up data for splitting into test/train

# Need to get spatial information in here
# Extract centroid for each polygon
centroid_poly <- terra::centroids(survey_poly) |>
  # Get geometric information (incl. coordinates) with geom()
  terra::geom() |>
  # Get geom (ID), x, and y columns out of this geom object
  (\(geom_obj) geom_obj[,c("geom", "x", "y")])() |>
  # Convert to data frame
  as.data.frame()
  
# Merge objects together and convert to sf for split  
poly_sf <- merge(
  x = poly_bands, y = centroid_poly,
  by.x = 'ID', by.y = 'geom'
) |>
  dplyr::select(-c(ID, n)) |>
  # Get sf info for spatial sample
  sf::st_as_sf(
    # Specify coordinates
    coords = c("x", "y"),
    # Get coordinate reference system - matches orthomosaic
    crs = terra::crs(survey_poly) 
  )

##### Split and fit models

### Declare variables for grid search
### (these are all model fitting parameters)

# Run models in parallel?
parallel.flag <- TRUE
# number of cores to run with
parallel.cores <- 4

# Run models with a defined seed?
seed <- NULL

# Trials per hyperparameter combination
trials.per <- 25
# v (folds in spatial sampler) to try
v <- (2:4)*3 # dont' go higher than 12
# mtry (variables to try per node split)
mtry <- (2:4)*6
# num.trees (number of trees to test)
num.trees <- c(100, 500)
# min.node.size (tree complexity)
min.node.size <- 1 + 2*(0:2)
# replace (sample with or without replacement)
replace <- c(TRUE, FALSE)
# sampler
sampler <- c('block', 'cluster')

# Create object with settings for each model fit

fit.iterator <- expand.grid(
  i = 1:trials.per,
  v = v,
  mtry = mtry,
  num.trees = num.trees,
  replace = replace,
  min.node.size = min.node.size,
  sampler = sampler
)

# Add an iter for easy parallelization and merging

fit.iterator$iter <- 1:nrow(fit.iterator)

##### Run models

# Set seed if specified
if (exists("seed")) set.seed(seed)

# Fit models and score each model
# (score is proportion of objects guessed correctly per model)
# pred.score is a data frame with column for iteration (for merging back with
# fit.iterator) and score

if (parallel.flag) {
  pred.score <- parallel::mclapply(
    # Split data frame into list for each row for mclapply
    X = base::split(fit.iterator, fit.iterator$iter),
    # Run a function that splits the data on the appropriate number of folds
    # and fits a model at the given hyperparameters
    FUN = function(specs) {
      if (specs$sampler %in% 'cluster') {
        data_split <- spatialsample::spatial_clustering_cv(poly_sf, v = specs$v)
      } else {
        data_split <- spatialsample::spatial_block_cv(poly_sf, v = specs$v)
      }
      scores     <- sapply(data_split$splits, fit_ranger_on_split, ranger_args = specs)
      # Return a data frame for easy merging
      return(data.frame(scores = mean(scores), iter = specs$iter))
    },
    # make sure parallel.cores is greater than 1 (and declared)
    mc.cores = parallel.cores
  ) |>
    # Bind together into dataframe
    do.call(what = rbind)
} else {
  # Split data frame into list for each row for lapply, then
  pred.score <- base::split(fit.iterator, fit.iterator$iter) |>
    # Run a function that splits the data on the appropriate number of folds
    # and fits a model at the given hyperparameters
    lapply(
      FUN = function(specs) {
        if (specs$sampler %in% 'cluster') {
          data_split <- spatialsample::spatial_clustering_cv(poly_sf, v = specs$v)
        } else {
          data_split <- spatialsample::spatial_block_cv(poly_sf, v = specs$v)
        }        
        scores     <- sapply(data_split$splits, fit_ranger_on_split, ranger_args = specs)
        # Return a data frame for easy merging
        return(data.frame(scores = mean(scores), iter = specs$iter))
      }
    ) |>
    # Bind together into dataframe
    do.call(what = rbind)
}

model_scores <- merge(fit.iterator, pred.score, by = 'iter') |>
  dplyr::group_by(v, mtry, num.trees, min.node.size, replace, sampler) |>
  dplyr::summarise(
    score.bar = mean(scores, na.rm = TRUE),
    score.var = var(scores, na.rm = TRUE),
    n = dplyr::n()
  )

##### Use ggplot to make a plot of results

library(ggplot2)

ggplot(model_scores |> dplyr::mutate(v = factor(v))) +
  geom_line(
    aes(
      x = mtry, y = score.bar,
      colour = v, linetype = replace
    ),
    linewidth = 1.2
  ) +
  geom_ribbon(
    aes(
      x = mtry,
      ymin = score.bar - 2*sqrt(score.var/n),
      ymax = score.bar + 2*sqrt(score.var/n),
      fill = v, group = interaction(v, replace)
    ),
    alpha = 0.1
  ) +
  facet_grid(min.node.size ~ num.trees + sampler)

detach(packages::ggplot2)
