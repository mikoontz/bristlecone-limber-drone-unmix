##### Script for doing hyperparameter tuning on the survey point dataset
#####
##### init SN - 21 Sept 2023
##### 
##### Requires:
##### here (loading in data), assertthat (data checks)
##### googledrive (loading google drive data)
##### terra (for handling ortho and survey points)
##### dplyr and tidyr (data manipulation)
##### spatialsample, rsample, sf (for splitting data into test/train
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

# Prepare survey points for analysis
# (downloads ortho and whole point survey dataset, and concatenates point survey
# dataset into a single vector object)
source('deploy/sandbox/prepare-survey-points.R')

# Namespace should now include `all_shape`

### Declare other global variables

# Run models in parallel?
parallel.flag <- TRUE
# number of cores to run with
parallel.cores <- 4

# Run models with a defined seed?
seed <- NULL

# Trials per hyperparameter combination
trials.per <- 3
# v (folds in spatial sampler) to try
v <- (2:5) * 2
# mtry (variables to try per node split)
mtry <- 1 + (1:5)*2
# num.trees (number of trees to test)
num.trees <- c(100, 500, 1000)
# replace (sample with or without replacement)
replace <- c(TRUE, FALSE)

##### Prepare survey for test/train splitting and model fitting

# Add spatial geometry data needed for spatial sampling
points_sf <- point_bands |> 
  # Remove unnecessary columns
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
  
##### Initiate an object defining hyperparameters to tune

fit.iterator <- expand.grid(
  i = 1:trials.per,
  v = v,
  mtry = mtry,
  num.trees = num.trees,
  replace = replace
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
      data_split <- spatialsample::spatial_clustering_cv(points_sf, v = specs$v)
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
        data_split <- spatialsample::spatial_clustering_cv(points_sf, v = specs$v)
        scores     <- sapply(data_split$splits, fit_ranger_on_split, ranger_args = specs)
        # Return a data frame for easy merging
        return(data.frame(scores = mean(scores), iter = specs$iter))
      }
    ) |>
    # Bind together into dataframe
    do.call(what = rbind)
}

# Merge together and get a summary of mean score and variance around mean for
# parameter combinations
model_scores <- merge(fit.iterator, pred.score, by = 'iter') |>
  dplyr::group_by(v, mtry, num.trees, replace) |>
  dplyr::summarise(
    score.bar = mean(scores),
    score.var = var(scores),
    n = dplyr::n()
  )
