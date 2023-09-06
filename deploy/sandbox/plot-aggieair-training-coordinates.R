### Script for plotting AggieAir training dataset 
### (spatial coordinates of surveyed trees)
### SN - 5 Sept 2023

### Requires: terra, dplyr, here, ggplot

# Clear namespace
rm(list = ls())

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

# Downlaod all aggie air data files
# (default args means download everything - all species, all files)
all_survey_paths <- download_field_survey_data()

# Load in all shape (.shp) files
shp_paths <- all_survey_paths |> dplyr::filter(grepl('shp$', object))

# Read in each of the data points
for (i in 1:nrow(shp_paths)) assign(shp_paths$object[i], terra::vect(shp_paths$file_path[i]))

# Bind together data frames with the following attributes:
# - species, tree index, height, diameter
# - x, y coordinates (UTM)

all_surv <- lapply(
  mget(shp_paths$object),
  function(shape_obj) {
    cbind(
      shape_obj |>
        terra::geom() |>
        as.data.frame() |>
        dplyr::select(x, y),
      shape_obj |>
        as.data.frame() |>
        dplyr::select(ClassUpdat, Ht, Diam, Notes, Descriptio)
    )
  }
) |>
  do.call(what = rbind)

# Add column for test/train dataset and make new columns for species only and dead/alive only
all_surv = all_surv |>
  dplyr::mutate(set = ifelse(grepl('train', row.names(all_surv)), 'train', 'test')) |>
  tidyr::separate(col = ClassUpdat, into = c('Species', 'Live'), sep = '_', remove = FALSE)
                
# Remove rownames
row.names(all_surv) <- NULL

# Look at data frame
head(all_surv)

# Look at distributions of class over the two datasets
with(all_surv, table(ClassUpdat, set))

# Plot points

library(ggplot2)

ggplot(all_surv, aes(x = x, y = y)) +
  geom_point(aes(colour = Species, shape = paste(Live, set)), size = 3) +
  scale_shape_manual(values = c(1, 19, 2, 17), '')

ggplot(all_surv, aes(x = x, y = y)) +
  geom_point(aes(shape = set, alpha = Live), size = 3) +
  scale_shape_manual(values = c(1, 19), '') +
  scale_alpha_manual(values = c(.5, 1)) +
  facet_wrap(~ Species)

# Sampling... not super complete in a way I would like.
# - Very few firs, and seemingly clustered
# - Spruce are all in same area as well

detach('packages::ggplot2')
