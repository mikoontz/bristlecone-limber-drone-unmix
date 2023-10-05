##### Script for reading in all aggie-air survey points, extracting band info
##### from orthomosaic, and doing other processing
##### SN - 13 Sept 2023, copied to deploy 3 Oct 2023

##### Requires: terra, dplyr, tidyr, here, assertthat
#####
##### Output: `point_bands` is a data frame with features (bands, indices, and
##### coordinates), with one row for each survey point.
##### (The namespace will also have individual shape objects as well as an
##### object, `all_shape`, with all points)

### Setup

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

### Load in orthomosaic

# call our get_gdrive_data() function to get the multispec_ortho data object
# from the Google Drive
if (!exists('ortho')) {
  ortho_path <- get_gdrive_data(object = "multispec_ortho")
  ortho <- terra::rast(ortho_path)
}

# names of the bands
names(ortho) <- c(
  "blue_475", "green_560", "red_668", "rededge_717", "nir_842",
  "coastalblue_444", "green_531", "red_650", "rededge_705", "rededge_740"
  )

### Load in survey data (field points)

# Download field survey data (points)
all_survey_paths <- download_field_survey_data()

# Load in all shape (.shp) files
shp_paths <- all_survey_paths |> dplyr::filter(grepl('shp$', object))

# Read in each of the data points
for (i in 1:nrow(shp_paths)) assign(shp_paths$object[i], terra::vect(shp_paths$file_path[i]))

# Concatenate all into a single shape object

# (I'm having trouble doing this sleekly with do.call, not sure why)
# Doing it with a for loop instead as unsatisfying as that is
all_shape <- get(shp_paths$object[1])
for (i in 2:nrow(shp_paths)) all_shape <- rbind(all_shape, get(shp_paths$object[i]))

# Check number of points
dim(all_shape)

# Add an ID column (will be useful for merging later)
all_shape$ID <- 1:nrow(all_shape)

### Extract band info from points

point_bands <- terra::extract(ortho, all_shape, ID = TRUE)

# Test to see if any polygons are missing (if so, remove them)
# (will happen with cropped orthomosaic)
message(sum(!complete.cases(point_bands)), " points with missing values")

point_bands <- point_bands[complete.cases(point_bands),]

head(point_bands)

# Merge in classification info
# `ClassUpdat` is the column that has the classifications in the point dataset
#   this might need to be changed for reproducibility
point_bands <- terra::merge(point_bands, all_shape[,c("ID", "ClassUpdat")])

head(point_bands)

# Add new class labels; add these to column `class
#   Assume for our models that we want classes for
#   (1) living bristlecones (pilo)
#   (2) living limber pines (pifl)
#   (3) living other trees (firs, etc.)
#   (4) dead trees

point_bands$class <- dplyr::case_when(
  # All dead vegetation is dead
  grepl('dead$', point_bands$ClassUpdat) ~ 'dead',
  # Pinus longaeva gets PILO - dead trees should have been taken out in previous step
  grepl('^PILO', point_bands$ClassUpdat) ~ 'pilo',
  # Pinus flexilis gets PIFL - dead trees should have been taken out in first step
  grepl('^PIFL', point_bands$ClassUpdat) ~ 'pifl',
  # Anything else is "other tree"
  .default = 'tree_other'
)

# Check to make sure this output looks correct
with(point_bands, table(ClassUpdat, class))
table(point_bands$class)

# Add composite measures (same ones used in aggieair report)

point_bands <- point_bands |>
  dplyr::mutate(
    ndvi_650 = (nir_842 - red_650) / (nir_842 + red_650),
    ndvi_688 = (nir_842 - red_668) / (nir_842 + red_668),
    ndre_705 = (nir_842 - rededge_705) / (nir_842 + rededge_705),
    ndre_717 = (nir_842 - rededge_717) / (nir_842 + rededge_717),
    ndre_740 = (nir_842 - rededge_740) / (nir_842 + rededge_740),
    mndvi    = (rededge_740 - rededge_705) / (rededge_740 + rededge_705 - 2*coastalblue_444),
    pri      = (green_531 - green_560) / (green_531 + green_560)
  )

# Add coordinates (will be used for spatial sampling)
point_bands <- merge(
  point_bands, as.data.frame(terra::geom(all_shape))[,c("geom", "x", "y")],
  by.x = 'ID', by.y = "geom"
)

