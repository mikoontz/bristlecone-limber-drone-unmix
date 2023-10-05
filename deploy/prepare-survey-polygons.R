##### Script for reading in all aggie-air survey points, extracting band info
##### from orthomosaic, and doing other processing
##### SN - 13 Sept 2023, finalized 3 Oct 2023

##### Requires: terra, dplyr, tidyr, here, assertthat
#####
##### Output: `poly_bands` is a data frame with features (bands, indices, and
##### coordinates), with one row for each survey polygon.
##### note that this gets summary statistics (mean, sd) as well as number of
##### cells (integer - not dealing with partial-overlaps differently) of each
##### band for each polygon
##### (The namespace will also have a processed version of the survey polygons -
##### `survey_poly`)

### Load in drone orthomosaic w/ band data

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

### Load in polygon field survey data and prepare for analysis

# Get file link for survey polygons and read in vector
survey_poly <- get_gdrive_data('fieldmap_gbbp_survey_gpkg') |>
  terra::vect()

# Convert date column to a date
survey_poly$collection_date <- as.Date(survey_poly$collection_date)

# Remove test observations (taken Aug 25)
survey_poly <- survey_poly[survey_poly$collection_date > as.Date('2023-08-28'),]

nrow(survey_poly)

# Add an ID column for indexing
survey_poly$ID <- 1:nrow(survey_poly)

# Re-set coordinate system if needed
if (terra::crs(survey_poly) != terra::crs(ortho)) {
  survey_poly <- terra::project(survey_poly, terra::crs(ortho))
}


### Extract spectral bands from orthomosaic and create object for splitting
### and model fitting

poly_bands <- terra::extract(ortho, survey_poly, ID = TRUE)

# Remove NAs
poly_bands <- poly_bands[complete.cases(poly_bands),]
# (NOTE: this step will preserve polygons if there is a fraction of them that
# have NAs - only the NA rows will be removed, but the rows that did have
# extracted data will be preserved - this may create a biased sample)

# Merge in classifications and prepare column for model
poly_bands <- merge(poly_bands, survey_poly[,c("ID", "cover_type")]) |>
  dplyr::rename(class = cover_type) |>
  dplyr::mutate(class = factor(class))

head(poly_bands)

# Add in additional band information and summarise
poly_bands <-  poly_bands |>
  dplyr::mutate(
    ndvi_650 = (nir_842 - red_650) / (nir_842 + red_650),
    ndvi_688 = (nir_842 - red_668) / (nir_842 + red_668),
    ndre_705 = (nir_842 - rededge_705) / (nir_842 + rededge_705),
    ndre_717 = (nir_842 - rededge_717) / (nir_842 + rededge_717),
    ndre_740 = (nir_842 - rededge_740) / (nir_842 + rededge_740),
    mndvi    = (rededge_740 - rededge_705) / (rededge_740 + rededge_705 - 2*coastalblue_444),
    pri      = (green_531 - green_560) / (green_531 + green_560)
  ) |>
  # Filter out cases that will cause NaNs in MNDVI
  dplyr::filter(rededge_740 + rededge_705 != 2*coastalblue_444) |>
  # Now, get summary statistics for each polygon-band combinatino
  dplyr::group_by(ID, class) |>
  dplyr::summarise(
    across(
      # For all columns (not specified above - should be solely bands)
      .cols = tidyr::everything(),
      # Get mean and standard deviation
      .fns = list(mean = mean, sd = sd),
      .names = "{.col}.{.fn}"
    ),
    # Also, for each *polygon* (not band) get number of observations
    n = dplyr::n()
  )

