##### Script for reading in all aggie-air survey points, extracting band info
##### from orthomosaic, and doing other processing
##### SN - 13 Sept 2023

##### Requires: terra, dplyr, tidyr, here

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

### Load in field survey polygons

survey_path <- get_gdrive_data("fieldmap_gbbp_survey_gpkg")
survey_poly <- terra::vect(survey_path)

# Take out early polygons (testing points)
survey_poly <- survey_poly[as.Date(survey_poly$CreationDate) > as.Date('2023-08-27'),]

# Designate CRS for polygons
survey_poly <- terra::project(survey_poly, terra::crs(ortho))
# Add ID column to polygons
survey_poly$ID <- 1:dim(survey_poly)[1]

### Extract bands from ortho at polygon locations

poly_bands <- terra::extract(ortho, survey_poly, ID = TRUE)

nrow(poly_bands)

# Test to see if any polygons are missing (if so, remove them)
# (will happen with cropped orthomosaics...)
message(sum(!complete.cases(poly_bands)), " points with missing values")
# Remove these points
poly_bands <- poly_bands[complete.cases(poly_bands),]

# Number of remaining points
length(unique(poly_bands$ID))

# Merge in class ids
poly_bands <- terra::merge(poly_bands, survey_poly[,c("ID", "cover_type")])

# Add other band info

poly_bands <- dplyr::mutate(
  poly_bands,
  ndvi_650 = (nir_842 - red_650) / (nir_842 + red_650),
  ndvi_688 = (nir_842 - red_668) / (nir_842 + red_668),
  ndre_705 = (nir_842 - rededge_705) / (nir_842 + rededge_705),
  ndre_717 = (nir_842 - rededge_717) / (nir_842 + rededge_717),
  ndre_740 = (nir_842 - rededge_740) / (nir_842 + rededge_740),
  mndvi    = (rededge_740 - rededge_705) / (rededge_740 + rededge_705 - 2*coastalblue_444),
  pri      = (green_531 - green_560) / (green_531 + green_560)
)

### Aggregate polygons into a mean value for each polygon

poly_means <- poly_bands |>
  dplyr::group_by(ID, cover_type) |>
  dplyr::summarise(dplyr::across(everything(), mean))
  

# in future, would be good to add other summary statistics

# tidyr::pivot_longer(
#   poly_bands, cols = -c(ID, cover_type),
#   names_to = 'band', values_to = 'refl'
# ) |>
#   dplyr::group_by(ID, band, cover_type) |>
#   dplyr::summarise(mean_refl = mean(refl))

# head(poly_means)

