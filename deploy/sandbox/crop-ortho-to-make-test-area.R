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
ortho_path <- get_gdrive_data(object = "multispec_ortho")
ortho <- terra::rast(ortho_path)

# names of the bands
names(ortho) <- c("blue_475", "green_560", "red_668",
                  "rededge_717", "nir_842",
                  "coastalblue_444", "green_531",
                  "red_650", "rededge_705", "rededge_740")

dev.new(noRStudioGD = TRUE)
terra::plot(ortho[[1]])
terra::draw()

crop_ext <- terra::ext(
  c(739833.14590542, 
    739976.751214358, 
    4352779.79298319, 
    4352910.57638955)
  )

ortho_crop <- terra::crop(x = ortho, y = crop_ext)

terra::plot(ortho_crop)

terra::writeRaster(x = ortho_crop, 
                   filename = "20210905_MtMoriah_10band_reflectance_scaled1000_crop.tif")
