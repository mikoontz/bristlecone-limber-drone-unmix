# Extract Metadata
library(exiftoolr)
library(tidyverse)
library(sf)
library(tidyterra)
library(glue)

metadata_fn <- "data/metadata.csv"

if(!file.exists(metadata_fn)){
img_files <-
  list.files("data/drone/L0/2023-08-19", pattern = "JPG", recursive = T, full.names = T)
img_files

out <- exif_read(img_files[1:10])
paste(colnames(out)[101:200], collapse = "', '")

tags <- c('SourceFile','DateTimeOriginal', 'Make', 'Model', 'Orientation', 
          'ExposureTime', 'FNumber', 'ExposureProgram', 'ISO', 'SensitivityType',
          'ShutterSpeedValue', 'ApertureValue', 'ExposureCompensation',
          'FocalLength', 'ExifImageWidth', 'ExifImageHeight', 
          'GPSLatitudeRef', 'GPSLongitudeRef', 'GPSAltitudeRef', 'GPSStatus', 
          'GPSMapDatum', 'GpsStatus', 'AltitudeType', 'AbsoluteAltitude', 'RelativeAltitude', 
          'GimbalRollDegree', 'GimbalYawDegree', 'GimbalPitchDegree', 
          'FlightRollDegree', 'FlightYawDegree', 'FlightPitchDegree', 
          'FlightXSpeed', 'FlightYSpeed', 'FlightZSpeed', 
          'CamReverse', 'GimbalReverse', 
          'RtkFlag', 'RtkStdLon', 'RtkStdLat', 'RtkStdHgt', 'RtkDiffAge', 'SurveyingMode',  
          'ShutterCount', 'Aperture', 
          'GPSAltitude', 'GPSLatitude', 'GPSLongitude',  
          'CircleOfConfusion', 'FOV')

metadata <- exif_read(img_files, tags = tags)
write_csv(metadata, metadata_fn)
}

metadata <- read_csv(metadata_fn)
metadata$subdir_name <- basename(dirname(metadata$SourceFile))

# metadata - spatial
metadata_sf <- st_as_sf(metadata, coords = c("GPSLongitude","GPSLatitude")) |>
  st_set_crs(4326)

plot(metadata_sf[,"subdir_name"])
table(metadata_sf$GimbalPitchDegree) 

# remove all with Gimball pitch not near -90 and not labeled oblique
metadata_sf |> 
  filter(GimbalPitchDegree < -89.8) |>
  filter(!grepl(x = subdir_name, pattern = "oblique", fixed = T)) |> 
  ggplot(aes(colour = subdir_name)) +
  geom_spatvector()

# make a separate folder that contains only the photos that are in this minimum dataset
ard_pics <- 
  metadata_sf |> 
  filter(GimbalPitchDegree < -89.8) |>
  filter(!grepl(x = subdir_name, pattern = "oblique", fixed = T)) |> 
  mutate(dest_filename = glue("data/drone/L0/ard/{subdir_name}_{basename(SourceFile)}"))

ard_pics |> 
  select(SourceFile, dest_filename) |> 
  st_drop_geometry() |> 
  unique()

length(unique(ard_pics$dest_filename))
dir.create("data/drone/L0/ard")
file.copy(from = ard_pics$SourceFile, to = ard_pics$dest_filename)




