#' @description
#' Function will download a file from the Google Drive to a known local location
#' 
#' @param object
#' 
#' @returns Local path to downloaded object

get_gdrive_data <- function(object) {
  
  googledrive::drive_auth(email = "*@colorado.edu")
  
  data_links <- read.csv(
    here::here("data", "data-links.csv")
  )
  
  assertthat::assert_that(
    object %in% data_links$object,
    msg = "Data are not available."
  )
  
  assertthat::assert_that(
    sum(data_links$object %in% object) == 1,
    msg = "Object name matches more than once. Something 
    awry with data-links.csv."
  )
  
  # filter to just the row where the input parameter `object` matches the value
  # in the `$object` column of the data_links.csv file. Use bang-bang to make
  # sure that the input parameter `object` is used on the right side of the
  # evaluation instead of the non-standard evaluation of the non-quoted column
  # name
  obj_link <- 
    data_links |>
    dplyr::filter(object == !!object) |>
    dplyr::pull(link)
  
  obj_dribble <- googledrive::as_dribble(obj_link)
  obj_basename <- obj_dribble$name
  
  local_path <- file.path(tempdir(), obj_basename)
  
  # We should check if the file has already been created
  # If it is a zip file, then we should see whether the unzipped version has
  # been created
  if(tools::file_ext(obj_basename) == "zip") {
    out_path <- file.path(
      tempdir(), 
      gsub(
        x = obj_basename, 
        pattern = ".zip", 
        replacement = ""
      )
    )
    file_exists <- file.exists(out_path)
  } else {
    file_exists <- file.exists(local_path)
  }
  
  # If file doesn't exist, then download it (potentially the zipped version)
  if(!file_exists) {
    googledrive::drive_download(
      file = googledrive::as_id(obj_link),
      path = local_path
    )
    
    # If it's a zip file, then we should unzip it. R has trouble with large zip
    # files, so we'll use the system's built-in unzipper
    # https://stackoverflow.com/questions/42740206/r-possible-truncation-of-4gb-file
    
    if(tools::file_ext(obj_basename) == "zip") {
      out_path <- gsub(
        x = local_path, pattern = ".zip", replacement = ""
      )
      
      # Switching the working directory temporarily simplifies the call to
      # system2()
      wd <- getwd()
      setwd(tempdir())
      
      decompression <-
        system2("unzip",
                args = c("-o", # include override flag
                         local_path),
                stdout = TRUE)
      
      # Return working director to the original
      setwd(wd)
      rm(wd)
      
      # delete the zip file
      unlink(local_path)
      
    } else {
      out_path <- local_path
    }
  } else {
    # Needed in case not declared above (if file exists and is non-zip)
    out_path <- local_path
  }
  
  return(out_path)
}
