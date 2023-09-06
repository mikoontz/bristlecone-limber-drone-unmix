#' @description 
#' Function will download field survey data. Currently set up for only aggie air products.
#' 
#' @param species
#' Species to read in (in our implementation, one of PILO, PIEN, PIFL, firs, dead, as string)
#' @param set
#' Test or train data set (in our implementation, test or train, as string)
#' 
#' @returns data frame with local filepaths to downloaded files

download_field_survey_data <- function(species = NULL, set = NULL) {
  
  # Check to make sure function for downloading google drive data is sourced
  # (if not, read it in)
  if (!exists('get_gdrive_data')) {
    source(here::here('src', 'get_gdrive_data.R'))
    message("Sourcing get_gdrive_data.R")
  }
  
  # Read in datalinks file
  data_links <- read.csv(here::here('data', 'data-links.csv'))
  
  # NOTE:
  # This function is written to handle the following species codes:
  #    - PILO, PIEN, PIFL
  #    - firs, dead
  # The following dataset types:
  #    - train, test
  # This function is also written to handle the following filetypes:
  #    - cpg, dbf, prj, sbn, sbx, shp, shpxml, shx
  # It also assumes a specific coding in the data-links.csv file
  # 
  
  # Get only relevant files (coded beginning with species names)
  data_links <- data_links |>
    dplyr::filter(grepl('aggie', object))
  
  # Go by cases
  # Default case is ALL
  
  if (!is.null(species) | !is.null(set)) {
    
    # If species or filetype is specified,  modify the dataframe for easy matching
    data_links <- data_links |>
      dplyr::mutate(split.temp = gsub('train', '_train_', object)) |>
      dplyr::mutate(split.temp = gsub('test', '_test_', split.temp)) |>
      dplyr::mutate(split.temp = gsub('aggieair_', '', split.temp)) |>
      tidyr::separate(col = split.temp, into = c("sp", "st", "ft"), sep = '__')
    
    if (!is.null(species) & is.null(set)) {
      
      # Get a regex string for subsetting
      regex_string <- paste(species, collapse = '|^')
      # Add caret to beginning
      regex_string <- paste0('^', regex_string)
      # Subset data_links data frame using above specified 
      data_links <- data_links |> dplyr::filter(grepl(regex_string, sp))
    
    } else if (!is.null(set) & is.null(species)) {
      
      regex_string <- paste(set, collapse = '$|^')
      # Add caret to beginning and dollar sign to end
      regex_string <- paste0('^', regex_string, '$')
      # Subset data_links using above specified
      data_links <- data_links |> dplyr::filter(grepl(regex_string, st))
    
    } else {
    
      sp_regex_string <- paste(species, collapse = '|^')
      # Add caret to beginning
      sp_regex_string <- paste0('^', sp_regex_string)
      
      set_regex_string <- paste(set, collapse = '$|^')
      # Add caret to beginning and dollar sign to end
      set_regex_string <- paste0('^', set_regex_string, '$')
      
      data_links <- data_links |>
        dplyr::filter(grepl(sp_regex_string, sp)) |>
        dplyr::filter(grepl(set_regex_string, st))
      
    }
      
  }
  
  message(
    nrow(data_links),
    " files available for reading in."
  )
  
  # Initialize data frame to be returned
  objs_loaded <- data_links
  objs_loaded$file_path <- NA
  objs_loaded$link <- NULL
  
  for (i in 1:nrow(data_links)) {
    # Loop through datalinks file and read in files
    objs_loaded$file_path[i] <- get_gdrive_data(data_links$object[i])
    message("Loaded object ", data_links$object[i])
  }
    
  
  return(objs_loaded)
  
}
