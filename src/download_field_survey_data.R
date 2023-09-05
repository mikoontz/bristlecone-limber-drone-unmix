#' @description 
#' Function will download all field survey data
#' 
#' @param species
#' Species to read in (in our implementation, one of PILO, PIEN, PIFL, firs, dead)
#' @param filetype
#' Filetype to read in (in our implementation, one of cpg, dbf, prj, sbn, sbx, shp, shpxml, shx)

download_field_survey_data <- function(species = NULL, filetype = NULL) {
  
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
  # This function is also written to handle the following filetypes:
  #    - cpg, dbf, prj, sbn, sbx, shp, shpxml, shx
  # It also assumes a specific coding in the data-links.csv file
  # 
  
  # Get only relevant files (coded beginning with species names)
  data_links <- data_links |>
    dplyr::filter(
      grepl('^PI', object) | grepl('^firs', object) | grepl('^dead', object)
    )
  
  # Go by cases
  # Default case is ALL
  
  if (!is.null(species) | !is.null(filetype)) {
    
    # If species or filetype is specified,  modify the dataframe for easy matching
    data_links <- data_links |>
      dplyr::mutate(split.temp = gsub('train', '_train_', object)) |>
      dplyr::mutate(split.temp = gsub('test', '_test_', split.temp)) |>
      tidyr::separate(col = split.temp, into = c("sp", "train", "ft"), sep = '__')
    
    if (!is.null(species) & is.null(filetype)) {
      
      # Get a regex string for subsetting
      regex_string <- paste(species, collapse = '|^')
      # Add caret to beginning
      regex_string <- paste0('^', regex_string)
      # Subset data_links data frame using above specified 
      data_links <- data_links |> dplyr::filter(grepl(regex_string, sp))
    
    } else if (!is.null(filetype) & is.null(species)) {
      
      regex_string <- paste(filetype, collapse = '$|^')
      # Add caret to beginning and dollar sign to end
      regex_string <- paste0('^', regex_string, '$')
      # Subset data_links using above specified
      data_links <- data_links |> dplyr::filter(grepl(regex_string, ft))
    
    } else {
    
      sp_regex_string <- paste(species, collapse = '|^')
      # Add caret to beginning
      sp_regex_string <- paste0('^', sp_regex_string)
      
      ft_regex_string <- paste(filetype, collapse = '$|^')
      # Add caret to beginning and dollar sign to end
      ft_regex_string <- paste0('^', ft_regex_string, '$')
      
      data_links <- data_links |>
        dplyr::filter(grepl(sp_regex_string, sp)) |>
        dplyr::filter(grepl(ft_regex_string, ft))
      
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
