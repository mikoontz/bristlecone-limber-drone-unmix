# SN - init 5 Sept 2023
# Script for assessing additions to the data-links.csv file

###

library(dplyr)
library(tidyr)

### 5 Sept 2023
#   Check for mistakes in entering training data

datalinks = read.csv(here::here('data', 'data-links.csv'))

head(datalinks)
nrow(datalinks)

# Get only relevant files (trees or dead)
datalinks = datalinks %>% filter(grepl('train', object))

nrow(datalinks)
# 40 objects (good)

# Check for duplicated objects
any(duplicated(datalinks$object)) # none - good
# Check for duplicated links
any(duplicated(datalinks$link)) # none - good

# Check for completeness of species and filetypes
datalinks %>%
  separate(col = object, into = c('species', 'filetype'), sep = '_train_') %>%
  complete.cases(.$species, .$filetype) %>%
  all()

# Looks like we are all good

datalinks %>%
  separate(col = object, into = c('species', 'filetype'), sep = '_train_') %>%
  group_by(species) %>%
  summarise(n = n())
# Five species, each with eight files (filetypes)


### Clear namespace

detach("package:dplyr")
detach("package:tidyr")
