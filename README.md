# bristlecone-limber-drone-unmix

Repository for initial project to build an open-source workflow for reading in drone and survey data, processing this data, and running random forest classification models that identify different pine species (*Pinus longaeva*, bristlecone pine, and *Pinus flexilis*, limbercone pine), as well as dead trees and other types of vegetation.

### Packages required

The following packages are necessary for running these scripts:

* `here` - used for flexible path names
* `assertthat` - used for error-checking functions and providing custom error messages
* `googledrive` - necessary for reading in outside data (stored on Google Drive)
* `dplyr` - used for data manipulation
* `tidyr` - used for data manipulation
* `terra` - used for reading in and handling spatial data (vector and raster)
* `spatialsample` - this and dependencies (`rsample`) are used for building test/train splits for model fitting
* `ranger` - used for fitting random forest models quickly

In addition, the following packages can be used for special features or improved performances:

* `ggplot2` - used for plotting some results
* `parallel` - fitting random forest models in parallel
