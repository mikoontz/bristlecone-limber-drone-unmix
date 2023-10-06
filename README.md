# bristlecone-limber-drone-unmix

*last updated 5 Oct 2023*

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

### Structure

Main directories of the repository:

* `src` - source code, includes wrapper functions, one function per file; these files get sourced at the beginning of scripts
* `data` - contains a file `data_links.csv` with links to data stored on Google drive
* `deploy` - scripts that perform analysis

Within the `deploy` directory is a `sandbox` directory with testing scripts and other smaller scripts that are used as examples/proof of concept.

The `deploy` directory (as of 5 Oct. 2023) has the following scripts:

* `prepare-survey-polygons.R` - script for downloading and processing the polygon dataset (from the Bentz crew's survey in August 2023)
* `prepare-survey-points.R`- script for downloading and processing the point dataset (used in the Aggieair analysis)
* `tune-ranger-hyperparameters_poly-data.R` - script for fitting models with the polygon dataset over a grid of hyperparameters and comparing performance with a generalized form of Matthew's correlation coefficient
* `tune-ranger-hyperparameters_point-data.R` - script for fitting models with the point dataset over a grid of hyperparameters and comparing performance

Notes about these:
- Each preparation script reads in an orthomosaic over the study area with intensities of ten bands per pixel; the processing script also estimates other indices from these bands
- All models are random forests fit with the `ranger` R package (very fast) and do spatially stratified sampling using the package `spatialsample`
- The wrapper function that fits the random forest models assumes that the classification for each point/polygon is stored in a column named `class` and that all of the remaining columns (except for coordinates) fed into the function are features to fit the model with (i.e., bands and indices). The preparation scripts (which currently have features in the raw data hard-coded in) should set up these data frames appropriately.
- The polygon dataset has six classes and they are currently not labeled informatively (they use numbers 1-6). The point dataset has only four classes (bristlecone pine, limber pine, other living tree, and dead tree).
- Because the polygon dataset includes multiple points within each polygon, the script performs an additional step of getting mean and standard deviation of intensity of each band or index per polygon. As such there are twice as many features in the models with the polygon dataset.

One other (pair of) sandbox script(s) of note:
* `assess-model_point-data.R`
* `assess-model_poly-data.R`

These scripts fit models at one parameter combination (as determined by the tuning functions) and do visualizations of the feature importances and confusion matrix. These visualizations are crude and preliminary but may provide a foundation for future work.

### Contact

Contact Scott (@swnordstrom) or Mike (@mikoontz) for more information. They may direct you to others as needed.
