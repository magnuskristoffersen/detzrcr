# detzrcr

[![Build Status](https://travis-ci.org/magnuskristoffersen/detzrcr.svg?branch=master)](https://travis-ci.org/magnuskristoffersen/detzrcr)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/detzrcr)](https://cran.r-project.org/package=detzrcr)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3324801.svg)](https://doi.org/10.5281/zenodo.3324801)

detzrcr is an R-package for plotting and running statistical tests on univariate
(U-Pb age) and bivariate (U-Pb age and Lu-Hf data) detrital zircon data for
comparison between samples. The package uses a user interface built with shiny
and produces publication quality figures using ggplot2.

## Installation

Install either from CRAN

```R
install.packages("detzrcr")
```

or install the development version from github (requires devtools)

```R
# install.packages("devtools")
devtools::install_github("magnuskristoffersen/detzrcr")
```

## Use package

Launch user interface with

```R
library("detzrcr")
run_detzrcr()
```

For the sake of simplicity input data is required to be a csv-file which
contains at least a column containing the U-Pb age data. This column must be
called age, associated uncertainty and discordancy (if included) must be in
columns uncert and disc, respectively. All ages must be in Ma.

The file can contain the Lu-Hf data in two ways:
measured 176/177Hf and measured Lu/Hf in columns called hfhf and luhf,
respectively; or a column with initial epsilon-Hf with the name ehf_i.

Several samples can be included in the csv-file and which one(s) to plot can
be choosen interactively in the user interface. For this to work a column
called sample must be present, and each analysis (i.e. each row) must contain
the name of the sample it belongs to in this column.

Detrital zircon U-Pb and Lu-Hf example data from the Natal Group, KwaZulu-Natal,
South Africa (Kristoffersen et al. 2016) are included in the package.

### References

Kristoffersen, M., Andersen, T., Elburg, M.A., Watkeys, M.K., 2016.
Detrital zircon in a supercontinental setting: locally derived and
far-transported components in the Ordovician Natal Group, South Africa.
J. Geol. Soc. London. 173, 203-215. <https://dx.doi.org/10.1144/jgs2015-012>
