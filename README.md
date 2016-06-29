# detzrcr

detzrcr is an R-package for plotting and running statistical tests on univariate
(U-Pb age) and bivariate (U-Pb age and Lu-Hf data) detrital zircon data for
comparison between samples. The package uses a user interface built with shiny
and produces publication quality figures using ggplot2.


## Installation

Install from github.

```R
install.packages("devtools")
devtools::install_github("magnuskristoffersen/detzrcr")
```

## Use package

Launch user interface with
```R
detzrcr::run_detzrcr()
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
South Africa are included in the package.
