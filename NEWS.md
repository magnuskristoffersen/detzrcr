# detzrcr 0.2.5
* Initial epsilon Hf as input data now works.
* Saving plots/tables automatically adds date and time to filenames to avoid
  overwriting. NB! Filenames still have to be manually fixed when using RStudio
  itegrated browser on Windows.
* Sample names can now start with a number.
* Automatically removes rows with non-numerical values in discordance-column.
  The row numbers of removed rows are printed.

# detzrcr 0.2.4
* Fixed error in y-axis slider for Hf plot
* Added option for y-axis error bar for Hf plot

# detzrcr 0.2.3
* Added option to use fixed y-axis in KDE/PDP plots
* Changed axis limit sliders to numeric input

# detzrcr 0.2.2
* Added optional Hf errorbars
* Added bandwidth selection in Hf plot

# detzrc 0.2.1
* Fixed misspelling in ECDF plot.
* Updated citation file.
* Maxima calculation in Reimink plot works

# detzrcr 0.2.0
* Updated citation file.
* Added Reimink et al. (2016) intercept calculation and plot.

# detzrcr 0.1.3
* Fixed error in likeness and 1-O tables occuring if in-data did not include
Hf-data.
* Fixed display error of graphical 1-O table if not all categories are present
in the data.
* Added vignette.

# detzrcr 0.1.2
* Added option to specify x-axis breaks for plots.
* Fixed 1-O calculation.
* Added option to combine result matrices (U-Pb upper triangle and Lu-Hf lower
triangle).
* Added graphical representation of 1-O matrix.
* Interpolate ecdfs over entire calculation range.
* Added plot options panel (only font options are included for now).
* In-data can now include empty cells.

# detzrcr 0.1.1
* User interface now actually works.

# detzrcr 0.1.0
* First release of the detzrcr package.
