## Test environments
local osx install (10.11.6), R 3.3.1

docker image rocker/r-devel, R Under development (unstable) (2016-07-31 r71015)

winbuilder (devel, R Under development (unstable) (2016-08-22 r71129))

ubuntu 12.04.5 LTS (on travis-ci), R 3.3.1

## R CMD check --as-cran results
There were no ERRORS or WARNINGS.
There was 1 NOTE: "checking CRAN incoming feasibility ... NOTE". Which,
according to this r-devel mailing list answer
https://stat.ethz.ch/pipermail/r-devel/2014-March/068497.html, is a note to the
CRAN maintainers to check that the package are submitted by the package
maintainer and not anybody else.

## Downstream dependencies
There are currently no downstream dependencies on this package.

## Additional comments
The user interface failed to launch (as such the package currently on CRAN does
not work) because of an error on my part in not
exporting some needed constants. Binaries have been built for osx and windows
and the user interface now works.
