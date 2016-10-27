## Test environments
local osx install (10.12.1), R 3.3.1

docker image rocker/r-devel, R Under development (unstable) (2016-10-17 r71531)

winbuilder (devel, R Under development (unstable) (2016-08-22 r71129))

ubuntu 12.04.5 LTS (on travis-ci), R 3.3.1

## R CMD check --as-cran results
There were no ERRORs or WARNINGs.
There was 1 NOTE: "checking CRAN incoming feasibility ... NOTE". Which,
according to this r-devel mailing list answer
https://stat.ethz.ch/pipermail/r-devel/2014-March/068497.html, is a note to the
CRAN maintainers to check that the package are submitted by the package
maintainer and not anybody else.

## Downstream dependencies
There are currently no downstream dependencies on this package.

## Additional comments
Fixed calculations and added plotting options.
