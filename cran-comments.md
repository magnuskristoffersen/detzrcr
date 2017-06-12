## Test environments
local osx install (10.12.3), R 3.3.2

docker image rocker/r-devel, R Under development (unstable) (2017-01-18 r72010)

winbuilder (devel, R Under development (unstable) (2017-06-10 r72776))

ubuntu 12.04.5 LTS (on travis-ci), R 3.3.2

## R CMD check --as-cran results
There were no ERRORs or WARNINGs.
There was 1 NOTE: "checking CRAN incoming feasibility ... NOTE". Which,
according to this r-devel mailing list answer
https://stat.ethz.ch/pipermail/r-devel/2014-March/068497.html, is a note to the
CRAN maintainers to check that the package are submitted by the package
maintainer and not anybody else.

## Downstream dependencies
There are currently no downstream dependencies on this package.
