## Test environments
local osx install (10.11.5), R 3.3.1

docker image rocker/r-devel, R Under development (unstable) (2016-05-31 r70688)

winbuilder (devel, R Under development (unstable) (2016-05-31 r70688))

ubuntu 12.04.5 LTS (on travis-ci), R 3.3.1 and R-devel (R Under development
(unstable) (2016-05-31 r70688))

## R CMD check --as-cran results
There were no ERRORS or WARNINGS. There was 1 NOTE: 
"checking CRAN incoming feasibility ... NOTE".

I suspect that this NOTE is shown because of the very short time since last
submission. If this is the case please let me know and I'll submit after waiting
the appropriate amount of time.

## Downstream dependencies
There are currently no downstream dependencies on this package.

## Additional comments
The user interface failed to launch (as such the package currently on CRAN does
not work) because of an error on my part in not
exporting some needed constants. Binaries have been built for osx and windows
and the user interface now works.
