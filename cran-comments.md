## Test environments
* local ubuntu 18.10, R 3.5.2
* ubuntu 14.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results
There were no ERRORs, WARNINGs or NOTEs

## Downstream dependencies
There are currently no downstream dependencies for this package. 

## Fixed: Version  0.1.1
Version 0.1.1 contains a fix for one of the vignettes which had seen a change
in package dependencies. DESCRIPTION now suggests to load textdata as well
and vignette (which needs to be run interactively) is now in an examples 
folder which is included in .Rbuildignore.