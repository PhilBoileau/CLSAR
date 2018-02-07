
<!-- README.md is generated from README.Rmd. Please edit that file -->
CLSAR
=====

CLSAR is a collection of R functions that makes working with CLSA data easier.

Installation
------------

You can install CLSAR from github with:

``` r
install.packages("devtools")
devtools::install_github("PhilBoileau/CLSAR")
```

List of Functions
-----------------

``` r
summaryTable(vect, categories, catname, missingValues)
wgtSummaryTable(vect, wgts, categories, catname, missingValues)
loadCLSAData(path)
compareNA(value1, value2)
gridArrangeSharedLegend(..., ncol, nrow, position)
```
