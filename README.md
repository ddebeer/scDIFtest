[![Build Status](https://travis-ci.org/ddebeer/scDIFtest.svg?branch=master)](https://travis-ci.org/ddebeer/scDIFtest)

# Item-wise score-based tests for DIF detection

This R-package allows efficient item-wise score-based tests for DIF-detection. The `scDIFtest` is nothing more than a handy wrapper around the `sctest` function from the `strucchange`-package. `scDIFtest` only works for fitted IRT models that are returned by the `mirt`-package. The computation of the scores is also done by the `mirt`-package.

Score-based DIF-tests can be executed for all items (or a selection of the items) simultaneously. Since the goal is to detect possible DIF in every item individually, no correction for multiple testing is included. 


## Installation


The package can be installed using using the `devtools`-package:

```
install.packages("devtools")
devtools::install_github("ddebeer/scDIFtest")
```
