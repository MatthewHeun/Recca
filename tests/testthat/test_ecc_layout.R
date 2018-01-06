# Contains tests for the Recca package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expected.

library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)
library(lazyeval)
library(byname)
library(testthat)
library(qgraph)

###########################################################
context("small example")
###########################################################

test_that("small example works as expected", {

  Industry_meta <- data.frame(
    Industries = c("p_ind_1", "pf_ind_1", "fd_ind_1"),
    Stage = c("Primary", "Primary --> Final", "Final demand")
  )
  Product_meta <- data.frame(
    Products = c("p_prod_1", "f_prod_1"),
    Stage = c("Primary", "Final")
  )
  ecc_layout(Industry_meta, Product_meta)

})
