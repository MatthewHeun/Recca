# Contains tests for the Recc package.

# Need to put dplyr before testthat.
# If not, the "matches" function in dplyr overrides the "matches" function in testthat,
# and tests containing the string "(" don't work as expectged.

library(dplyr)
library(tidyr)
library(magrittr)
library(tibble)
library(lazyeval)
library(byname)
library(testthat)

###########################################################
context("small example")
###########################################################

test_that("small example works as expected", {
  p_ind <- "p_ind_1"
  p_prod <- "p_prod_1"
  pf_ind <- "pf_ind_1"
  f_prod <- "f_prod_1"
  fd_ind <- "fd_ind_1"
  names <- c("p_ind_1", "p_prod_1", "pf_ind_1", "f_prod_1", "fd_ind_1")
  g <- qgraph(matrix(c(0, 10, 0, 0, 0,
                       0,  0, 9, 0, 0,
                       0,  0, 0, 8, 0, 
                       0,  0, 0, 0, 7, 
                       0,  0, 0, 0, 0),
                     nrow = 5, byrow = TRUE,
                     dimnames = list(names, names)), 
              DoNotPlot = TRUE)
  calc_ecc_layout(g, 
                  p_industries = p_ind, 
                  p_products = p_prod,
                  pf_industries = pf_ind,
                  f_products = f_prod,
                  fd_industries = fd_ind)
})