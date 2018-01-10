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
library(matsindf)

###########################################################
context("small example")
###########################################################

test_that("small example works as expected", {
  Industry_meta <- data.frame(
    Industry = c("Stock changes", "p_ind_1", "pf_ind_1", "fd_ind_1", "p_ind_2", "p_ind_3", "Bunker"),
    Stage = c("Storage", "Primary industry", "Primary --> Final", "Final demand",
              "Primary industry", "Primary industry", "Storage"),
    Group = c(NA, "Oil", NA, NA, "Oil", "Coal", NA),
    stringsAsFactors = FALSE
  )
  Product_meta <- data.frame(
    Product = c("p_prod_1", "f_prod_1"),
    Stage = c("Primary product", "Final product"),
    Group = c(NA, "Oil"),
    stringsAsFactors = FALSE
  )
  layout <- ecc_layout(Industries = Industry_meta, Products = Product_meta)

  layout_named <- data.frame(layout) %>% column_to_rownames(var = "Node_name")
  expect_equal(layout_named["p_ind_1", "x"], 1)
  expect_equal(layout_named["p_ind_2", "x"], 1)
  expect_equal(layout_named["p_ind_3", "x"], 1)
  expect_equal(layout_named["p_prod_1", "x"], 2)
  expect_equal(layout_named["pf_ind_1", "x"], 3)
  expect_equal(layout_named["f_prod_1", "x"], 4)
  expect_equal(layout_named["fd_ind_1", "x"], 5)
  expect_equal(layout_named["p_ind_1", "y"], 3)
  expect_equal(layout_named["p_ind_2", "y"], 2)
  expect_equal(layout_named["p_ind_3", "y"], 1)
  expect_equal(layout_named["p_prod_1", "y"], 2)
  expect_equal(layout_named["pf_ind_1", "y"], 2)
  expect_equal(layout_named["f_prod_1", "y"], 2)
  expect_equal(layout_named["fd_ind_1", "y"], 2)
  expect_equal(layout_named["Stock changes", "x"], 2.5)
  expect_equal(layout_named["Bunker", "x"], 3.5)
  expect_equal(layout_named["Stock changes", "y"], 4)
  expect_equal(layout_named["Bunker", "y"], 4)
})


###########################################################
context("UKEnergy2000")
###########################################################

test_that("UKEnergy2000 works as expected", {
  # Use the UKEnergy2000 data from the matsindf package as an example.
  bigmat <- UKEnergy2000 %>%
    # Add metadata
    add_matnames() %>%
    add_row_col_meta() %>%
    # Group correctly
    group_by(Country, Year, matname) %>%
    # Ensure all entries are positive numbers, as they should be in PSUT framework.
    mutate(
      E.ktoe = abs(E.ktoe)
    ) %>%
    # Make matrices
    collapse_to_matrices(matnames = "matname", values = "E.ktoe",
                         rownames = "rowname", colnames = "colname",
                         rowtypes = "rowtype", coltypes = "coltype") %>%
    rename(
      matrix = E.ktoe
    ) %>%
    # Spread so we can add all matrices
    spread(key = matname, value = matrix) %>%
    # Sum all matrices to make one big matrix with "from" or "source" nodes in rows
    # and "to" or "destination" nodes in columns
    mutate(
      # First, set row and column types so they can be summed.
      U = U %>% setrowtype("Source") %>% setcoltype("Destination"),
      V = V %>% setrowtype("Source") %>% setcoltype("Destination"),
      Y = Y %>% setrowtype("Source") %>% setcoltype("Destination"),
      # Do the sum.
      bigsum = sum_byname(U, V) %>% sum_byname(Y)
    ) %>%
    # Next 2 lines are like saying .$bigsum[[1]]
    use_series(bigsum) %>%
    extract2(1) %>%
    # If we don't complete_and_sort(), qgraph mis-interprets the data.
    # qgraph does not work "byname."
    complete_and_sort()

  # Make data frames with information about stages and groups.
  Industries <- data.frame(Industry = c("Resources - NG", "Gas wells & proc.",
                                        "NG dist.", "Power plants", "Elect. grid",
                                        "Residential",
                                        "Resources - Crude", "Oil fields",
                                        "Crude dist.", "Oil refineries", "Diesel dist.", "Petrol dist.",
                                        "Transport"),
                           Stage = c("Resources", "Extraction",
                                     "Primary dist.", "Primary --> Final", "Final dist.",
                                     "Final demand",
                                     "Resources", "Extraction",
                                     "Primary dist.", "Primary --> Final", "Final dist.", "Final dist.",
                                     "Final demand"))
  Products <- data.frame(Product = c("NG", "NG - Wells", "NG - dist.",
                                     "Elect.", "Elect. - Grid",
                                     "Crude", "Crude - Fields", "Crude - dist.",
                                     "Diesel", "Petrol", "Diesel - dist.", "Petrol - dist."),
                         Stage = c("Primary", "Primary extracted", "Primary distributed",
                                   "Final", "Final distributed",
                                   "Primary", "Primary extracted", "Primary distributed",
                                   "Final", "Final", "Final distributed", "Final distributed"))
  # Create layout for qgraph
  layout = ecc_layout(Industries = Industries, Products = Products)
  # Make a qgraph network.
  g <- qgraph(bigmat, layout = layout)
})
