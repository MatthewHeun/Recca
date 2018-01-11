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
  edge_list <- do.call(rbind, list(
    data.frame(from = "p_ind_1", to = "p_prod_1", weight = 10),
    data.frame(from = "p_ind_2", to = "p_prod_1", weight = 12),
    data.frame(from = "p_ind_3", to = "p_prod_1", weight = 14),
    data.frame(from = "p_prod_1", to = "pf_ind_1", weight = 14),
    data.frame(from = "pf_ind_1", to = "f_prod_1", weight = 14),
    data.frame(from = "f_prod_1", to = "fd_ind_1", weight = 14),
    data.frame(from = "Stock changes", to = "p_prod_1", weight = 6),
    data.frame(from = "pf_ind_1", to = "fd_ind_1", weight = 14),
    data.frame(from = "f_prod_1", to = "Bunker", weight = 14)
  ))

  layout <- ecc_layout(Industries = Industry_meta,
                       Products = Product_meta,
                       g = qgraph(edge_list, DoNotPlot = TRUE))

  layout_df <- data.frame(layout)
  expect_equal(layout_df["p_ind_1", "x"], 1)
  expect_equal(layout_df["p_ind_2", "x"], 1)
  expect_equal(layout_df["p_ind_3", "x"], 1)
  expect_equal(layout_df["p_prod_1", "x"], 2)
  expect_equal(layout_df["pf_ind_1", "x"], 3)
  expect_equal(layout_df["f_prod_1", "x"], 4)
  expect_equal(layout_df["fd_ind_1", "x"], 5)
  expect_equal(layout_df["p_ind_1", "y"], 3)
  expect_equal(layout_df["p_ind_2", "y"], 2)
  expect_equal(layout_df["p_ind_3", "y"], 1)
  expect_equal(layout_df["p_prod_1", "y"], 2)
  expect_equal(layout_df["pf_ind_1", "y"], 2)
  expect_equal(layout_df["f_prod_1", "y"], 2)
  expect_equal(layout_df["fd_ind_1", "y"], 2)
  expect_equal(layout_df["Stock changes", "x"], 2.5)
  expect_equal(layout_df["Bunker", "x"], 3.5)
  expect_equal(layout_df["Stock changes", "y"], 4)
  expect_equal(layout_df["Bunker", "y"], 4)

  # Make a qgraph network plot.
  # qgraph(edge_list, layout = layout)

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
  Products <- data.frame(Product = c("NG", "NG - Wells", "NG - Dist.",
                                     "Elect", "Elect - Grid",
                                     "Crude", "Crude - Fields", "Crude - Dist.",
                                     "Diesel", "Petrol", "Diesel - Dist.", "Petrol - Dist."),
                         Stage = c("Primary", "Primary extracted", "Primary distributed",
                                   "Final", "Final distributed",
                                   "Primary", "Primary extracted", "Primary distributed",
                                   "Final", "Final", "Final distributed", "Final distributed"))
  # Create layout for qgraph
  layout = ecc_layout(g = qgraph(bigmat, DoNotPlot = TRUE),
                      Industries = Industries, Products = Products)

  # Convert to a data frame for easier testing
  layout_df <- as.data.frame(layout)

  expect_equal(layout_df["Crude", "x"], 2)
  expect_equal(layout_df["Crude", "y"], 1.5)

  expect_equal(layout_df["Crude - Dist.", "x"], 6)
  expect_equal(layout_df["Crude - Dist.", "y"], 1.5)

  expect_equal(layout_df["Crude - Fields", "x"], 4)
  expect_equal(layout_df["Crude - Fields", "y"], 1.5)

  expect_equal(layout_df["Crude dist.", "x"], 5)
  expect_equal(layout_df["Crude dist.", "y"], 1.5)

  expect_equal(layout_df["Diesel", "x"], 8)
  expect_equal(layout_df["Diesel", "y"], 2)

  expect_equal(layout_df["Diesel - Dist.", "x"], 10)
  expect_equal(layout_df["Diesel - Dist.", "y"], 2)

  expect_equal(layout_df["Diesel dist.", "x"], 9)
  expect_equal(layout_df["Diesel dist.", "y"], 2)

  expect_equal(layout_df["Elect", "x"], 8)
  expect_equal(layout_df["Elect", "y"], 3)

  expect_equal(layout_df["Elect - Grid", "x"], 10)
  expect_equal(layout_df["Elect - Grid", "y"], 3)

  expect_equal(layout_df["Elect. grid", "x"], 9)
  expect_equal(layout_df["Elect. grid", "y"], 3)

  expect_equal(layout_df["Gas wells & proc.", "x"], 3)
  expect_equal(layout_df["Gas wells & proc.", "y"], 2.5)

  expect_equal(layout_df["NG", "x"], 2)
  expect_equal(layout_df["NG", "y"], 2.5)

  expect_equal(layout_df["NG - Dist.", "x"], 6)
  expect_equal(layout_df["NG - Dist.", "y"], 2.5)

  expect_equal(layout_df["NG - Wells", "x"], 4)
  expect_equal(layout_df["NG - Wells", "y"], 2.5)

  expect_equal(layout_df["NG dist.", "x"], 5)
  expect_equal(layout_df["NG dist.", "y"], 2.5)

  expect_equal(layout_df["Oil fields", "x"], 3)
  expect_equal(layout_df["Oil fields", "y"], 1.5)

  expect_equal(layout_df["Oil refineries", "x"], 7)
  expect_equal(layout_df["Oil refineries", "y"], 1.5)

  expect_equal(layout_df["Petrol", "x"], 8)
  expect_equal(layout_df["Petrol", "y"], 1)

  expect_equal(layout_df["Petrol - Dist.", "x"], 10)
  expect_equal(layout_df["Petrol - Dist.", "y"], 1)

  expect_equal(layout_df["Petrol dist.", "x"], 9)
  expect_equal(layout_df["Petrol dist.", "y"], 1)

  expect_equal(layout_df["Power plants", "x"], 7)
  expect_equal(layout_df["Power plants", "y"], 2.5)

  expect_equal(layout_df["Residential", "x"], 11)
  expect_equal(layout_df["Residential", "y"], 2.5)

  expect_equal(layout_df["Resources - Crude", "x"], 1)
  expect_equal(layout_df["Resources - Crude", "y"], 1.5)

  expect_equal(layout_df["Resources - NG", "x"], 1)
  expect_equal(layout_df["Resources - NG", "y"], 2.5)

  expect_equal(layout_df["Transport", "x"], 11)
  expect_equal(layout_df["Transport", "y"], 1.5)

  # Check that the order is correct
  expect_equal(rownames(layout_df), c("Crude",
                                   "Crude - Dist.",
                                   "Crude - Fields",
                                   "Crude dist.",
                                   "Diesel",
                                   "Diesel - Dist.",
                                   "Diesel dist.",
                                   "Elect",
                                   "Elect - Grid",
                                   "Elect. grid",
                                   "Gas wells & proc.",
                                   "NG",
                                   "NG - Dist.",
                                   "NG - Wells",
                                   "NG dist.",
                                   "Oil fields",
                                   "Oil refineries",
                                   "Petrol",
                                   "Petrol - Dist.",
                                   "Petrol dist.",
                                   "Power plants",
                                   "Residential",
                                   "Resources - Crude",
                                   "Resources - NG",
                                   "Transport"))

  # Make a qgraph network plot.
  # qgraph(bigmat, layout = layout, cut = 70, curveAll = TRUE, curve = 1)
})

