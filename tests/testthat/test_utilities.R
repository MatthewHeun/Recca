library(Hmisc)

###########################################################
context("Utilities")
###########################################################

test_that("error messages about column names works as expected", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  newcols <- c("c", "d", "a", "b")
  expect_error(verify_cols_missing(df, newcols),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))

  expect_silent(verify_cols_missing(df, c("d")))

  # Try with a list in newcols
  expect_silent(verify_cols_missing(df, list("d", "e")))
  expect_error(verify_cols_missing(df, list("a", "c")),
               Hmisc::escapeRegex("column(s) 'a' is (are) already column names in data frame 'df'"))
})

test_that("verify_cols_missing works when either strings or names are provided", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  # Try with strings
  newcols <- c("a", "b")
  expect_error(verify_cols_missing(df, newcols),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))
  # Try with names
  newcolnames <- lapply(newcols, as.name)
  expect_error(verify_cols_missing(df, newcolnames),
               Hmisc::escapeRegex("column(s) 'a', 'b' is (are) already column names in data frame 'df'"))
})

test_that("verify_cols_missing works with a single value", {
  df <- data.frame(a = c(1,2), b = c(3,4))
  expect_silent(verify_cols_missing(df, as.name("c")))
  expect_error(verify_cols_missing(df, as.name("a")),
               Hmisc::escapeRegex("column(s) 'a' is (are) already column names in data frame 'df'"))
})

test_that("any_start_with works properly", {
  expect_true(any_start_with(x = c("a", "b", "c"), target = "b"))
  expect_true(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Production"))
  expect_false(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Offshore"))
  expect_false(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Crude"))
  expect_equal(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = c("Production", "Offshore")),
               c(TRUE, FALSE))
  # Does it also work with lists?
  expect_equal(any_start_with(x = list("Production - Crude", "Production - NG", "Bogus"), target = c("Production", "Offshore")),
               c(TRUE, FALSE))
})

test_that("starts_with_any_of works properly", {
  expect_true(starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "prefix")))
  expect_false(starts_with_any_of(x = "prefix - suffix", target = c("a", "b", "c")))
  expect_false(starts_with_any_of(x = "prefix - suffix", target = "suffix"))
  expect_equal(starts_with_any_of(x = c("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
  # Does it also work with lists?
  expect_equal(starts_with_any_of(x = list("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  target = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
})

test_that("primary_industries works correctly", {
  mats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  expected <- c("Resources - Crude", "Resources - NG")
  expect_equal(primary_industries(mats)[["p_industries"]],
               list(expected, expected, expected, expected))
  # Try with individual matrices
  for (i in 1:nrow(mats)) {
    expect_equal(primary_industries(U = mats$U[[i]], V = mats$V[[i]]) %>% set_names(NULL) %>% unlist(), expected)
  }
})


###########################################################
context("Edge list")
###########################################################

test_that("edge_list (unsimplified) works correctly", {
  sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  eldf <- edge_list(sutmats, simplify_edges = FALSE)
  # Verify a few of the data points
  el_final <- eldf$`Edge list`[[1]] %>%
    mutate_if(is.factor, as.character)
  expect_equal(el_final %>%
                 filter(From == "Crude - Dist.", To == "Crude dist.", Product == "Crude - Dist."),
               data.frame(From = "Crude - Dist.", To = "Crude dist.", Value = 500, Product = "Crude - Dist.", stringsAsFactors = FALSE))
  expect_equal(el_final %>%
                 filter(From == "Crude dist.", To == "Crude - Dist.", Product == "Crude - Dist."),
               data.frame(From = "Crude dist.", To = "Crude - Dist.", Value = 47500, Product = "Crude - Dist.", stringsAsFactors = FALSE))
})

test_that("edge_list (simplified) works correctly", {
  # Make an edge list that chould be simplified.
  el <- data.frame(From = c("A", "Oil"), To = c("Oil", "C"), Value = c(42, 42), Product = c("Oil", "Oil"), stringsAsFactors = FALSE)
  el_simple <- Recca:::simplify_edge_list(el)
  expect_equal(el_simple, data.frame(From = "A", To = "C", Value = 42, Product = "Oil", stringsAsFactors = FALSE))

  # Try another edge list that should be simplified, one that loops back on itself in self-use fashion.
  el_A <- data.frame(From = c("A", "Oil"), To = c("Oil", "A"), Value = c(42, 42), Product = c("Oil", "Oil"), stringsAsFactors = FALSE)
  el_simple_A <- Recca:::simplify_edge_list(el_A)
  expect_equal(el_simple_A, data.frame(From = "A", To = "A", Value = 42, Product = "Oil", stringsAsFactors = FALSE))

  # Now try with a bigger ECC.
  sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  eldf_simplified <- edge_list(sutmats, simplify_edges = TRUE)
  el_final_simplified <- eldf_simplified$`Edge list`[[1]] %>%
    mutate_if(is.factor, as.character)
  # Verify a few of the data points
  expect_equal(el_final_simplified %>%
                 filter(From == "Crude dist.", To == "Crude dist.", Product == "Crude - Dist."),
               data.frame(From = "Crude dist.", To = "Crude dist.", Value = 500, Product = "Crude - Dist.", stringsAsFactors = FALSE))
  expect_equal(el_final_simplified %>%
                 filter(From == "Elect. grid", To == "Residential", Product == "Elect - Grid"),
               data.frame(From = "Elect. grid", To = "Residential", Value = 6000, Product = "Elect - Grid", stringsAsFactors = FALSE))
  expect_equal(el_final_simplified %>%
                 filter(From == "Oil refineries", To == "Oil refineries", Product == "Diesel"),
               data.frame(From = "Oil refineries", To = "Oil refineries", Value = 5000, Product = "Diesel", stringsAsFactors = FALSE))
})

test_that("waste_edges works as expected", {
  sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  eldf <- edge_list(sutmats)
  el_final <- eldf$`Edge list`[[1]] %>%
    mutate_if(is.factor, as.character)
  # Verify that a few of the numbers are correct
  expect_equal(el_final %>%
                 filter(From == "Crude dist.", To == "Waste", Product == "Waste"),
               data.frame(From = "Crude dist.", To = "Waste", Value = 550, Product = "Waste", stringsAsFactors = FALSE))
  expect_equal(el_final %>%
                 filter(From == "Power plants", To == "Waste", Product == "Waste"),
               data.frame(From = "Power plants", To = "Waste", Value = 9700, Product = "Waste", stringsAsFactors = FALSE))
  expect_equal(el_final %>%
                 filter(From == "Gas wells & proc.", To == "Waste", Product == "Waste"),
               data.frame(From = "Gas wells & proc.", To = "Waste", Value = 2075, Product = "Waste", stringsAsFactors = FALSE))

  # Test waste_edges when only matrices are specified
  Umat <- sutmats$U[[1]]
  Vmat <- sutmats$V[[1]]
  Ymat <- sutmats$Y[[1]]
  el_final <- edge_list(U = Umat, V = Vmat, Y = Ymat)[["Edge list"]]
  expect_equal(el_final %>%
                 filter(From == "Crude dist.", To == "Crude dist.", Product == "Crude - Dist."),
               data.frame(From = "Crude dist.", To = "Crude dist.", Value = 500, Product = "Crude - Dist.", stringsAsFactors = FALSE))
  expect_equal(el_final %>%
                 filter(From == "Elect. grid", To == "Residential", Product == "Elect - Grid"),
               data.frame(From = "Elect. grid", To = "Residential", Value = 6000, Product = "Elect - Grid", stringsAsFactors = FALSE))
  expect_equal(el_final %>%
                 filter(From == "Oil refineries", To == "Oil refineries", Product == "Diesel"),
               data.frame(From = "Oil refineries", To = "Oil refineries", Value = 5000, Product = "Diesel", stringsAsFactors = FALSE))

  expect_equal(el_final %>%
                 filter(From == "Crude dist.", To == "Waste", Product == "Waste"),
               data.frame(From = "Crude dist.", To = "Waste", Value = 550, Product = "Waste", stringsAsFactors = FALSE))
  expect_equal(el_final %>%
                 filter(From == "Power plants", To == "Waste", Product == "Waste"),
               data.frame(From = "Power plants", To = "Waste", Value = 9700, Product = "Waste", stringsAsFactors = FALSE))
  expect_equal(el_final %>%
                 filter(From == "Gas wells & proc.", To == "Waste", Product == "Waste"),
               data.frame(From = "Gas wells & proc.", To = "Waste", Value = 2075, Product = "Waste", stringsAsFactors = FALSE))

})
