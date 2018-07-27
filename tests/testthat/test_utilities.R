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
  expect_equivalent(el_final %>%
                      filter(From == "Crude - Dist.", To == "Crude dist.", Product == "Crude - Dist.") %>%
                      select(Value),
                    500)
  expect_equivalent(el_final %>%
                      filter(From == "Crude dist.", To == "Crude - Dist.", Product == "Crude - Dist.") %>%
                      select(Value),
                    47500)
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
  expect_equivalent(el_final_simplified %>%
                      filter(From == "Crude dist.", To == "Crude dist.", Product == "Crude - Dist.") %>%
                      select(Value),
                    500)
  expect_equivalent(el_final_simplified %>%
                      filter(From == "Elect. grid", To == "Residential", Product == "Elect - Grid") %>%
                      select(Value),
                    6000)
  expect_equivalent(el_final_simplified %>%
                      filter(From == "Oil refineries", To == "Oil refineries", Product == "Diesel") %>%
                      select(Value),
                    5000)
})

test_that("waste_edges works as expected", {
  sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  eldf <- edge_list(sutmats)
  el_final <- eldf$`Edge list`[[1]] %>%
    mutate_if(is.factor, as.character)
  # Verify that a few of the numbers are correct
  expect_equivalent(el_final %>%
                      filter(From == "Crude dist.", To == "Waste", Product == "Waste") %>%
                      select(Value),
                    550)
  expect_equivalent(el_final %>%
                      filter(From == "Power plants", To == "Waste", Product == "Waste") %>%
                      select(Value),
                    9700)
  expect_equivalent(el_final %>%
                      filter(From == "Gas wells & proc.", To == "Waste", Product == "Waste") %>%
                      select(Value),
                    2075)

  # Test waste_edges when only matrices are specified
  Umat <- sutmats$U[[1]]
  Vmat <- sutmats$V[[1]]
  Ymat <- sutmats$Y[[1]]
  el_final <- edge_list(U = Umat, V = Vmat, Y = Ymat)[["Edge list"]]
  expect_equivalent(el_final %>%
                      filter(From == "Crude dist.", To == "Crude dist.", Product == "Crude - Dist.") %>%
                      select(Value),
                    500)
  expect_equivalent(el_final %>%
                      filter(From == "Elect. grid", To == "Residential", Product == "Elect - Grid") %>%
                      select(Value),
                    6000)
  expect_equivalent(el_final %>%
                      filter(From == "Oil refineries", To == "Oil refineries", Product == "Diesel") %>%
                      select(Value),
                    5000)

  expect_equivalent(el_final %>%
                      filter(From == "Crude dist.", To == "Waste", Product == "Waste") %>%
                      select(Value),
                    550)
  expect_equivalent(el_final %>%
                     filter(From == "Power plants", To == "Waste", Product == "Waste") %>%
                     select(Value),
                   9700)
  expect_equivalent(el_final %>%
                      filter(From == "Gas wells & proc.", To == "Waste", Product == "Waste") %>%
                      select(Value),
                    2075)
})

test_that("add_edge_id works as expected", {
  sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  elDF <- edge_list(sutmats)
  expect_equal(elDF$`Edge list`[[1]][["edge_id"]], seq(1, 39))
})

test_that("add_node_id_works as expected", {
  sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  elDF <- edge_list(sutmats)
  edge_lists <- elDF$`Edge list`
  for (el in edge_lists) {
    # Test every edge list to ensure that node IDs are matched with same node names everywhere
    fromIDs <- el %>% select(From, From_node_id) %>% rename(Name = From, ID = From_node_id)
    toIDs <- el %>% select(To, To_node_id) %>% rename(Name = To, ID = To_node_id)
    fromtoIDs <- bind_rows(fromIDs, toIDs)
    n_unique_pairs <- fromtoIDs %>% unique() %>% nrow()
    n_unique_names <- fromtoIDs %>% select(Name) %>% unique() %>% nrow()
    expect_equal(n_unique_pairs, n_unique_names)
  }
})
