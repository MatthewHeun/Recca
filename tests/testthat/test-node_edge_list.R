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
  R_mat <- sutmats$R[[1]]
  U_mat <- sutmats$U[[1]]
  V_mat <- sutmats$V[[1]]
  Y_mat <- sutmats$Y[[1]]
  el_final <- edge_list(R = R_mat, U = U_mat, V = V_mat, Y = Y_mat)[["Edge list"]]
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
    # The testing strategy is to gather all name/ID pairs and count.
    # Gather from IDs and to IDs and bind them together.
    fromIDs <- el %>% select(From, From_node_id) %>% rename(Name = From, ID = From_node_id)
    toIDs <- el %>% select(To, To_node_id) %>% rename(Name = To, ID = To_node_id)
    fromtoIDs <- bind_rows(fromIDs, toIDs)
    # Count the number of unique pairs.
    n_unique_pairs <- fromtoIDs %>% unique() %>% nrow()
    # Count the number of unique names.
    n_unique_names <- fromtoIDs %>% select(Name) %>% unique() %>% nrow()
    # Count the number of uniqe ID numbers.
    n_unique_ids <- fromtoIDs %>% select(ID) %>% unique() %>% nrow()
    # We expect that the number of unique pairs is the same as the number of unique names.
    # If not, one of the names is paired with two or more ID numbers.
    expect_equal(n_unique_pairs, n_unique_names)
    # We expect that the number of unique names is the same as the number of unique ID numbers.
    # If not, something is amiss.
    expect_equal(n_unique_names, n_unique_ids)
  }
})

test_that("node_list works as expected", {
  # Test a single edge list
  sutmats <- UKEnergy2000mats %>% spread(key = matrix.name, value = matrix)
  el <- edge_list(sutmats)$`Edge list`[[1]]
  nl <- node_list(edge_list = el)
  # Verify a few of the node numbers
  expect_equal(filter(nl, Node == "Resources - Crude")$node_id, 0)
  expect_equal(filter(nl, Node == "Diesel dist.")$node_id, 4)
  expect_equal(filter(nl, Node == "NG dist.")$node_id, 8)
  expect_equal(filter(nl, Node == "Transport")$node_id, 11)
  expect_equal(filter(nl, Node == "Waste")$node_id, 13)
})


test_that("edge_list works when R is missing", {
  sutmats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      V = matsbyname::sum_byname(R, V),
      R = NULL
    )
  elDF <- edge_list(sutmats)
  expect_equal(elDF$`Edge list`[[1]][["edge_id"]], seq(1, 39))
})

