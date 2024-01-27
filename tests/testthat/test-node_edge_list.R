test_that("edge_list() (unsimplified) works correctly", {
  sutmats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix)
  eldf <- edge_list(sutmats, simplify_edges = FALSE)
  # Verify a few of the data points
  el_final <- eldf$`Edge list`[[1]] %>%
    dplyr::mutate_if(is.factor, as.character)
  el_final %>%
    dplyr::filter(From == "Crude [from Dist.]", To == "Crude dist.", Product == "Crude [from Dist.]") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(500)
  el_final %>%
    dplyr::filter(From == "Crude dist.", To == "Crude [from Dist.]", Product == "Crude [from Dist.]") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(47500)
})


test_that("edge_list() (simplified) works correctly", {
  # Make an edge list that chould be simplified.
  el <- data.frame(From = c("A", "Oil"), To = c("Oil", "C"), Value = c(42, 42), Product = c("Oil", "Oil"), stringsAsFactors = FALSE)
  el_simple <- Recca:::simplify_edge_list(el)
  expect_equal(el_simple, data.frame(From = "A", To = "C", Value = 42, Product = "Oil", stringsAsFactors = FALSE))

  # Try another edge list that should be simplified, one that loops back on itself in self-use fashion.
  el_A <- data.frame(From = c("A", "Oil"), To = c("Oil", "A"), Value = c(42, 42), Product = c("Oil", "Oil"), stringsAsFactors = FALSE)
  el_simple_A <- Recca:::simplify_edge_list(el_A)
  expect_equal(el_simple_A, data.frame(From = "A", To = "A", Value = 42, Product = "Oil", stringsAsFactors = FALSE))

  # Now try with a bigger ECC.
  sutmats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix)
  eldf_simplified <- edge_list(sutmats, simplify_edges = TRUE)
  el_final_simplified <- eldf_simplified$`Edge list`[[1]] %>%
    dplyr::mutate_if(is.factor, as.character)
  # Verify a few of the data points
  el_final_simplified %>%
    dplyr::filter(From == "Crude dist.", To == "Crude dist.", Product == "Crude [from Dist.]") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(500)
  el_final_simplified %>%
    dplyr::filter(From == "Elect. grid", To == "Residential", Product == "Elect [from Grid]") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(6000)
  el_final_simplified %>%
    dplyr::filter(From == "Oil refineries", To == "Oil refineries", Product == "Diesel") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(5000)
})


test_that("waste_edges() works as expected", {
  sutmats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix)
  eldf <- edge_list(sutmats)
  el_final <- eldf$`Edge list`[[1]] %>%
    dplyr::mutate_if(is.factor, as.character)
  # Verify that a few of the numbers are correct
  el_final %>%
    dplyr::filter(From == "Crude dist.", To == "Waste", Product == "Waste") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(550)
  el_final %>%
    dplyr::filter(From == "Power plants", To == "Waste", Product == "Waste") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(9700)
  el_final %>%
    dplyr::filter(From == "Gas wells & proc.", To == "Waste", Product == "Waste") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(2075)

  # Test waste_edges when only matrices are specified
  R_mat <- sutmats$R[[1]]
  U_mat <- sutmats$U[[1]]
  V_mat <- sutmats$V[[1]]
  Y_mat <- sutmats$Y[[1]]
  el_final <- edge_list(R = R_mat, U = U_mat, V = V_mat, Y = Y_mat)[["Edge list"]]
  el_final %>%
    dplyr::filter(From == "Crude dist.", To == "Crude dist.", Product == "Crude [from Dist.]") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(500)
  el_final %>%
    dplyr::filter(From == "Elect. grid", To == "Residential", Product == "Elect [from Grid]") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(6000)
  el_final %>%
    dplyr::filter(From == "Oil refineries", To == "Oil refineries", Product == "Diesel") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(5000)

  el_final %>%
    dplyr::filter(From == "Crude dist.", To == "Waste", Product == "Waste") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(550)
  el_final %>%
    dplyr::filter(From == "Power plants", To == "Waste", Product == "Waste") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(9700)
  el_final %>%
    dplyr::filter(From == "Gas wells & proc.", To == "Waste", Product == "Waste") %>%
    dplyr::select(Value) |>
    magrittr::extract2(1) |>
    expect_equal(2075)
})


test_that("add_edge_id() works as expected", {
  sutmats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix)
  elDF <- edge_list(sutmats)
  expect_equal(elDF$`Edge list`[[1]][["edge_id"]], seq(1, 39))
})


test_that("add_node_id_works as expected", {
  sutmats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix)
  elDF <- edge_list(sutmats)
  edge_lists <- elDF$`Edge list`
  for (el in edge_lists) {
    # Test every edge list to ensure that node IDs are matched with same node names everywhere
    # The testing strategy is to gather all name/ID pairs and count.
    # Gather from IDs and to IDs and bind them together.
    fromIDs <- el %>%
      dplyr::select(From, From_node_id) %>%
      dplyr::rename(Name = From, ID = From_node_id)
    toIDs <- el %>%
      dplyr::select(To, To_node_id) %>%
      dplyr::rename(Name = To, ID = To_node_id)
    fromtoIDs <- dplyr::bind_rows(fromIDs, toIDs)
    # Count the number of unique pairs.
    n_unique_pairs <- fromtoIDs %>%
      unique() %>%
      nrow()
    # Count the number of unique names.
    n_unique_names <- fromtoIDs %>%
      dplyr::select(Name) %>%
      unique() %>%
      nrow()
    # Count the number of uniqe ID numbers.
    n_unique_ids <- fromtoIDs %>%
      dplyr::select(ID) %>%
      unique() %>%
      nrow()
    # We expect that the number of unique pairs is the same as the number of unique names.
    # If not, one of the names is paired with two or more ID numbers.
    expect_equal(n_unique_pairs, n_unique_names)
    # We expect that the number of unique names is the same as the number of unique ID numbers.
    # If not, something is amiss.
    expect_equal(n_unique_names, n_unique_ids)
  }
})


test_that("node_list() works as expected", {
  # Test a single edge list
  sutmats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix)
  el <- edge_list(sutmats)$`Edge list`[[1]]
  nl <- node_list(edge_list = el)
  # Verify a few of the node numbers
  expect_equal(dplyr::filter(nl, Node == "Resources [of Crude]")$node_id, 0)
  expect_equal(dplyr::filter(nl, Node == "Diesel dist.")$node_id, 4)
  expect_equal(dplyr::filter(nl, Node == "NG dist.")$node_id, 8)
  expect_equal(dplyr::filter(nl, Node == "Transport")$node_id, 11)
  expect_equal(dplyr::filter(nl, Node == "Waste")$node_id, 13)
})


test_that("edge_list() works when R is missing", {
  sutmats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix) %>%
    dplyr::mutate(
      V = matsbyname::sum_byname(R, V),
      R = NULL
    )
  elDF <- edge_list(sutmats)
  expect_equal(elDF$`Edge list`[[1]][["edge_id"]], seq(1, 39))
})

