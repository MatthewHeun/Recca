
test_that("any_start_with() works properly", {
  expect_true(any_start_with(x = c("a", "b", "c"), target = "b"))
  expect_true(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Production"))
  expect_false(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Offshore"))
  expect_false(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = "Crude"))
  expect_equal(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = c("Production", "Offshore")),
               c(TRUE, FALSE))
  # Does it also work with lists?
  expect_equal(any_start_with(x = c("Production - Crude", "Production - NG", "Bogus"), target = list("Production", "Offshore")),
               c(TRUE, FALSE))
})


test_that("startsWith_any_of() works properly", {
  expect_true(startsWith_any_of(x = "prefix - suffix", prefixes = c("a", "b", "prefix")))
  expect_false(startsWith_any_of(x = "prefix - suffix", prefixes = c("a", "b", "c")))
  expect_false(startsWith_any_of(x = "prefix - suffix", prefixes = "suffix"))
  expect_equal(startsWith_any_of(x = c("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                 prefixes = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
  # Does it also work with lists?
  expect_equal(startsWith_any_of(x = list("Production - Crude", "Production - NG", "Exports - Oil", "Exports - Crude"),
                                  prefixes = c("Production", "Imports")),
               c(TRUE, TRUE, FALSE, FALSE))
})


test_that("resource_industries() works correctly", {
  mats <- UKEnergy2000mats %>%
    tidyr::spread(key = matrix.name, value = matrix)
  expected <- c("Resources [of Crude]", "Resources [of NG]")
  expect_equal(resource_industries(mats)[["r_industries"]],
               list(expected, expected, expected, expected))
  # Try with individual matrices
  for (i in 1:nrow(mats)) {
    expect_equal(resource_industries(R = mats$R[[i]], U = mats$U[[i]], V = mats$V[[i]]) %>% magrittr::set_names(NULL) %>% unlist(), expected)
  }
})


test_that("separate_RV() works correctly", {
  expected <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix")

  mats <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    dplyr::mutate(
      # Make an R+V matrix
      R_plus_V = matsbyname::sum_byname(R, V),
      # Delete the R and V matrices
      R = NULL,
      V = NULL
    ) %>%
    # Now separate R and V
    separate_RV()

  # Make sure that we get the expected values for R and V matrices
  for (i in 1:4) {
    expect_true(matsbyname::equal_byname(mats$R[[i]], expected$R[[i]]))
    expect_true(matsbyname::equal_byname(mats$V[[i]], expected$V[[i]]))
  }

  # If I try to separate R from V on the mats data frame, I should get nothing,
  # because R has already been separated from V.
  mats %>% separate_RV(R_plus_V = "V") |>
    expect_warning("No R created in separate_RV") |>
    expect_warning("No R created in separate_RV") |>
    expect_warning("No R created in separate_RV") |>
    expect_warning("No R created in separate_RV") |>
    expect_warning("Name collision in matsindf::matsindf_apply")
})


test_that("combine_RV works correctly", {
  mats <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    dplyr::mutate(
      R_plus_V = matsbyname::sum_byname(R, V),
      R = NULL,
      V = NULL
    ) %>%
    separate_RV() %>%
    dplyr::rename(
      R_plus_V_expected = R_plus_V
    ) %>%
    combine_RV()

  # Make sure that we get the expected values for R_plus_V matrices
  for (i in 1:4) {
    expect_true(matsbyname::equal_byname(mats$R_plus_V[[i]], mats$R_plus_V_expected[[i]]))
  }
})


test_that("products_unit_homogeneous() works correctly", {
  result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    products_unit_homogeneous() %>%
    magrittr::extract2("products_unit_homogeneous") %>%
    unlist()
  expect_true(all(result))

  # Now make an S_units matrix that should fail.
  su <- matrix(c(1, 1, 0, 1), byrow = TRUE, nrow = 2, ncol = 2, dimnames = list(c("p1", "p2"), c("m", "kg")))
  expect_false(products_unit_homogeneous(S_units = su)[[1]])

  # Test when details are requested
  detailed_result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    products_unit_homogeneous(keep_details = TRUE) %>%
    magrittr::extract2(".products_unit_homogeneous") %>%
    unlist()
  expect_true(all(detailed_result))

  # Test a failing situation when details are requested.
  su_detailed <- products_unit_homogeneous(S_units = su, keep_details = TRUE)
  # The first row has two units, the second row has one unit.
  expect_equal(su_detailed[[".products_unit_homogeneous"]][ , 1], c(p1 = FALSE, p2 = TRUE))
})


test_that("inputs_unit_homogeneous() works correctly", {
  result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    inputs_unit_homogeneous() %>%
    magrittr::extract2(".inputs_unit_homogeneous") %>%
    unlist()
  # The 2nd and 4th rows of UKEnergy2000mats have services inputs to industries, with different units, of course.
  # Thus, we expect to have FALSE when services are the LastStage.
  expected <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    dplyr::mutate(
      expected = dplyr::case_when(
        LastStage == IEATools::last_stages$services ~ FALSE,
        LastStage != IEATools::last_stages$services ~ TRUE,
        TRUE ~ NA
        )
    ) %>%
    magrittr::extract2("expected")
  # Perform the test.
  expect_equal(result, expected)

  # Now test when details are requested.
  # When LastStage is "services", we have mixed units on the inputs for *dist. industries,
  # because services (with funny units) are inputs to the industries.
  result_details <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    inputs_unit_homogeneous(keep_details = TRUE) %>%
    dplyr::select(Country, Year, EnergyType, LastStage, .inputs_unit_homogeneous) %>%
    tidyr::gather(key = "matnames", value = "matvals", .inputs_unit_homogeneous) %>%
    matsindf::expand_to_tidy() %>%
    dplyr::mutate(
      expected = dplyr::case_when(
        LastStage == IEATools::last_stages$final ~ TRUE,
        LastStage == IEATools::last_stages$useful ~ TRUE,
        endsWith(rownames, "dist.") ~ FALSE,
        !endsWith(rownames, "dist.") ~ TRUE,
        TRUE ~ NA
      )
    )
  expect_equal(result_details$matvals, result_details$expected)
})


test_that("output_unit_homogeneous() works correctly", {
  result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    outputs_unit_homogeneous() %>%
    magrittr::extract2(".outputs_unit_homogeneous") %>%
    unlist()
  # All outputs are unit-homogeneous in the UKEnergy2000mats data frame.
  expect_true(all(result))

  # Now make a version that we expect to fail
  V <- matrix(c(1, 1,
                1, 0), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("i1", "i2"), c("p1", "p2")))
  S_units <- matrix(c(1, 0,
                      0, 1), nrow = 2, ncol = 2, byrow = TRUE, dimnames = list(c("p1", "p2"), c("m", "kg")))
  result2 <- outputs_unit_homogeneous(V = V, S_units = S_units)
  expect_false(result2 %>% unlist())

  # Now test the failure when details are requested.
  result2_details <- outputs_unit_homogeneous(V = V, S_units = S_units, keep_details = TRUE)
  expect_equal(result2_details[[".outputs_unit_homogeneous"]][ ,1], c(i1 = FALSE, i2 = TRUE))
})


test_that("inputs_outputs_unit_homogeneous works as expected", {
  result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    inputs_outputs_unit_homogeneous() %>%
    magrittr::extract2(".inputs_outputs_unit_homogeneous") %>%
    unlist()
  # The 2nd and 4th rows of UKEnergy2000mats have services inputs to industries, with different units, of course.
  # Thus, we expect to have FALSE when services are the LastStage.
  expected <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    dplyr::mutate(
      expected = dplyr::case_when(
        LastStage == IEATools::last_stages$services ~ FALSE,
        LastStage != IEATools::last_stages$services ~ TRUE,
        TRUE ~ NA
      )
    ) %>%
    magrittr::extract2("expected")
  expect_equal(result, expected)

  # Test when details are requested.
  result2 <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    flows_unit_homogeneous(keep_details = TRUE) %>%
    dplyr::select(Country, Year, EnergyType, LastStage, .flows_unit_homogeneous) %>%
    tidyr::gather(key = "matnames", value = "matvals", .flows_unit_homogeneous) %>%
    matsindf::expand_to_tidy() %>%
    dplyr::mutate(
      expected = dplyr::case_when(
        LastStage == IEATools::last_stages$final ~ TRUE,
        LastStage == IEATools::last_stages$useful ~ TRUE,
        endsWith(rownames, "dist.") ~ FALSE,
        rownames %in% c("Cars", "Homes", "Rooms", "Trucks") ~ FALSE,
        TRUE ~ TRUE
      )
    )
  expect_equal(result2$matvals, result2$expected)
})


test_that("flows_unit_homogeneous() works as expected", {
  result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    flows_unit_homogeneous() %>%
    magrittr::extract2(".flows_unit_homogeneous") %>%
    unlist()
  # The 2nd and 4th rows of UKEnergy2000mats have services inputs to industries, with different units, of course.
  # Thus, we expect to have FALSE when services are the LastStage.
  expected <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    dplyr::mutate(
      expected = dplyr::case_when(
        LastStage == IEATools::last_stages$services ~ FALSE,
        LastStage != IEATools::last_stages$services ~ TRUE,
        TRUE ~ NA
      )
    ) %>%
    magrittr::extract2("expected")
  expect_equal(result, expected)

  # Test when details are requested.
  result2 <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    flows_unit_homogeneous(keep_details = TRUE) %>%
    dplyr::select(Country, Year, EnergyType, LastStage, .flows_unit_homogeneous) %>%
    tidyr::gather(key = "matnames", value = "matvals", .flows_unit_homogeneous) %>%
    matsindf::expand_to_tidy() %>%
    dplyr::mutate(
      expected = dplyr::case_when(
        LastStage == IEATools::last_stages$final ~ TRUE,
        LastStage == IEATools::last_stages$useful ~ TRUE,
        endsWith(rownames, "dist.") ~ FALSE,
        rownames %in% c("Cars", "Homes", "Rooms", "Trucks") ~ FALSE,
        TRUE ~ TRUE
      )
    )
  expect_equal(result2$matvals, result2$expected)
})


test_that("reverse() works as expected", {
  result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    reverse()
  for (i in 1:4) {
    R_rev_expected <- matsbyname::transpose_byname(result$Y[[i]])
    U_rev_expected <- matsbyname::transpose_byname(result$V[[i]])
    V_rev_expected <- matsbyname::transpose_byname(result$U[[i]])
    Y_rev_expected <- matsbyname::transpose_byname(result$R[[i]])
    expect_equal(result$R_rev[[i]], R_rev_expected)
    expect_equal(result$U_rev[[i]], U_rev_expected)
    expect_equal(result$V_rev[[i]], V_rev_expected)
    expect_equal(result$Y_rev[[i]], Y_rev_expected)
  }
})


test_that("unescape_html_codes() works as expected", {
  expect_equal(replace_html_codes("&amp;"), "&")
  expect_equal(replace_html_codes("&lt;"), "<")
  expect_equal(replace_html_codes("&gt;"), ">")

  expect_equal(replace_html_codes("&amp"), "&amp")
  expect_equal(replace_html_codes("&lt"), "&lt")
  expect_equal(replace_html_codes("&gt"), "&gt")

  expect_equal(replace_html_codes(c("a", "&amp;")), c("a", "&"))
  expect_equal(replace_html_codes(list("a", "&amp;", "&lt;", "&gt;", "bcd")),
               c("a", "&", "<", ">", "bcd"))
  # Make sure it works in the context of a data frame
  df <- data.frame(text = c("a", "&amp;", "&lt;", "&gt;", "bcd"))
  escaped <- df %>% dplyr::mutate(
    fixed = replace_html_codes(text)
  )
  expect_equal(escaped$fixed, c("a", "&", "<", ">", "bcd"))

  # Try to generate some errors
  expect_equal(replace_html_codes(42), "42") # this works, surprisingly
  expect_error(replace_html_codes(fortytwo), "object 'fortytwo' not found")
  expect_equal(replace_html_codes(list(c("&amp;", "&amp;"), c("&lt;", "&lt;"), c("&gt;", "&gt;"))),
               list(c("&", "&"), c("<", "<"), c(">", ">")))
})


test_that("find_p_industry_names() works as expected", {
  Rrows <- c("Resources [of Oil and gas extraction]", "Resources [of Coal mines]")
  R <- matrix(c(1, 0,
                0, 2), nrow = 2, byrow = TRUE,
             dimnames = list(Rrows,
                             c("Crude oil", "Brown coal")))
  Vrows <- c("Imports [of Crude oil]", "Stock changes [of Bituminous coal]")
  V <- matrix(c(3, 0,
                0, 4), nrow = 2, byrow = TRUE,
              dimnames = list(Vrows, c("Crude oil", "Bituminous coal")))
  Ycols <- c("Exports [of Electricity]", "International marine bunkers [of Gas/diesel oil]")
  Y <- matrix(c(5, 0,
                0, 6), nrow = 2, byrow = TRUE,
              dimnames = list(c("Electricity", "Gas/diesel oil"), Ycols))
  p_industry_prefixes <- c("Resources", "Imports", "Exports", "Stock changes", "International marine bunkers")
  # Need to make sure these are all lists, so that the different types
  # (p_industries are strings and R, V, and Y are matrices)
  # are handled correctly by the function.
  p_industries_full_names <- find_p_industry_names(p_industry_prefixes = list(p_industry_prefixes),
                                                   R = list(R), V = list(V), Y = list(Y))
  expect_type(p_industries_full_names, type = "list")
  expect_equal(p_industries_full_names[["p_industries_complete"]], c(Rrows, Vrows, Ycols))

  # Try with a null matrix.  Should still work.
  p_industries_full_names_RY <- find_p_industry_names(p_industry_prefixes = list(p_industry_prefixes),
                                                      R = list(R), V = NULL, Y = list(Y))
  expect_equal(p_industries_full_names_RY[[1]], c(Rrows, Ycols))

  # Try with a data frame.
  DF <- tibble::tibble(R = list(R,R), V = list(V,V), Y = list(Y,Y),
                       p_industry_prefixes = list(p_industry_prefixes, "Resources"),
                       expected = list(c(Rrows, Vrows, Ycols), Rrows))
  DF_augmented <- DF %>%
    find_p_industry_names()
  expect_equal(DF_augmented[[Recca::industry_cols$p_industries_complete]], DF_augmented$expected)
})


test_that("get_all_products_and_industries() works as intended", {
  ecc <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix")

  res <- ecc %>%
    get_all_products_and_industries()

  expect_setequal(res[[Recca::prod_ind_names_colnames$product_names]][[1]],
                  c("Crude", "NG", "Crude [from Dist.]", "Crude [from Fields]", "Diesel", "Diesel [from Dist.]", "Elect", "Elect [from Grid]",
                    "NG [from Dist.]", "NG [from Wells]", "Petrol", "Petrol [from Dist.]"))

  expect_setequal(res[[Recca::prod_ind_names_colnames$industry_names]][[1]],
                  c("Resources [of Crude]", "Resources [of NG]", "Crude dist.", "Diesel dist.", "Elect. grid", "Gas wells & proc.", "NG dist.",
                  "Oil fields", "Oil refineries", "Petrol dist.", "Power plants", "Residential", "Transport"))

  expect_setequal(res[[Recca::prod_ind_names_colnames$product_names]][[2]],
                  c("Crude", "NG", "Crude [from Dist.]", "Crude [from Fields]",
                    "Diesel", "Diesel [from Dist.]", "Elect", "Elect [from Grid]",
                    "Freight [tonne-km/year]", "Light", "LTH", "MD [from Car engines]",
                    "MD [from Truck engines]", "NG [from Dist.]", "NG [from Wells]", "Petrol",
                    "Petrol [from Dist.]", "Illumination [lumen-hrs/yr]", "Passenger [passenger-km/yr]", "Space heating [m3-K]"))

  expect_setequal(res[[Recca::prod_ind_names_colnames$industry_names]][[4]],
                  c("Resources [of Crude]", "Resources [of NG]", "Car engines", "Cars", "Crude dist.", "Diesel dist.", "Elect. grid",
                    "Furnaces", "Gas wells & proc.", "Homes", "Light fixtures", "NG dist.", "Oil fields", "Oil refineries",
                    "Petrol dist.", "Power plants", "Rooms", "Truck engines", "Trucks", "Residential", "Transport"))
})


test_that("get_all_products_and_industries() works with pieces", {
  ecc <- UKEnergy2000mats %>%
    tidyr::pivot_wider(names_from = "matrix.name", values_from = "matrix")
  res <- ecc %>%
    get_all_products_and_industries(piece = "noun", inf_notation = FALSE, notation = RCLabels::notations_list$bracket_notation)
  expect_equal(res$Product.names[[1]],
               c("Crude", "NG", "Diesel", "Elect", "Petrol"))
  expect_equal(res$Industry.names[[1]],
               c("Resources", "Crude dist.", "Diesel dist.", "Elect. grid", "Gas wells & proc.", "NG dist.", "Oil fields",
                 "Oil refineries", "Petrol dist.", "Power plants", "Transport", "Residential"))
})


test_that("add_row_col_types() works as expected", {
  mats <- list(R = matrix(1), U = matrix(2),
               V = matrix(3), Y = matrix(4),
               U_feed = matrix(5), U_EIOU = matrix(6),
               r_EIOU = matrix(7), S_units = matrix(8))
  res <- add_row_col_types(matvals = mats)

  # R
  expect_equal(matsbyname::rowtype(res[["R"]]), Recca::row_col_types$industry_type)
  expect_equal(matsbyname::coltype(res[["R"]]), Recca::row_col_types$product_type)

  # U
  expect_equal(matsbyname::rowtype(res[["U"]]), Recca::row_col_types$product_type)
  expect_equal(matsbyname::coltype(res[["U"]]), Recca::row_col_types$industry_type)

  # V
  expect_equal(matsbyname::rowtype(res[["V"]]), Recca::row_col_types$industry_type)
  expect_equal(matsbyname::coltype(res[["V"]]), Recca::row_col_types$product_type)

  # Y
  expect_equal(matsbyname::rowtype(res[["Y"]]), Recca::row_col_types$product_type)
  expect_equal(matsbyname::coltype(res[["Y"]]), Recca::row_col_types$industry_type)

  # U_feed
  expect_equal(matsbyname::rowtype(res[["U_feed"]]), Recca::row_col_types$product_type)
  expect_equal(matsbyname::coltype(res[["U_feed"]]), Recca::row_col_types$industry_type)

  # U_eiou
  expect_equal(matsbyname::rowtype(res[["U_EIOU"]]), Recca::row_col_types$product_type)
  expect_equal(matsbyname::coltype(res[["U_EIOU"]]), Recca::row_col_types$industry_type)

  # r_eiou
  expect_equal(matsbyname::rowtype(res[["r_EIOU"]]), Recca::row_col_types$product_type)
  expect_equal(matsbyname::coltype(res[["r_EIOU"]]), Recca::row_col_types$industry_type)

  # S_units
  expect_equal(matsbyname::rowtype(res[["S_units"]]), Recca::row_col_types$product_type)
  expect_equal(matsbyname::coltype(res[["S_units"]]), Recca::row_col_types$unit_type)

  # Check that it works in a data frame
  df <- tibble::tibble(matnames = c("R", "S_units"),
                       matvals = c(matrix(1, dimnames = list("row", "col")),
                                   matrix(2)))
  res2 <- df |>
    add_row_col_types(matnames = df$matname,
                      matvals = df$matval)
  expect_equal(matsbyname::rowtype(res2[[1]]), Recca::row_col_types$industry_type)
  expect_equal(matsbyname::coltype(res2[[1]]), Recca::row_col_types$product_type)
  expect_equal(matsbyname::rowtype(res2[[2]]), Recca::row_col_types$product_type)
  expect_equal(matsbyname::coltype(res2[[2]]), Recca::row_col_types$unit_type)

  res3 <- df |>
    dplyr::mutate(
      matvals2 = add_row_col_types(matvals = .data[["matval"]],
                                   matnames = .data[["matname"]])
    )
  expect_equal(matsbyname::rowtype(res3$matvals2[[1]]),
               Recca::row_col_types$industry_type)
  expect_equal(matsbyname::coltype(res3$matvals2[[1]]),
               Recca::row_col_types$product_type)
  expect_equal(matsbyname::rowtype(res3$matvals2[[2]]),
               Recca::row_col_types$product_type)
  expect_equal(matsbyname::coltype(res3$matvals2[[2]]),
               Recca::row_col_types$unit_type)

  # Try passing the data frame and column names.
  res4 <- df |>
    add_row_col_types()
  expect_equal(matsbyname::rowtype(res4$WithRCTypes[[1]]),
               Recca::row_col_types$industry_type)
  expect_equal(matsbyname::coltype(res3$matvals2[[1]]),
               Recca::row_col_types$product_type)
  expect_equal(matsbyname::rowtype(res3$matvals2[[2]]),
               Recca::row_col_types$product_type)
  expect_equal(matsbyname::coltype(res3$matvals2[[2]]),
               Recca::row_col_types$unit_type)

})











