# library(dplyr)
# library(matsindf)
# library(tidyr)

test_that("ERRs are calculated correctly", {
  result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    # dplyr::rename(R_plus_V = V) %>%
    # separate_RV() %>%
    calc_io_mats() %>%
    calc_ERRs_gamma() %>%
    dplyr::select(Country, Year, Energy.type, Last.stage, ger_gamma, ner_gamma, r_gamma) %>%
    tidyr::gather(key = matnames, value = matvals, ger_gamma, ner_gamma, r_gamma) %>%
    matsindf::expand_to_tidy()

  # Test some specific values
  expect_equal(result %>% dplyr::filter(Last.stage == IEATools::last_stages$final, matnames == "ger_gamma", rownames == "Crude dist.") %>%
                 magrittr::extract2("matvals"), 86.3636363636)
  expect_equal(result %>% dplyr::filter(Last.stage == IEATools::last_stages$final, matnames == "ger_gamma", rownames == "Diesel dist.") %>%
                 magrittr::extract2("matvals"), 44.2857142857)
  expect_true(result %>% dplyr::filter(Last.stage == IEATools::last_stages$final, matnames == "ger_gamma", rownames == "Elect. grid") %>%
                magrittr::extract2("matvals") %>% is.infinite())
  expect_equal(result %>% dplyr::filter(Last.stage == IEATools::last_stages$final, matnames == "ner_gamma", rownames == "NG dist.") %>%
                 magrittr::extract2("matvals"), 819)
  expect_equal(result %>% dplyr::filter(Last.stage == IEATools::last_stages$final, matnames == "ner_gamma", rownames == "Power plants") %>%
                 magrittr::extract2("matvals"), 63)
  expect_equal(result %>% dplyr::filter(Last.stage == IEATools::last_stages$final, matnames == "r_gamma", rownames == "Oil fields") %>%
                 magrittr::extract2("matvals"), 0.948500)
  expect_equal(result %>% dplyr::filter(Last.stage == IEATools::last_stages$final, matnames == "r_gamma", rownames == "Oil refineries") %>%
                 magrittr::extract2("matvals"), 0.8920212766)

  # These industries have inhomogeneous units. Check for NAs where appropriate.
  inf_na_industries <- c("Elect. grid", "Car engines", "Cars", "Furnaces", "Homes", "Light fixtures", "Rooms", "Truck engines", "Trucks")
  detailed_result <- result %>%
    dplyr::mutate(
      rowtypes = NULL,
      coltypes = NULL,
      matnames = NULL,
      expected = dplyr::case_when(
        Last.stage == IEATools::last_stages$services & rownames %in% c("Petrol dist.", "Crude dist.", "Diesel dist.", "NG dist.") ~ NA_real_,
        rownames %in% inf_na_industries & colnames == "r_gamma" ~ 0/0,
        rownames %in% inf_na_industries ~ 1/0,
        TRUE ~ matvals
      )
    )
  expect_equal(detailed_result$matvals, detailed_result$expected)
})

test_that("column names are correct in calc_ERRs_gamma", {
  result <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    calc_io_mats() %>%
    calc_ERRs_gamma()

  # Ensure that ger column is named correctly.
  for (i in 1:nrow(result)) {
    ger <- result$ger_gamma[[i]]
    expect_equal(colnames(ger)[1], "ger_gamma")
  }

  # Ensure that ner column is named correctly.
  for (i in 1:nrow(result)) {
    ner <- result$ner_gamma[[i]]
    expect_equal(colnames(ner)[1], "ner_gamma")
  }

  # Ensure that r column is named correctly.
  for (i in 1:nrow(result)) {
    r <- result$r_gamma[[i]]
    expect_equal(colnames(r)[1], "r_gamma")
  }
})
