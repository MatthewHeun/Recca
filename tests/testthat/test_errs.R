###########################################################
context("ERRs")
###########################################################

test_that("ERRs are calculated correctly", {
  result <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
    rename(R_plus_V = V) %>%
    separate_RV() %>%
    calc_io_mats() %>%
    calc_ERRs_gamma() %>%
    select(Country, Year, Energy.type, Last.stage, ger_gamma, ner_gamma, r_gamma) %>%
    gather(key = matnames, value = matvals, ger_gamma, ner_gamma, r_gamma) %>%
    expand_to_tidy()


  # Test some specific values
  expect_equal(result %>% filter(Last.stage == "final", matnames == "ger_gamma", rownames == "Crude dist.") %>% extract2("matvals"), 86.3636363636)
  expect_equal(result %>% filter(Last.stage == "final", matnames == "ger_gamma", rownames == "Diesel dist.") %>% extract2("matvals"), 44.2857142857)
  expect_true(result %>% filter(Last.stage == "final", matnames == "ger_gamma", rownames == "Elect. grid") %>% extract2("matvals") %>% is.infinite())
  expect_equal(result %>% filter(Last.stage == "final", matnames == "ner_gamma", rownames == "NG dist.") %>% extract2("matvals"), 819)
  expect_equal(result %>% filter(Last.stage == "final", matnames == "ner_gamma", rownames == "Power plants") %>% extract2("matvals"), 63)
  expect_equal(result %>% filter(Last.stage == "final", matnames == "r_gamma", rownames == "Oil fields") %>% extract2("matvals"), 0.948500)
  expect_equal(result %>% filter(Last.stage == "final", matnames == "r_gamma", rownames == "Oil refineries") %>% extract2("matvals"), 0.8920212766)

  # These industries have inhomogeneous units. Check for NAs where appropriate.

})

test_that("column names are correct in calc_ERRs_gamma", {
  result <- UKEnergy2000mats %>%
    spread(key = "matrix.name", value = "matrix") %>%
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
