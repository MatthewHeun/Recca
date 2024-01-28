
test_that("make_sankey() works as expected", {

  sutmats <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix")
  # First, try with some missing values.
  single <- sutmats[1, ]
  expect_false(is.na(make_sankey(R = single$R[[1]], U = single$U[[1]], V = single$V[[1]], Y = single$Y[[1]])))
  expect_true(is.na(make_sankey(R = single$R[[1]], U = NA_real_, V = single$V[[1]], Y = single$Y[[1]])))
  expect_true(is.na(make_sankey(R = single$R[[1]], U = single$U[[1]], V = NA_real_, Y = single$Y[[1]])))
  expect_true(is.na(make_sankey(R = single$R[[1]], U = single$U[[1]], V = single$V[[1]], Y = NA_real_)))
  expect_true(is.na(make_sankey(R = NA_real_, U = single$U[[1]], V = single$V[[1]], Y = single$Y[[1]])))

  # Now try to make some real Sankey diagrams
  for (i in 1:nrow(sutmats)) {
    s <- make_sankey(R = sutmats$R[[1]], U = sutmats$U[[i]], V = sutmats$V[[i]], Y = sutmats$Y[[i]])
    expect_false(is.null(s))
  }
  sankeys <-  sutmats %>%
    make_sankey()
  for (i in nrow(sankeys)) {
    expect_false(is.null(sankeys$Sankey[[i]]))
  }

  # Try without any R values.
  sutmats_noR <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix") %>%
    dplyr::mutate(
      V = matsbyname::sum_byname(R, V),
      R = NULL
    )

  # Check missing values again
  single_noR <- sutmats_noR[1, ]
  expect_false(is.na(make_sankey(U = single_noR$U[[1]], V = single_noR$V[[1]], Y = single_noR$Y[[1]])))
  expect_true(is.na(make_sankey(U = single_noR$U[[1]], V = NA_real_, Y = single_noR$Y[[1]])))

  for (i in 1:nrow(sutmats_noR)) {
    s <- make_sankey(U = sutmats_noR$U[[i]], V = sutmats_noR$V[[i]], Y = sutmats_noR$Y[[i]])
    expect_false(is.null(s))
  }
  sankeys_noR <-  sutmats_noR %>%
    make_sankey()
  for (i in nrow(sankeys_noR)) {
    expect_false(is.null(sankeys_noR$Sankey[[i]]))
  }

})
