
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


test_that("make_sankey() works with height and width arguments", {
  # Mostly these are tests to show that the arguments can be passed through
  # to networkD3::sankeyNetwork().
  s <- UKEnergy2000mats |>
    tidyr::spread(key = "matrix.name", value = "matrix") |>
    make_sankey(height = 100, width = 100)
  expect_true(!any(is.null(s$Sankey)))
})


test_that("make_sankey() works with fontSize and fontFamily arguments", {
  # Mostly these are tests to show that the arguments can be passed through
  # to networkD3::sankeyNetwork().
  s <- UKEnergy2000mats |>
    tidyr::spread(key = "matrix.name", value = "matrix") |>
    make_sankey(fontSize = 30, fontFamily = "Helvetica")
  expect_true(!any(is.null(s$Sankey)))
})


test_that("make_sankey() works with colourScale argument", {
  # See https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
  # for information on how to control colours.
  # Make an initial colour list for the resources
  colour_df <- tibble::tribble(~name, ~colour,
                               "Resources [of Crude]", "red",
                               "Crude dist.",          "sienna3",
                               "Oil fields",           "indianred",
                               "Oil refineries",       "lightsalmon",
                               "Diesel dist.",         "darkturquoise",
                               "Power plants",         "green",
                               "Elect. grid",          "deeppink",
                               "Resources [of NG]",    "blue",
                               "NG dist.",             "orange",
                               "Gas wells & proc.",    "purple",
                               "Petrol dist.",         "firebrick1",
                               "Transport",            "dodgerblue",
                               "Residential",          "lavenderblush",
                               "Waste",                "yellow",
                               "Crude",                "gray",
                               "Crude [from Dist.]",   "gray",
                               "Crude [from Fields]",  "gray",
                               "Diesel",               "gray",
                               "Diesel [from Dist.]",  "red",
                               "Diesel [from Fields]", "gray",
                               "Elect",                "gray",
                               "Elect [from Grid]",    "gray",
                               "NG",                   "gray",
                               "NG [from Dist.]",      "gray",
                               "NG [from Wells]",      "gray",
                               "Petrol",               "gray",
                               "Petrol [from Dist.]",  "gray")
  name_string <- paste0(paste0('"', colour_df[["name"]], '"'), collapse = ", ")
  colour_string <- paste0(paste0('"', colour_df[["colour"]], '"'), collapse = ", ")
  # colours <- paste0('d3.scaleOrdinal() .domain([', name_string, ']) .range([', colour_string, '])')
  colours <- paste0('d3.scaleOrdinal([', name_string, '], [', colour_string, ']);')



  s <- UKEnergy2000mats |>
    tidyr::spread(key = "matrix.name", value = "matrix") |>
    make_sankey(colourScale = networkD3::JS(colours),
                fontSize = 10,
                fontFamily = "Helvetica")

  s$Sankey[[1]]
  expect_true(!any(is.null(s$Sankey)))
})



test_that("Simple sankey colours work", {

})
