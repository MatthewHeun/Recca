
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


test_that("a simple sankey works as expected", {
  # Examples from
  # https://r-graph-gallery.com/322-custom-colours-in-sankey-diagram.html
  links <- data.frame(
    source=c("group_A","group_A", "group_B", "group_C", "group_C", "group_E"),
    target=c("group_C","group_D", "group_E", "group_F", "group_G", "group_H"),
    value=c(2,3, 2, 3, 1, 3)
  )

  # From these flows we need to create a node data frame: it lists every entities involved in the flow
  nodes <- data.frame(
    name = c(as.character(links$source), as.character(links$target)) |>
      unique()
  )

  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1
  links$IDtarget <- match(links$target, nodes$name)-1

  # prepare color scale: I give one specific color for each node.
  my_color <- 'd3.scaleOrdinal() .domain(["group_A", "group_B","group_C", "group_D", "group_E", "group_F", "group_G", "group_H"]) .range(["blue", "green" , "chocolate", "orange", "red", "yellow", "aquamarine", "purple"])'

  # Make the Network. I call my colour scale with the colourScale argument
  p <- networkD3::sankeyNetwork(Links = links,
                                Nodes = nodes,
                                Source = "IDsource",
                                Target = "IDtarget",
                                Value = "value",
                                NodeID = "name",
                                colourScale = my_color)
  expect_true(!is.null(p))

  # Add color to the flows.
  links$group <- c("type_a","type_a","type_a","type_b","type_b","type_b")

  # Add a 'group' column to each node. Here I decide to put all of them in the same group to make them grey
  nodes$group <- c("node_group")

  # Give a color for each group:
  my_color2 <- 'd3.scaleOrdinal() .domain(["type_a", "type_b", "node_group"]) .range(["#69b3a2", "steelblue", "grey"])'
  # Make the Network
  p2 <- networkD3::sankeyNetwork(Links = links,
                                 Nodes = nodes,
                                 Source = "IDsource",
                                 Target = "IDtarget",
                                 Value = "value",
                                 NodeID = "name",
                                 colourScale = my_color2,
                                 LinkGroup="group",
                                 NodeGroup="group")

  my_color3 <- tibble::tribble(~name, ~colour,
                               "type_a", "#69b3a2",
                               "type_b", "steelblue",
                               "node_group", "grey") |>
    create_sankey_colour_string()

  p3 <- networkD3::sankeyNetwork(Links = links,
                                 Nodes = nodes,
                                 Source = "IDsource",
                                 Target = "IDtarget",
                                 Value = "value",
                                 NodeID = "name",
                                 colourScale = my_color3,
                                 LinkGroup="group",
                                 NodeGroup="group")

  expect_true(!is.null(p3))
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
  # name_string <- paste0(paste0('"', colour_df[["name"]], '"'), collapse = ", ")
  # colour_string <- paste0(paste0('"', colour_df[["colour"]], '"'), collapse = ", ")
  # # colours <- paste0('d3.scaleOrdinal() .domain([', name_string, ']) .range([', colour_string, '])')
  # colours <- paste0('d3.scaleOrdinal([', name_string, '], [', colour_string, ']);')

  colours <- create_sankey_colour_string(colour_df)

  s <- UKEnergy2000mats |>
    tidyr::spread(key = "matrix.name", value = "matrix") |>
    make_sankey(colour_string = colours,
                fontSize = 10,
                fontFamily = "Helvetica")

  s$Sankey[[1]]
  expect_true(!any(is.null(s$Sankey)))
})



test_that("Simple sankey colours work", {

})
