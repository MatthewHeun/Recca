test_that("re-exoprting works correctly", {
  exists("nonenergy_products", where = "package:Recca", mode= "function")
  Recca::nonenergy_products
})
