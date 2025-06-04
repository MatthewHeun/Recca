test_that("write_ecc_to_excel() works as expected", {
  ecc <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix")
  ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")

  res <- write_ecc_to_excel(ecc, path = ecc_temp_path, overwrite = TRUE)

  expect_true(file.exists(ecc_temp_path))
  if (file.exists(ecc_temp_path)) {
    file.remove(ecc_temp_path)
  }
})


test_that("write_ecc_to_excel() fails when the file already exists", {
  ecc <- UKEnergy2000mats %>%
    tidyr::spread(key = "matrix.name", value = "matrix")
  ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")
  file.create(ecc_temp_path)
  expect_error(write_ecc_to_excel(ecc, path = ecc_temp_path, overwrite = FALSE))
  # Formerly tested against a specific error message, but
  # regex matching is seemingly different and problematic on various platforms,
  # especially windows, where backslash ("\") is the file separator
  # but also the regex escape character.
  # So eliminate that feature of the test.
  # regexp = paste0("File ", ecc_temp_path, " already exists. Call write_ecc_to_excel with overwrite = TRUE to overwrite."))
  if (file.exists(ecc_temp_path)) {
    file.remove(ecc_temp_path)
  }
})


test_that("write_ecc_to_excel() works with Matrix objects", {
  ecc <- UKEnergy2000mats |>
    tidyr::spread(key = "matrix.name", value = "matrix") |>
    dplyr::mutate(
      R = matsbyname::Matrix(R)
    )
  ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")
  res <- write_ecc_to_excel(ecc, path = ecc_temp_path, overwrite = TRUE)

  expect_true(file.exists(ecc_temp_path))
  if (file.exists(ecc_temp_path)) {
    file.remove(ecc_temp_path)
  }
})


test_that("calc_mats_locations_excel() fails correctly", {
  R <- matrix(1, nrow = 5, ncol = 1)
  U <- matrix(1, nrow = 4, ncol = 2)
  V <- matrix(1, nrow = 3, ncol = 3)
  Y <- matrix(1, nrow = 2, ncol = 4)
  S_units = matrix(1, nrow = 1, ncol = 1)
  expect_error(Recca:::calc_mats_locations_excel(R = R, U = U, V = V, Y = Y, S_units = S_units),
               regexp = "R and V should have same number of columns in calc_mats_locations_excel")
  # Correct the problem with U and Y.
  # Now should have a problem with R and V.
  V2 = matrix(1, nrow = 4, ncol = 1)
  expect_error(Recca:::calc_mats_locations_excel(R = R, U = U, V = V2, Y = Y, S_units = S_units),
               regexp = "U and Y should have same number of rows in calc_mats_locations_excel")
})


