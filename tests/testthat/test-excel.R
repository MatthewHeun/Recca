test_that("write_ecc_to_excel() works as expected", {
  ecc <- UKEnergy2000mats |>
    tidyr::spread(key = "matrix.name", value = "matrix") |>
    dplyr::mutate(
      worksheet_names = paste0(EnergyType, "-", LastStage)
    )
  ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")

  res <- write_ecc_to_excel(ecc,
                            path = ecc_temp_path,
                            worksheet_names = "worksheet_names",
                            overwrite = TRUE)

  expect_true(file.exists(ecc_temp_path))

  # Test that named matrix regions exist in the file
  regions <- openxlsx::getNamedRegions(ecc_temp_path)
  expect_equal(regions,
               c("R_E-Final", "U_E-Final", "V_E-Final",
                 "Y_E-Final", "r_eiou_E-Final",
                 "U_eiou_E-Final", "U_feed_E-Final",
                 "S_units_E-Final",
                 "R_E-Services", "U_E-Services", "V_E-Services",
                 "Y_E-Services", "r_eiou_E-Services",
                 "U_eiou_E-Services", "U_feed_E-Services",
                 "S_units_E-Services",
                 "R_E-Useful", "U_E-Useful", "V_E-Useful",
                 "Y_E-Useful", "r_eiou_E-Useful",
                 "U_eiou_E-Useful", "U_feed_E-Useful",
                 "S_units_E-Useful",
                 "R_X-Services", "U_X-Services", "V_X-Services",
                 "Y_X-Services", "r_eiou_X-Services",
                 "U_eiou_X-Services", "U_feed_X-Services",
                 "S_units_X-Services"),
               ignore_attr = TRUE)

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




test_that("write_ecc_to_excel() sets sheet names", {
  ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file",
                            fileext = ".xlsx")

  ecc <- UKEnergy2000mats |>
    tidyr::pivot_wider(names_from = "matrix.name",
                       values_from = "matrix")
  res_no_names <- write_ecc_to_excel(ecc,
                                     path = ecc_temp_path,
                                     overwrite = TRUE)
  expect_true(file.exists(ecc_temp_path))
  # Read the workbook
  openxlsx::loadWorkbook(file = ecc_temp_path) |>
    names() |>
    expect_equal(c("1", "2", "3", "4"))


  # Now try with tab names
  ecc_with_names <- ecc |>
    dplyr::mutate(
      worksheet_names = paste0(EnergyType, "-", LastStage)
    )
  res_with_names <- write_ecc_to_excel(ecc_with_names,
                                       worksheet_names = "worksheet_names",
                                       path = ecc_temp_path,
                                       overwrite = TRUE)
  expect_true(file.exists(ecc_temp_path))
  # Read the workbook
  openxlsx::loadWorkbook(file = ecc_temp_path) |>
    names() |>
    expect_equal(c("E-Final",
                   "E-Services",
                   "E-Useful",
                   "X-Services"))

  if (file.exists(ecc_temp_path)) {
    file.remove(ecc_temp_path)
  }
})


test_that("check_named_region_violations() works as expected", {
  # Is OK
  check_named_region_violations(c("test1", "test2")) |>
    expect_null()
  # Contains illegal character
  check_named_region_violations("\\") |>
    expect_warning()
  check_named_region_violations("a\\") |>
    expect_warning()
  # Three warnings here
  check_named_region_violations(c("a", "c12", " ", ",")) |>
    expect_warning() |>
    expect_warning() |>
    expect_warning()
  # Starts with illegal character
  check_named_region_violations(" ") |>
    expect_warning()
  # Resembles cell reference
  check_named_region_violations("B12") |>
    expect_warning()
  # Too long
  check_named_region_violations(strrep("x", 256)) |>
    expect_warning()
})






