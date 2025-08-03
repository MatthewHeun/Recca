test_that("write_ecc_to_excel() works as expected", {
  ecc <- UKEnergy2000mats |>
    tidyr::spread(key = "matrix.name", value = "matrix") |>
    dplyr::mutate(
      worksheet_names = paste0(EnergyType, "_", LastStage)
    )
  ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")

  res <- write_ecc_to_excel(ecc,
                            path = ecc_temp_path,
                            worksheet_names = "worksheet_names",
                            overwrite_file = TRUE)

  expect_true(file.exists(ecc_temp_path))

  # Test that named matrix regions exist in the file
  regions <- openxlsx::getNamedRegions(ecc_temp_path)
  expect_equal(regions,
               c("R__E_Final", "U__E_Final", "V__E_Final",
                 "Y__E_Final", "r_EIOU__E_Final",
                 "U_EIOU__E_Final", "U_feed__E_Final",
                 "S_units__E_Final",
                 "R__E_Services", "U__E_Services", "V__E_Services",
                 "Y__E_Services", "r_EIOU__E_Services",
                 "U_EIOU__E_Services", "U_feed__E_Services",
                 "S_units__E_Services",
                 "R__E_Useful", "U__E_Useful", "V__E_Useful",
                 "Y__E_Useful", "r_EIOU__E_Useful",
                 "U_EIOU__E_Useful", "U_feed__E_Useful",
                 "S_units__E_Useful",
                 "R__X_Services", "U__X_Services", "V__X_Services",
                 "Y__X_Services", "r_EIOU__X_Services",
                 "U_EIOU__X_Services", "U_feed__X_Services",
                 "S_units__X_Services"),
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
  expect_error(write_ecc_to_excel(ecc,
                                  path = ecc_temp_path,
                                  overwrite_file = FALSE))
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
  res <- write_ecc_to_excel(ecc,
                            path = ecc_temp_path,
                            overwrite_file = TRUE)

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
                                     overwrite_file = TRUE)
  expect_true(file.exists(ecc_temp_path))
  # Read the workbook
  openxlsx::loadWorkbook(file = ecc_temp_path) |>
    names() |>
    expect_equal(c("1", "2", "3", "4"))


  # Now try with tab names
  ecc_with_names <- ecc |>
    dplyr::mutate(
      worksheet_names = paste0(EnergyType, "_", LastStage)
    )
  res_with_names <- write_ecc_to_excel(ecc_with_names,
                                       worksheet_names = "worksheet_names",
                                       path = ecc_temp_path,
                                       overwrite_file = TRUE)
  expect_true(file.exists(ecc_temp_path))
  # Read the workbook
  openxlsx::loadWorkbook(file = ecc_temp_path) |>
    names() |>
    # We opened an existing workbook with tabs names 1, 2, 3, 4.
    # We are not overwriting the existing tabs,
    # we are adding new ones.
    expect_equal(c("1", "2", "3", "4",
                   "E_Final",
                   "E_Services",
                   "E_Useful",
                   "X_Services"))

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


test_that("check_worksheet_name_violations() works as expected", {
  # Is OK
  check_worksheet_name_violations(c("test1", "test2")) |>
    expect_null()
  # Illegal characters
  check_worksheet_name_violations(c("abc", "[")) |>
    expect_warning()
  # Empty name
  check_worksheet_name_violations(c("", "abc")) |>
    expect_warning()
  # Too long
  check_worksheet_name_violations(strrep("x", 32)) |>
    expect_warning()
  # Duplicates
  check_worksheet_name_violations(c("abc123", "abc123")) |>
    expect_warning()
})


test_that("read_ecc_from_excel() works as expected", {
  ecc <- UKEnergy2000mats |>
    tidyr::spread(key = "matrix.name", value = "matrix") |>
    dplyr::mutate(
      WorksheetNames = paste0(EnergyType, "_", LastStage)
    )
  ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")

  res <- write_ecc_to_excel(ecc,
                            path = ecc_temp_path,
                            worksheet_names = "WorksheetNames",
                            overwrite_file = TRUE)

  expect_true(file.exists(ecc_temp_path))

  # Now read the regions
  ecc_round_trip <- ecc_temp_path |>
    read_ecc_from_excel()

  all_mats <- dplyr::left_join(ecc, ecc_round_trip,
                               by = "WorksheetNames",
                               suffix = c(".orig", ".roundtrip"))

  for (i_row in 1:nrow(all_mats)) {
    for (j_matname in c("R", "U", "V", "Y",
                        "r_EIOU", "U_EIOU", "U_feed", "S_units")) {
      # print(paste("row =", i_row, "mat =", j_matname))
      j_orig_mat_name <- paste(j_matname, "orig", sep = ".")
      j_roundtrip_mat_name <- paste(j_matname, "roundtrip", sep = ".")
      orig_mat <- all_mats[i_row, j_orig_mat_name][[1]]
      roundtrip_mat <- all_mats[i_row, j_roundtrip_mat_name][[1]]
      should_be_zero <- matsbyname::difference_byname(roundtrip_mat, orig_mat)
      # This code can be uncommented to pinpoint any failures.
      # if (!matsbyname::iszero_byname(should_be_zero, tol = 1e-4)) {
      #   print(paste("Not zero for row =",
      #               i_row,
      #               "and matrix =",
      #               j_matname))
      # }
      expect_true(matsbyname::equal_byname(roundtrip_mat,
                                           orig_mat,
                                           tol = 1e-4))
    }
  }

  if (file.exists(ecc_temp_path)) {
    file.remove(ecc_temp_path)
  }
})


testthat::test_that("write_ecc_to_excel() works with pre-existing file", {
  ecc <- UKEnergy2000mats |>
    tidyr::spread(key = "matrix.name", value = "matrix") |>
    dplyr::mutate(
      WorksheetNames = paste0(EnergyType, "_", LastStage)
    )
  ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")

  res <- write_ecc_to_excel(ecc,
                            path = ecc_temp_path,
                            worksheet_names = "WorksheetNames",
                            overwrite_file = TRUE)

  expect_true(file.exists(ecc_temp_path))
  # Check worksheet names
  openxlsx2::wb_load(ecc_temp_path) |>
    openxlsx2::wb_get_sheet_names() |>
    expect_equal(c(E_Final = "E_Final", E_Services = "E_Services",
                   E_Useful = "E_Useful", X_Services = "X_Services"))

  # Now add a couple more rows
  ecc2 <- ecc |>
    dplyr::bind_rows(ecc |>
                       dplyr::mutate(
                         EnergyType = c("B", "B", "B", "BX"),
                         WorksheetNames = paste0(EnergyType, "_", LastStage)
                       ))
  # And write to the same file without enabling overwriting of sheets.
  # This should fail.
  write_ecc_to_excel(ecc2,
                     path = ecc_temp_path,
                     worksheet_names = "WorksheetNames",
                     overwrite_file = TRUE) |>
    expect_error("A worksheet by the name 'E_Final' already exists!")

  # Now try after enabling overwriting of sheets
  write_ecc_to_excel(ecc2,
                     path = ecc_temp_path,
                     worksheet_names = "WorksheetNames",
                     overwrite_file = TRUE,
                     overwrite_worksheets = TRUE)

  # Check worksheet names
  openxlsx2::wb_load(ecc_temp_path) |>
    openxlsx2::wb_get_sheet_names() |>
    expect_equal(c(E_Final = "E_Final", E_Services = "E_Services",
                   E_Useful = "E_Useful", X_Services = "X_Services",
                   B_Final = "B_Final", B_Services = "B_Services",
                   B_Useful = "B_Useful", BX_Services = "BX_Services"))

  # Clean up after ourselves
  if (file.exists(ecc_temp_path)) {
    file.remove(ecc_temp_path)
  }
})
