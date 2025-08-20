#' Write energy conversion chain matrices to an Excel file
#'
#' It is often helpful to see energy conversion chain (ECC) matrices in Excel format,
#' arranged spatially.
#' This function takes ECC matrices and writes them to an Excel file.
#'
#' If `.psut_data` is a PSUT data frame,
#' each row is written to a different tab in the output file at `path`.
#'
#' When `worksheet_names` is not `NULL` (the default),
#' be sure that worksheet names are unique.
#' Also, be aware that worksheet names
#' must have 31 characters or fewer.
#' Furthermore, the worksheet names
#' may not contain any of the following characters:
#' `\  /  ?  *  [  ]`.
#'
#' When `include_named_regions` is `TRUE` (the default),
#' named regions for matrices are added to Excel sheets.
#' The format for the region names is
#' `<<matrix symbol>><<sep>><<worksheet name>>`.
#' For example, "R_4" is the name for the region of the
#' **R** matrix on the sheet named "4".
#' The names help to identify matrices in high-level views of an Excel sheet
#' and can also be used for later reading matrices from Excel files.
#' (See [read_ecc_from_excel()].)
#' The region names apply to the rectangle of numbers _and_
#' the row and column names for the matrices.
#'
#' Note that region names are more restricted than worksheet names and
#' may not contain any of the following characters:
#' `! @ # $ % ^ & * ( ) + - / = { } [ ] | \ : ; " ' < > , . ? spaces`.
#' Best to stick with letters, numbers, and underscores.
#'
#' Finally, note that because region names include the worksheet name,
#' worksheet names should avoid illegal characters for region names.
#' Again, best to stick with letters, numbers, and underscores.
#'
#' A warning is given when any worksheet names or region names
#' contain illegal characters.
#'
#' When `path` already exists,
#' the worksheets are added to the file when
#' `overwrite_file` is `TRUE`.
#' The file at `path` may have pre-existing worksheets
#' with the same names as worksheets to be written.
#' `overwrite_worksheets` controls whether the pre-existing
#' worksheets will be deleted before writing the new worksheets.
#'
#' This function is an inverse of [read_ecc_from_excel()].
#'
#' @param .psut_data A list or data frame of energy conversion chains.
#'                   Default is `NULL`, in which case
#'                   single matrices can be supplied in the
#'                   `R`, `U`, `V`, `Y`, `r_eiou`, `U_eiou`, `U_feed`, and `S_units`
#'                   arguments.
#' @param path The path of the Excel file to be created.
#' @param overwrite_file A boolean that tells whether you want to overwrite
#'                       the file at `path`, if it already exists.
#'                       Default is `FALSE`.
#' @param worksheet_names A string or string vector identifying
#'                        the names for the worksheets in the workbook.
#'                        Alternatively, when `.psut_data` is a data frame,
#'                        the string name of a column in the data frame
#'                        containing the names of the worksheets.
#'                        When `NULL`, the default, tabs are
#'                        numbered sequentially.
#' @param overwrite_worksheets A boolean that tells whether to overwrite
#'                             existing worksheets of the same name
#'                             when `path` already exists.
#' @param pad The number of rows and columns between adjacent matrices in the Excel sheet.
#'            Default is `2`.
#' @param include_named_regions A boolean that tells whether to name regions of
#'                              the Excel tabs according to matrices.
#'                              Default is `TRUE`.
#' @param R,U,U_feed,U_eiou,r_eiou,V,Y,S_units Names of ECC matrices or actual matrices.
#'                                             See `Recca::psut_cols` for defaults.
#' @param sep The separator between matrix name and worksheet name
#'            for named regions.
#'            Default is "__".
#' @param .wrote_mats_colname The name of the outgoing column
#'                            that tells whether a worksheet was written successfully.
#'                            Default is "Wrote mats".
#' @param UV_bg_color The color of cells containing U and V matrices.
#'                    Default is a creamy yellow.
#' @param RY_bg_color The color of cells containing R and Y matrices.
#'                    Default is a rust color.
#' @param calculated_bg_color The color of cells containing calculated matrices.
#'                            Default is gray.
#' @param col_widths The widths of columns of matrices.
#'                   Default is `7` to save space.
#'
#' @return An unmodified version of `.psut_data` (if not `NULL`) or a list of
#'         the incoming matrices.
#'
#' @export
#'
#' @seealso [read_ecc_from_excel()]
#'
#' @examples
#' \dontrun{
#' ecc <- UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = "matrix.name",
#'                      values_from = "matrix") |>
#' dplyr::mutate(
#'   # Specify worksheet names using metadata guaranteed to be unique.
#'   worksheet_names = paste(EnergyType, LastStage, sep = "_")
#' )
#' ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")
#' write_ecc_to_excel(ecc,
#'                    path = ecc_temp_path,
#'                    worksheet_names = "worksheet_names",
#'                    overwrite = TRUE)
#' }
write_ecc_to_excel <- function(.psut_data = NULL,
                               path,
                               overwrite_file = FALSE,
                               worksheet_names = NULL,
                               overwrite_worksheets = FALSE,
                               pad = 2,
                               include_named_regions = TRUE,
                               R = Recca::psut_cols$R,
                               U = Recca::psut_cols$U,
                               V = Recca::psut_cols$V,
                               Y = Recca::psut_cols$Y,
                               r_eiou = Recca::psut_cols$r_eiou,
                               U_eiou = Recca::psut_cols$U_eiou,
                               U_feed = Recca::psut_cols$U_feed,
                               S_units = Recca::psut_cols$S_units,
                               sep = "__",
                               .wrote_mats_colname = "Wrote mats",
                               UV_bg_color = "#FDF2D0",
                               RY_bg_color = "#D3712D",
                               calculated_bg_color = "#D9D9D9",
                               col_widths = 7) {

  # Check if path exists. Throw an error if overwrite_file is FALSE.
  if (file.exists(path) & !overwrite_file) {
    stop(paste("File", path,
               "already exists. Call `Recca::write_ecc_to_excel()` with `overwrite = TRUE`?"))
  }
  if (file.exists(path)) {
    # The file already exists, and
    # the caller is OK with overwriting it
    # ecc_wb <- openxlsx::loadWorkbook(file = path)
    ecc_wb <- openxlsx2::wb_load(file = path)
  } else {
    # Create the workbook from scratch
    # ecc_wb <- openxlsx::createWorkbook()
    ecc_wb <- openxlsx2::wb_workbook()
  }

  create_one_tab <- function(R_mat, U_mat, V_mat, Y_mat,
                             U_eiou_mat, U_feed_mat, r_eiou_mat,
                             S_units_mat, worksheet_name) {

    # Figure out the worksheet name
    if (!is.null(worksheet_name)) {
      sheet_name <- worksheet_name
    } else {
      # Get existing sheet names
      # existing_sheets <- openxlsx::sheets(ecc_wb)
      existing_sheets <- openxlsx2::wb_get_sheet_names(ecc_wb)

      # Create a new name by incrementing the integer
      if (length(existing_sheets) == 0) {
        sheet_name <- "1"
      } else {
        sheet_name <- (as.integer(existing_sheets) |> max()) + 1
      }
    }
    # Check for malformed sheet names. Emit a warning if problem found.
    check_worksheet_name_violations(sheet_name)

    # If the file already exists,
    # check if the sheet already exists.
    # If overwrite_file is TRUE and the sheet exists,
    # remove it before writing a new sheet.
    # If overwrite_file is FALSE and the sheet exists,
    # allow it to error.
    if (file.exists(path) & overwrite_worksheets) {
      # Check for the existence of a sheet with the same name and delete it
      # existing_sheet_names <- names(ecc_wb)
      existing_sheet_names <- openxlsx2::wb_get_sheet_names(ecc_wb)
      if (!is.null(existing_sheet_names)) {
        if (sheet_name %in% existing_sheet_names) {
          # Delete the existing sheet before writing the new sheet
          # openxlsx::removeWorksheet(ecc_wb, sheet = sheet_name)
          # ecc_wb <- openxlsx2::wb_remove_worksheet(ecc_wb, sheet = sheet_name)
          ecc_wb$remove_worksheet(sheet = sheet_name)
        }
      }
    }

    # Add the new worksheet to the workbook
    # openxlsx::addWorksheet(ecc_wb, sheet_name)
    # ecc_wb <- ecc_wb |>
    #   openxlsx2::wb_add_worksheet(sheet_name)
    ecc_wb$add_worksheet(sheet_name)

    # Complete matrices relative to one another to make sure we have same number
    # of rows or columns, as appropriate
    # Ensure same columns of U and rows of V
    U_mat_T <- matsbyname::transpose_byname(U_mat)
    completedUV <- matsbyname::complete_and_sort(U_mat_T, V_mat, margin = c(1,2))
    U_mat <- matsbyname::transpose_byname(completedUV[[1]])
    V_mat <- completedUV[[2]]
    # Ensure same columns for R and V
    completedRV <- matsbyname::complete_and_sort(R_mat, V_mat, margin = 2)
    R_mat <- completedRV[[1]]
    V_mat <- completedRV[[2]]
    # Ensure same rows for U and Y
    completedUY <- matsbyname::complete_and_sort(U_mat, Y_mat, margin = 1)
    U_mat <- completedUY[[1]]
    Y_mat <- completedUY[[2]]
    # Ensure same rows and cols for U_EIOU and U
    completedU_eiou <- matsbyname::complete_and_sort(U_eiou_mat, U_mat, margin = c(1, 2))
    U_eiou_mat <- completedU_eiou[[1]]
    # Ensure same rows and cols for U_feed and U
    completedU_feed <- matsbyname::complete_and_sort(U_feed_mat, U_mat, margin = c(1, 2))
    U_feed_mat <- completedU_feed[[1]]
    # Ensure same rows and cols for r_EIOU and U
    completedr_eiou <- matsbyname::complete_and_sort(r_eiou_mat, U_mat, margin = c(1, 2))
    r_eiou_mat <- completedr_eiou[[1]]
    # Ensure same rows for S_units and U
    completedS_units <- matsbyname::complete_and_sort(S_units_mat, U_mat, margin = 1)
    S_units_mat <- completedS_units[[1]]

    # Calculate starting locations for each matrix.
    locations <- calc_mats_locations_excel(R = R_mat,
                                           U = U_mat,
                                           V = V_mat,
                                           Y = Y_mat,
                                           r_eiou = r_eiou_mat,
                                           U_eiou = U_eiou_mat,
                                           U_feed = U_feed_mat,
                                           S_units = S_units_mat,
                                           pad = pad)
    # Write each matrix to the worksheet
    Map(list(Recca::psut_cols$R,
             Recca::psut_cols$U,
             Recca::psut_cols$V,
             Recca::psut_cols$Y,
             Recca::psut_cols$r_eiou,
             Recca::psut_cols$U_eiou,
             Recca::psut_cols$U_feed,
             Recca::psut_cols$S_units),
        list(R_mat, U_mat, V_mat, Y_mat,
             r_eiou_mat, U_eiou_mat, U_feed_mat, S_units_mat),
        locations,
        f = function(this_mat_name, this_mat, this_loc) {
          # Find the locations of the matrix origin and matrix extent
          # from this_loc.
          # We'll use this in many places below.
          mat_origin <- this_loc[["origin"]] + c(x = 1, y = 1)  # Offset for the row and column names
          mat_extent <- this_loc[["extent"]] + c(x = 0, y = -1) # Offset for the matrix label
          # Write the data
          # openxlsx::writeData(wb = ecc_wb,
          #                     sheet = sheet_name,
          #                     # Account for the fact that this_mat could be a
          #                     # non-native matrix class (such as Matrix)
          #                     x = as.matrix(this_mat),
          #                     xy = this_loc[["origin"]],
          #                     array = TRUE, colNames = TRUE, rowNames = TRUE)
          # Gives the region for the numbers AND the row and column labels
          mat_region_dims <- openxlsx2::wb_dims(
            rows = (mat_origin[["y"]]-1):mat_extent[["y"]],
            cols = (mat_origin[["x"]]-1):mat_extent[["x"]])
          # ecc_wb <- ecc_wb |>
          #   openxlsx2::wb_add_data(sheet = sheet_name,
          #                          # Account for the fact that this_mat could be a
          #                          # non-native matrix class (such as Matrix)
          #                          x = as.matrix(this_mat),
          #                          dims = mat_region_dims,
          #                          array = TRUE,
          #                          col_names = TRUE,
          #                          row_names = TRUE)
          ecc_wb$add_data(sheet = sheet_name,
                          # Account for the fact that this_mat could be a
                          # non-native matrix class (such as Matrix)
                          x = as.matrix(this_mat),
                          dims = mat_region_dims,
                          array = TRUE,
                          col_names = TRUE,
                          row_names = TRUE)
          if (include_named_regions) {
            # Set the name of the region for this matrix.
            # Note that the name of a region can be at most 255 characters long.
            # Excel sheet names can be at most 31 characters long.
            # I'm creating names from the matrix name (max 7 characters)
            # plus the sheet name (max 31 characters)
            # plus the "_" character, for a maximum of 39 characters,
            # well below the maximum name size (255 characters).
            # So this approach should work fine.
            mat_region_name <- paste(this_mat_name, sheet_name, sep = sep)
            # Check for malformed region names. Emit a warning if problem found.
            check_named_region_violations(mat_region_name)
            # openxlsx::createNamedRegion(wb = ecc_wb,
            #                             sheet = sheet_name,
            #                             rows = (mat_origin[["y"]]-1):mat_extent[["y"]],
            #                             cols = (mat_origin[["x"]]-1):mat_extent[["x"]],
            #                             name = region_name,
            #                             # Set false to flag any problems.
            #                             overwrite = FALSE)
            ecc_wb <- ecc_wb |>
              openxlsx2::wb_add_named_region(sheet = sheet_name,
                                             dims = mat_region_dims,
                                             name = mat_region_name,
                                             # local_sheet = TRUE,
                                             # Set false to flag any problems.
                                             overwrite = FALSE)
          }
          # Set the background color to matrix_bg_color for the numbers in the matrix
          # Define the matrix numbers style
          if (this_mat_name %in% c("R", "Y")) {
            this_bg_color <- RY_bg_color
          } else if (this_mat_name %in% c("U", "V")) {
            this_bg_color <- UV_bg_color
          } else {
            this_bg_color <- calculated_bg_color
          }




          ##########################
          # Got to here with changing openxlsx --> opensxls2
          ##########################




          # # Create the style for this matrix.
          # mat_num_style <- openxlsx::createStyle(fgFill = this_bg_color,
          #                                        halign = "center",
          #                                        valign = "center")
          # openxlsx::addStyle(wb = ecc_wb,
          #                    sheet = sheet_name,
          #                    style = mat_num_style,
          #                    rows = mat_origin[["y"]]:mat_extent[["y"]],
          #                    cols = mat_origin[["x"]]:mat_extent[["x"]],
          #                    gridExpand = TRUE,
          #                    stack = TRUE)
          # # Rotate and center column labels
          # col_label_style <- openxlsx::createStyle(textRotation = 90,
          #                                          halign = "center",
          #                                          valign = "bottom")
          # openxlsx::addStyle(wb = ecc_wb,
          #                    sheet = sheet_name,
          #                    style = col_label_style,
          #                    rows = this_loc[["origin"]][["y"]],
          #                    cols = this_loc[["origin"]][["x"]]:this_loc[["extent"]][["x"]],
          #                    gridExpand = TRUE,
          #                    stack = TRUE)
          # # Right align row labels
          # row_label_style <- openxlsx::createStyle(halign = "right",
          #                                          valign = "center")
          # openxlsx::addStyle(wb = ecc_wb,
          #                    sheet = sheet_name,
          #                    style = row_label_style,
          #                    rows = this_loc[["origin"]][["y"]]:this_loc[["extent"]][["y"]],
          #                    cols = this_loc[["origin"]][["x"]],
          #                    gridExpand = TRUE,
          #                    stack = TRUE)
          # # Add matrix label
          # openxlsx::writeData(wb = ecc_wb,
          #                     sheet = sheet_name,
          #                     x = this_mat_name,
          #                     startRow = this_loc[["extent"]][["y"]],
          #                     startCol = mat_origin[["x"]])
          # # Format matrix label
          # mat_name_style <- openxlsx::createStyle(halign = "center",
          #                                         textDecoration = "Bold")
          # openxlsx::addStyle(wb = ecc_wb,
          #                    sheet = sheet_name,
          #                    style = mat_name_style,
          #                    rows = this_loc[["extent"]][["y"]],
          #                    cols = mat_origin[["x"]],
          #                    gridExpand = TRUE,
          #                    stack = TRUE)
          # openxlsx::mergeCells(wb = ecc_wb,
          #                      sheet = sheet_name,
          #                      rows = this_loc[["extent"]][["y"]],
          #                      cols = mat_origin[["x"]]:mat_extent[["x"]])
          # # Set column widths to "auto" to save space.
          # openxlsx::setColWidths(wb = ecc_wb,
          #                        sheet = sheet_name,
          #                        cols = mat_origin[["x"]]:mat_extent[["x"]],
          #                        widths = col_widths,
          #                        ignoreMergedCells = TRUE)
        })
    list(TRUE) %>%
      magrittr::set_names(.wrote_mats_colname)
  }

  out <- matsindf::matsindf_apply(.psut_data,
                                  FUN = create_one_tab,
                                  R_mat = R,
                                  U_mat = U,
                                  V_mat = V,
                                  Y_mat = Y,
                                  r_eiou_mat = r_eiou,
                                  U_eiou_mat = U_eiou,
                                  U_feed_mat = U_feed,
                                  S_units_mat = S_units,
                                  worksheet_name = worksheet_names)
  # Make sure the directory exists
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  # Write the workbook
  # openxlsx::saveWorkbook(ecc_wb, file = path, overwrite = overwrite_file)

  # This version of ecc_wb is still the original version.
  # Do I need to change to chaining ($) rather than pipe (|>)
  # when making changes to the workbook in the code above?
  ecc_wb |>
    openxlsx2::wb_save(file = path, overwrite = overwrite_file)
}


#' Calculate the origin and extent for each matrix
#'
#' The origin is defined as the upper-left corner of the matrix on the worksheet.
#' The extent is defined as the lower-right corner of the matrix on the worksheet.
#'
#' The outer structure of the return value is matrices,
#' in the order provided in the argument list.
#' The inner structure of the return value is a list of "origin" and "extent,"
#' in that order.
#'
#' This is a helper function, so it is not public.
#'
#' @param R,U,V,Y,r_eiou,U_eiou,U_feed,S_units Matrices to be arranged on an Excel worksheet.
#' @param pad The number of blank rows or columns between matrices.
#'
#' @return A nested list of origins and extents.
calc_mats_locations_excel <- function(R, U, V, Y, r_eiou, U_eiou, U_feed, S_units, pad = 2) {
  # At this point, each argument should be a single matrix.
  # Calculate horizontal sizes for matrices.
  # Each has a +1 due to the column of rownames
  hsizeS_units <- ncol(S_units) + 1
  if (ncol(R) != ncol(V)) {
    stop("R and V should have same number of columns in calc_mats_locations_excel().")
  }
  hsizeVR <- ncol(R) + 1
  hsizeU <- ncol(U) + 1
  hsizeY <- ncol(Y) + 1

  # Calculate vertical sizes for matrices.
  # Each as a +2 due to the row of column names and the label beneath the matrix.
  vsizeS_units <- nrow(S_units) + 2
  if (nrow(U) != nrow(Y)) {
    stop("U and Y should have same number of rows in calc_mats_locations_excel().")
  }
  vsizeUY <- nrow(U) + 2
  vsizeR <- nrow(R) + 2
  vsizeV <- nrow(V) + 2

  # Calculate origin and extent locations for each matrix.
  # The origin is the top left cell of the matrix, including all labels.
  # The extent is the bottom right cell of the matrix, including all labels.
  # x and y are
  # row number (with 1 at the top of the worksheet) and
  # column number (with 1 at the left of the worksheet),
  # respectively.
  originS_units <- c(x = 1, y = 1)
  extentS_units <- originS_units + c(x = hsizeS_units - 1, y = vsizeS_units - 1)

  left_side_U <- hsizeVR + pad + 1

  originU_eiou <- c(x = left_side_U, y = 1)
  extentU_eiou <- originU_eiou + c(x = hsizeU - 1, y = vsizeUY - 1)

  left_side_Y <- extentU_eiou[["x"]] + pad + 1

  originr_eiou <- c(x = left_side_Y, y = 1)
  extentr_eiou <- originr_eiou + c(x = hsizeU - 1, y = vsizeUY - 1)

  originU_feed <- c(x = left_side_U, y = extentU_eiou[["y"]] + pad + 1)
  extentU_feed <- originU_feed + c(x = hsizeU - 1, y = vsizeUY - 1)

  top_row_UY <- extentU_feed[["y"]] + pad + 1

  originU <- c(x = left_side_U, y = top_row_UY)
  extentU <- originU + c(x = hsizeU - 1, y = vsizeUY - 1)

  originY <- c(x = left_side_Y, y = top_row_UY)
  extentY <- originY + c(x = hsizeY - 1, y = vsizeUY - 1)

  originV <- c(x = 1, y = extentU[["y"]] + pad + 1)
  extentV <- originV + c(x = hsizeVR - 1, y = vsizeV - 1)

  originR <- c(x = 1, y = extentV[["y"]] + pad + 1)
  extentR <- originR + c(x = hsizeVR - 1, y = vsizeR - 1)

  list(R = list(origin = originR, extent = extentR),
       U = list(origin = originU, extent = extentU),
       V = list(origin = originV, extent = extentV),
       Y = list(origin = originY, extent = extentY),
       r_eiou = list(origin = originr_eiou, extent = extentr_eiou),
       U_eiou = list(origin = originU_eiou, extent = extentU_eiou),
       U_feed = list(origin = originU_feed, extent = extentU_feed),
       S_units = list(origin = originS_units, extent = extentS_units))
}


#' Develop a warning message for malformed Excel region names
#'
#' Well-formed Excel region names are important for named regions
#' in the Excel workbooks created by `write_ecc_to_excel()`.
#' This function warns if the region names contain illegal characters,
#' start with an illegal character,
#' resemble a cell reference, or
#' are too long.
#'
#' @param candidate_region_names A character vector of candidate region names.
#'
#' @returns `NULL` invisibly and a warning if any problems are detected.
#'
#' @export
#'
#' @examples
#' # No warning
#' check_named_region_violations(c("test1", "test2"))
#' \dontrun{
#'   # Warnings
#'   # Illegal character
#'   check_named_region_violations("\\")
#'   check_named_region_violations("a\\")
#'   # Starts with illegal character
#'   check_named_region_violations(" ")
#'   # Resembles cell reference
#'   check_named_region_violations("B12")
#'   # Too long
#'   check_named_region_violations(strrep("x", 256))
#' }
check_named_region_violations <- function(candidate_region_names) {
  for (name in candidate_region_names) {
    problems <- character(0)

    # 1. Check for illegal characters
    illegal_chars <- unlist(regmatches(name, gregexpr("[^A-Za-z0-9_.]", name)))
    if (length(illegal_chars) > 0) {
      problems <- c(problems,
                    paste0("contains illegal character(s): ", paste(unique(illegal_chars), collapse = " "))
      )
    }

    # 2. Check for illegal starting character
    if (grepl("^[^A-Za-z_\\\\]", name)) {
      problems <- c(problems, "starts with an invalid character (must begin with a letter, underscore, or backslash)")
    }

    # 3. Check if name resembles a cell reference (e.g., A1, Z100)
    if (grepl("^[A-Za-z]{1,3}[1-9][0-9]{0,6}$", name)) {
      problems <- c(problems, "resembles a cell reference (e.g., A1), which is not allowed")
    }

    # 4. Check for length violation
    if (nchar(name) > 255) {
      problems <- c(problems, "exceeds Excel's 255-character limit")
    }

    # 5. Report any problems
    if (length(problems) > 0) {
      warning(
        sprintf("Invalid Excel region name: '%s'\n  Problem(s): %s",
                name, paste(problems, collapse = "; ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}


#' Develop a warning message for malformed Excel worksheet names
#'
#' `write_ecc_to_excel()` can include worksheet names, but
#' it is important that they are legal names.
#' This function emits a warning when `candidate_worksheet_names`
#' is mal-formed.
#'
#' @param candidate_worksheet_names Worksheet names to be checked.
#'
#' @returns `NULL` invisibly and a warning if any problems are detected.
#'
#' @export
#'
#' @examples
#' # No warning
#' check_worksheet_name_violations(c("abc", "123"))
#' \dontrun{
#'   # Warnings
#'   # Illegal characters
#'   check_worksheet_name_violations(c("abc", "["))
#'   # Empty name
#'   check_worksheet_name_violations(c("", "abc"))
#'   # Too long
#'   check_worksheet_name_violations(strrep("x", 32))
#'   # Duplicates
#'   check_worksheet_name_violations(c("abc123", "abc123"))
#' }
check_worksheet_name_violations <- function(candidate_worksheet_names) {
  seen <- character(0)

  for (name in candidate_worksheet_names) {
    problems <- character(0)

    # 1. Check for illegal characters: \ / * ? [ ]
    matches <- gregexpr("(\\\\|/|\\*|\\?|\\[|\\])", name)[[1]]
    if (any(matches > 0)) {
      illegal_chars <- substring(name, matches, matches)
      problems <- c(problems,
                    paste0("contains illegal character(s): ",
                           paste(unique(illegal_chars), collapse = " "))
      )
    }

    # 2. Check for empty names
    if (nchar(name) == 0) {
      problems <- c(problems, "is blank (worksheet names cannot be empty)")
    }

    # 3. Check for length
    if (nchar(name) > 31) {
      problems <- c(problems, "exceeds Excel's 31-character limit")
    }

    # 4. Check for duplicates
    if (name %in% seen) {
      problems <- c(problems, "is a duplicate (worksheet names must be unique)")
    } else {
      seen <- c(seen, name)
    }

    # Report problems
    if (length(problems) > 0) {
      warning(
        sprintf("Invalid Excel worksheet name: '%s'\n  Problem(s): %s",
                name, paste(problems, collapse = "; ")
        ),
        call. = FALSE
      )
    }
  }

  invisible(NULL)
}


#' Read an energy conversion chain from a Excel file
#'
#' Reads matrices from named regions in an Excel file
#' into `matsindf` format.
#' The named regions are assumed to be global
#' to the workbook.
#' The format for the region names is assumed to be
#' `<<matrix symbol>><<sep>><<worksheet name>>`,
#' as written by [write_ecc_to_excel()].
#' For example, "R__4" for the **R** matrix on the sheet named "4".
#'
#' Named regions are assumed to include
#' the rectangle of numerical values,
#' row names (to the left of the rectangle of numbers), and
#' column names (above the rectangle of numbers),
#' the same format
#' as written by [write_ecc_to_excel()].
#'
#' This function is an inverse of [write_ecc_to_excel()].
#'
#' @param path The path to the Excel file.
#' @param worksheets Names of worksheets from which matrices
#'                   are to be read.
#'                   Default is `NULL`, meaning that all
#'                   worksheets are read.
#' @param add_rc_types A boolean that tells whether to
#'                     add row and column types
#'                     to the outgoing matrices.
#'                     When `TRUE`, matrix names are determined by
#'                     [add_row_col_types()].
#'                     Default is `TRUE`.
#' @param worksheet_names_colname The name of a column in
#'                                the outgoing data frame that
#'                                contains the names of worksheets
#'                                on which the matrices were found.
#'                                Default is "WorksheetNames".
#' @param R,U,V,Y,r_eiou,U_eiou,U_feed,S_units String names for regions
#'                                             in the file at `path`
#'                                             containing matrices.
#'                                             See `Recca::psut_cols`
#'                                             for defaults.
#' @param industry_type,product_type,unit_type String names of row and
#'                                             column types optionally
#'                                             applied to matrices
#'                                             read from the file at
#'                                             `path`.
#'                                             Defaults are taken from
#'                                             `Recca::row_col_types`.
#' @param sep The separator between matrix name and worksheet name
#'            for named regions.
#'            Default is "__".
#'
#' @returns A data frame in `matsindf` format containing
#'          the matrices from named regions in
#'          the file at `path`.
#'
#' @export
#'
#' @seealso [write_ecc_to_excel()]
#'
#' @examples
#' \dontrun{
#'   ecc <- UKEnergy2000mats |>
#'     tidyr::spread(key = "matrix.name", value = "matrix") |>
#'     dplyr::mutate(
#'       WorksheetNames = paste0(EnergyType, "_", LastStage)
#'     )
#'   ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file",
#'                             fileext = ".xlsx")
#'   write_ecc_to_excel(ecc,
#'                      path = ecc_temp_path,
#'                      worksheet_names = "WorksheetNames",
#'                      overwrite = TRUE)
#'   # Now read the regions
#'   ecc_temp_path |>
#'     read_ecc_from_excel()
#'   if (file.exists(ecc_temp_path)) {
#'     file.remove(ecc_temp_path)
#'   }
#' }
read_ecc_from_excel <- function(path,
                                worksheets = NULL,
                                add_rc_types = TRUE,
                                worksheet_names_colname = "WorksheetNames",
                                R = Recca::psut_cols$R,
                                U = Recca::psut_cols$U,
                                V = Recca::psut_cols$V,
                                Y = Recca::psut_cols$Y,
                                r_eiou = Recca::psut_cols$r_eiou,
                                U_eiou = Recca::psut_cols$U_eiou,
                                U_feed = Recca::psut_cols$U_feed,
                                S_units = Recca::psut_cols$S_units,
                                industry_type = Recca::row_col_types$industry_type,
                                product_type = Recca::row_col_types$product_type,
                                unit_type = Recca::row_col_types$unit_type,
                                sep = "__") {

  workbook <- openxlsx2::wb_load(path)

  if (is.null(worksheets)) {
    worksheets <- workbook |>
      openxlsx2::wb_get_sheet_names()
  }

  matrix_names <- c(R, U, V, Y,
                    r_eiou, U_eiou, U_feed, S_units)

  # Read all of the worksheets
  worksheets |>
    lapply(FUN = function(this_worksheet) {
      # Set the region names: <<matrix symbol>><<sep>><<worksheet name>>
      region_names <- paste(matrix_names, this_worksheet, sep = sep)
      # Look at all regions
      result <- region_names |>
        sapply(simplify = FALSE,
               USE.NAMES = FALSE,
               FUN = function(this_region) {
                 # Read the region as a data frame
                 df <- openxlsx2::wb_to_df(workbook,
                                           named_region = this_region,
                                           row_names = TRUE)
                 # Convert all NA values (blanks) to 0s
                 df[is.na(df)] <- 0
                 this_matrix <- df |>
                   # Convert the data frame to a matrix
                   as.matrix() |>
                   # Then to a Matrix so it is amenable to
                   # storage in a 1-row column of a tibble.
                   Matrix::Matrix(sparse = TRUE)
                 # Bundle in a vector for easier conversion to a tibble
                 c(this_matrix)
               }) |>
        # Set the names (which will later become column names)
        magrittr::set_names(matrix_names)
      if (add_rc_types) {
        result <- add_row_col_types(matvals = result,
                                    matnames = names(result),
                                    R = R, U = U, V = V, Y = Y,
                                    U_feed = U_feed, U_eiou = U_eiou,
                                    r_eiou = r_eiou, S_units = S_units,
                                    industry_type = industry_type,
                                    product_type = product_type,
                                    unit_type = unit_type) |>
          unlist(recursive = FALSE) |>
          magrittr::set_names(matrix_names)
      }
      result |>
        # Convert to to a row of a data frame
        tibble::as_tibble_row() |>
        # Set the name of the column that contains the worksheets
        dplyr::mutate(
          "{worksheet_names_colname}" := this_worksheet
        ) |>
        # Put the worksheet name column first (leftmost) in the data frame
        dplyr::relocate(tidyselect::all_of(worksheet_names_colname))
    }) |>
    # Bind each row into a data frame
    dplyr::bind_rows()
}
