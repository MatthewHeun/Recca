#' Write energy conversion chain matrices in an Excel file
#'
#' It is often helpful to see energy conversion chain (ECC) matrices in Excel format,
#' arranged spatially.
#' This function takes ECC matrices and writes them to an Excel file.
#'
#' If `.psut_data` is a PSUT data frame,
#' each row is written to a different tab in the output file at `path`.
#'
#' When `include_named_regions` is `TRUE` (the default),
#' named regions for matrices are added to Excel sheets.
#' The format for the names is <<matrix>>_<<sheet name>>.
#' For example, "R_4" for the **R** matrix on the sheet named "4".
#' The names help to identify matrices in high-level views of the Excel file.
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
#' @param pad The number of rows and columns between adjacent matrices in the Excel sheet.
#'            Default is `2`.
#' @param include_named_regions A boolean that tells whether to name regions of
#'                              the Excel tabs according to matrices.
#'                              Default is `TRUE`.
#' @param R,U,U_feed,U_eiou,r_eiou,V,Y,S_units Names of ECC matrices or actual matrices.
#'                                             See `Recca::psut_cols`.
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
#' @examples
#' \dontrun{
#' ecc <- UKEnergy2000mats %>%
#'   tidyr::spread(key = "matrix.name", value = "matrix")
#' ecc_temp_path <- tempfile(pattern = "write_excel_ecc_test_file", fileext = ".xlsx")
#' write_ecc_to_excel(ecc, path = ecc_temp_path, overwrite = TRUE)
#' }
write_ecc_to_excel <- function(.psut_data = NULL,
                               path,
                               overwrite_file = FALSE,
                               pad = 2,
                               # include_io_mats = FALSE,
                               include_named_regions = TRUE,
                               R = Recca::psut_cols$R,
                               U = Recca::psut_cols$U,
                               V = Recca::psut_cols$V,
                               Y = Recca::psut_cols$Y,
                               r_eiou = Recca::psut_cols$r_eiou,
                               U_eiou = Recca::psut_cols$U_eiou,
                               U_feed = Recca::psut_cols$U_feed,
                               S_units = Recca::psut_cols$S_units,
                               .wrote_mats_colname = "Wrote mats",
                               UV_bg_color = "#FDF2D0",
                               RY_bg_color = "#D3712D",
                               calculated_bg_color = "#D9D9D9",
                               col_widths = 7) {

  # Check if path exists. If so, throw an error.
  if (file.exists(path) & !overwrite_file) {
    stop(paste("File", path, "already exists. Call write_ecc_to_excel with overwrite = TRUE to overwrite."))
  }
  # Create the workbook
  ecc_wb <- openxlsx::createWorkbook()

  create_one_tab <- function(R_mat, U_mat, V_mat, Y_mat, U_eiou_mat, U_feed_mat, r_eiou_mat, S_units_mat) {

    # Get existing sheet names
    existing_sheets <- openxlsx::sheets(ecc_wb)
    if (length(existing_sheets) == 0) {
      sheet_name <- "1"
    } else {
      sheet_name <- (as.integer(existing_sheets) %>% max()) + 1
    }
    # Add the worksheet to the workbook
    openxlsx::addWorksheet(ecc_wb, sheet_name)

    # Complete matrices relative to one another to make sure we have same number
    # of rows or columns, as appropriate
    # Ensure same columns of U and rows of V
    U_mat_T <- matsbyname::transpose_byname(U_mat)
    completedUV <- matsbyname::complete_and_sort(U_mat_T, V_mat, margin = 1)
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
    Map(list("R", "U", "V", "Y", "r_eiou", "U_eiou", "U_feed", "S_units"),
        list(R_mat, U_mat, V_mat, Y_mat, r_eiou_mat, U_eiou_mat, U_feed_mat, S_units_mat),
        locations,
        f = function(this_mat_name, this_mat, this_loc) {
          # Find the locations of the matrix origin and matrix extent
          # from this_loc.
          # We'll use this in many places below.
          mat_origin <- this_loc[["origin"]] + c(x = 1, y = 1)  # Offset for the row and column names
          mat_extent <- this_loc[["extent"]] + c(x = 0, y = -1) # Offset for the matrix label
          # Write the data
          openxlsx::writeData(wb = ecc_wb,
                              sheet = sheet_name,
                              # Account for the fact that this_mat could be a
                              # non-native matrix class (such as Matrix)
                              x = as.matrix(this_mat),
                              xy = this_loc[["origin"]],
                              array = TRUE, colNames = TRUE, rowNames = TRUE)
          # Set the background color to matrix_bg_color for the numbers in the matrix
          # Define the matrix numbers style
          if (this_mat_name %in% c("R", "Y")) {
            this_bg_color <- RY_bg_color
          } else if (this_mat_name %in% c("U", "V")) {
            this_bg_color <- UV_bg_color
          } else {
            this_bg_color <- calculated_bg_color
          }
          if (include_named_regions) {
            # Set the name of this range for this matrix
            openxlsx::createNamedRegion(wb = ecc_wb,
                                        sheet = sheet_name,
                                        rows = mat_origin[["y"]]:mat_extent[["y"]],
                                        cols = mat_origin[["x"]]:mat_extent[["x"]],
                                        name = paste(this_mat_name, sheet_name, sep = "_"),
                                        # Set false to flag any problems.
                                        overwrite = FALSE)
          }
          # Create the style for this matrix.
          mat_num_style <- openxlsx::createStyle(fgFill = this_bg_color,
                                                 halign = "center",
                                                 valign = "center")
          openxlsx::addStyle(wb = ecc_wb,
                             sheet = sheet_name,
                             style = mat_num_style,
                             rows = mat_origin[["y"]]:mat_extent[["y"]],
                             cols = mat_origin[["x"]]:mat_extent[["x"]],
                             gridExpand = TRUE,
                             stack = TRUE)
          # Rotate and center column labels
          col_label_style <- openxlsx::createStyle(textRotation = 90,
                                                   halign = "center",
                                                   valign = "bottom")
          openxlsx::addStyle(wb = ecc_wb,
                             sheet = sheet_name,
                             style = col_label_style,
                             rows = this_loc[["origin"]][["y"]],
                             cols = this_loc[["origin"]][["x"]]:this_loc[["extent"]][["x"]],
                             gridExpand = TRUE,
                             stack = TRUE)
          # Right align row labels
          row_label_style <- openxlsx::createStyle(halign = "right",
                                                   valign = "center")
          openxlsx::addStyle(wb = ecc_wb,
                             sheet = sheet_name,
                             style = row_label_style,
                             rows = this_loc[["origin"]][["y"]]:this_loc[["extent"]][["y"]],
                             cols = this_loc[["origin"]][["x"]],
                             gridExpand = TRUE,
                             stack = TRUE)
          # Add matrix label
          openxlsx::writeData(wb = ecc_wb,
                              sheet = sheet_name,
                              x = this_mat_name,
                              startRow = this_loc[["extent"]][["y"]],
                              startCol = mat_origin[["x"]])
          # Format matrix label
          mat_name_style <- openxlsx::createStyle(halign = "center",
                                                  textDecoration = "Bold")
          openxlsx::addStyle(wb = ecc_wb,
                             sheet = sheet_name,
                             style = mat_name_style,
                             rows = this_loc[["extent"]][["y"]],
                             cols = mat_origin[["x"]],
                             gridExpand = TRUE,
                             stack = TRUE)
          openxlsx::mergeCells(wb = ecc_wb,
                               sheet = sheet_name,
                               rows = this_loc[["extent"]][["y"]],
                               cols = mat_origin[["x"]]:mat_extent[["x"]])
          # Set column widths to "auto" to save space.
          openxlsx::setColWidths(wb = ecc_wb,
                                 sheet = sheet_name,
                                 cols = mat_origin[["x"]]:mat_extent[["x"]],
                                 widths = col_widths,
                                 ignoreMergedCells = TRUE)
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
                                  S_units_mat = S_units)
  # Make sure the directory exists
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  # Write the workbook
  openxlsx::saveWorkbook(ecc_wb, file = path, overwrite = overwrite_file)
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


