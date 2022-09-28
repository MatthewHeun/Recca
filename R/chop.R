#' Chop the **R** and **Y** matrices and swim downstream/upstream
#'
#' Chopping the resource (**R**) or final demand (**Y**) matrices
#' involves isolating products and industries then
#' swimming downstream/upstream to identify an energy conversion chain (ECC)
#' associated with each resource or final demand category.
#' These functions perform those calculations.
#'
#' Chopping **R** involves calculating an ECC for each product column in the **R** matrix.
#' This calculation is accomplished for each description of an energy conversion chain (ECC)
#' by the following algorithm:
#'
#' 1. Calculate io matrices with `calc_io_mats()`.
#' 2. Identify each product from columns of the **R** matrix.
#' 3. For each product independently,
#'    perform an downstream swim with `new_R_ps()`
#'    to obtain the ECC induced by that product only.
#' 4. Optionally (but included by default),
#'    calculate primary and final demand aggregates using `primary_aggregates()` and
#'    `finaldemand_aggregates()`.
#'    Both functions are called with `by = "Total"`,
#'    yielding total primary and final demand aggregates.
#' 5. Add the chopped ECCs to the right side
#'    of `.sut_data` as a nested data frame.
#'    If calculated, add the primary and final demand aggregates
#'    as columns in the nested data frame.
#'
#' Chopping **Y** involves calculating an ECC for each individual
#' product row and sector column of final demand in the **Y** matrix.
#' This calculation is accomplished for each description of an ECC
#' by the following algorithm:
#'
#' 1. Calculate io matrices with `calc_io_mats()`.
#' 2. Identify each product and sector from rows and columns of the **Y** matrix.
#' 3. For each product and sector independently,
#'    perform an upstream swim with `new_Y()`
#'    to obtain the ECC requirements to supply that product or sector only.
#' 4. Optionally (but included by default),
#'    calculate primary and final demand aggregates using `primary_aggregates()` and
#'    `finaldemand_aggregates()`.
#'    Both functions are called with `by = "Total"`,
#'    yielding total primary and final demand aggregates.
#' 5. Add the chopped ECCs to the right side
#'    of `.sut_data` as a nested data frame.
#'    If calculated, add the primary and final demand aggregates
#'    as columns in the nested data frame.
#'
#' Use `unnest` to define how the aggregate data are added to the right side of `.sut_data`
#' when `.sut_data` is a `matsindf` data frame.
#'
#' Note that the nested data frame includes columns for the ECC matrices
#' for each isolated product or sector.
#' Optionally, the nested data frame includes primary and final demand aggregates
#' for the chopped ECCs.
#' The names of the columns in the data frame are taken from the `*_prime_colname` arguments.
#'
#' `chop_R()` and `chop_Y()` involve downstream and upstream swims
#' performed by the `new_R_ps()` and `new_Y()` functions.
#' Both involve matrix inverses.
#' The `method` arguments specify how the matrix inversion is accomplished.
#' The `tol` argument specifies the tolerance for detecting linearities in the matrix
#' to be inverted.
#' See the documentation at `matsbyname::invert_byname()` for details.
#'
#' Both `tol` and `method` should be a single values and apply to all rows of `.sut_data`.
#'
#' When the **R** and **Y** matrices are chopped by rows or columns, the sum of the ECCs
#' created from the chopped rows or columns should equal the original ECC.
#' Internally, this function checks for sum consistency and emits an error if not.
#'
#' @param .sut_data A data frame or list of physical supply-use table matrices.
#'                  Default is `NULL`.
#' @param calc_pfd_aggs A boolean that tells whether (`TRUE`) or not (`FALSE`)
#'                      to include primary and final demand aggregates to the
#'                      nested data frame.
#' @param p_industries A vector of names of industries to be aggregated as "primary"
#'                     and used if aggregations are requested.
#'                     If `.sut_data` is a data frame, `p_industries` should be the name of a column in the data frame.
#'                     If `.sut_data` is `NULL`, `p_industries` can be a single vector of industry names.
#'                     These industries in `p_industries` will appear in rows of the resource (**R**) and make (**V**) matrices and
#'                     columns of the final demand matrix (**Y**).
#'                     Entries in **Y_p** will be subtracted from entries in **R_p** `+` **V_p** to obtain
#'                     the total primary energy aggregate,
#'                     where `*_p` is the primary part of those matrices.
#'                     The function `find_p_industry_names()` might be helpful to find
#'                     primary industry names if they can be identified by prefixes.
#'                     This argument is passed to `primary_aggregates()`.
#'                     Default is `NULL`.
#' @param fd_sectors A vector of names of sectors in final demand
#'                   and used if aggregations are requested.
#'                   Names should include columns in the **Y** and **U_EIOU** matrices
#'                   to cover both net (in **Y**) and gross (in **Y** and **U_EIOU**) final demand.
#'                   This argument is passed to `finaldemand_aggregates()`.
#'                   Default is `NULL`.
#' @param pattern_type One of "exact", "leading", "trailing", or "anywhere" which specifies
#'                     how matches are made for `p_industries`.
#'                     If "exact", exact matches specify the sectors to be aggregated.
#'                     If "leading", sectors are aggregated if any entry in `p_industries` matches the leading part of a final demand sector's name.
#'                     If "trailing", sectors are aggregated if any entry in `p_industries` matches the trailing part of a final demand sector's name.
#'                     If "anywhere", sectors are aggregated if any entry in `p_industries` matches any part of a final demand sector's name.
#'                     Default is "exact".
#'                     This argument is passed to both `primary_aggregates()` and `finaldemand_aggregates()`.
#' @param unnest A boolean that tells whether to unnest the outgoing data.
#'               When `TRUE`, creates a new column called `product_sector` and columns of primary and final demand aggregates.
#'               Default is `FALSE`.
#' @param method One of "solve", "QR", or "SVD". Default is "solve". See details.
#' @param tol_invert The tolerance for detecting linear dependencies in the columns inverted matrices.
#'                   Default is `.Machine$double.eps`.
#' @param tol_chop_sum The allowable deviation from `0` for the difference between
#'                     the sum of the chopped ECCs and the original ECC.
#'                     Default is `1e-4`.
#' @param R,U,U_feed,V,Y,S_units Matrices that describe the energy conversion chain (ECC).
#'                               See `Recca::psut_cols` for default values.
#' @param product_sector The name of the output column that contains the product, industry, or sector
#'                       for which footprint aggregates are given.
#'                       Default is `Recca::aggregate_cols$product_sector`.
#' @param chop_df,aggregate_primary,net_aggregate_demand,gross_aggregate_demand Names of output columns.
#'                                                                                    See `Recca::aggregate_cols`.
#' @param .prime A string that denotes new matrices.
#'               This string is used as a suffix that is appended to
#'               many variable names.
#'               Default is "_prime".
#' @param R_colname,U_colname,U_feed_colname,U_eiou_colname,r_eiou_colname,V_colname,Y_colname Names of input matrices in `.sut_data`. See `Recca::psut_cols` for default values.
#' @param R_prime_colname,U_prime_colname,U_feed_prime_colname,U_eiou_prime_colname,r_eiou_prime_colname,V_prime_colname,Y_prime_colname Names of output matrices in the return value.
#'                                        Default values are constructed from
#'                                        `Recca::psut_cols` values suffixed with
#'                                        the value of the `.prime` argument.
#'
#' @return Chopped **R** and **Y** energy conversion chains
#'         with optional primary and final demand aggregates.
#'
#' @examples
#' p_industries <- c("Resources - Crude", "Resources - NG")
#' fd_sectors <- c("Residential", "Transport", "Oil fields")
#' psut_mats <- UKEnergy2000mats %>%
#'   tidyr::pivot_wider(names_from = matrix.name, values_from = matrix)
#' psut_mats %>%
#'   chop_Y(p_industries = p_industries, fd_sectors = fd_sectors)
#' psut_mats %>%
#'   chop_Y(p_industries = p_industries, fd_sectors = fd_sectors, unnest = TRUE)
#' psut_mats_2 <- psut_mats %>%
#'   # Slice to avoid the services rows on which NA values are obtained due to unit homogeneity.
#'   dplyr::filter(Last.stage != "Services")
#' # Calculate aggregates
#' psut_mats_2 %>%
#'   chop_R(p_industries = p_industries, fd_sectors = fd_sectors)
#' psut_mats_2 %>%
#'   chop_R(p_industries = p_industries, fd_sectors = fd_sectors, unnest = TRUE)
#' @name chop-doc
NULL



#' @export
#' @rdname chop-doc
chop_Y <- function(.sut_data = NULL,
                   calc_pfd_aggs = TRUE,
                   p_industries = NULL,
                   fd_sectors = NULL,
                   pattern_type = c("exact", "leading", "trailing", "anywhere"),
                   unnest = FALSE,
                   method = c("solve", "QR", "SVD"),
                   tol_invert = .Machine$double.eps,
                   tol_chop_sum = 1e-4,
                   # Input names or matrices
                   R = Recca::psut_cols$R,
                   U = Recca::psut_cols$U,
                   U_feed = Recca::psut_cols$U_feed,
                   V = Recca::psut_cols$V,
                   Y = Recca::psut_cols$Y,
                   S_units = Recca::psut_cols$S_units,
                   # Output names
                   chop_df = Recca::aggregate_cols$chop_df,
                   product_sector = Recca::aggregate_cols$product_sector,
                   aggregate_primary = Recca::aggregate_cols$aggregate_primary,
                   net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
                   gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand,
                   # Other internal names
                   .prime = "_prime",
                   R_colname = Recca::psut_cols$R,
                   U_colname = Recca::psut_cols$U,
                   U_feed_colname = Recca::psut_cols$U_feed,
                   U_eiou_colname = Recca::psut_cols$U_eiou,
                   r_eiou_colname = Recca::psut_cols$r_eiou,
                   V_colname = Recca::psut_cols$V,
                   Y_colname = Recca::psut_cols$Y,
                   R_prime_colname = paste0(R_colname, .prime),
                   U_prime_colname = paste0(U_colname, .prime),
                   U_feed_prime_colname = paste0(U_feed_colname, .prime),
                   U_eiou_prime_colname = paste0(U_eiou_colname, .prime),
                   r_eiou_prime_colname = paste0(r_eiou_colname, .prime),
                   V_prime_colname = paste0(V_colname, .prime),
                   Y_prime_colname = paste0(Y_colname, .prime)) {

  pattern_type <- match.arg(pattern_type)
  method <- match.arg(method)

  footprint_func <- function(R_mat, U_mat, U_feed_mat, V_mat, Y_mat, S_units_mat) {
    # At this point, we have single matrices for each of the above variables.
    # Calculate the IO matrices
    with_io <- list(R = R_mat, U = U_mat, U_feed = U_feed_mat, V = V_mat, Y = Y_mat, S_units = S_units_mat) %>%
      # We accept the default vector and matrix names.
      calc_io_mats(method = method, tol = tol_invert)

    # Get the row names in Y. Those are the Products we want to evaluate.
    product_names <- matsbyname::getrownames_byname(Y_mat)
    new_Y_products <- product_names %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_product) {
        # For each product (in each row), make a new Y matrix to be used for the calculation.
        Y_mat %>%
          matsbyname::select_rows_byname(Hmisc::escapeRegex(this_product))
      })

    # Get the column names in Y. Those are the Sectors we want to evaluate.
    sector_names <- matsbyname::getcolnames_byname(Y_mat)
    new_Y_sectors <- sector_names %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_sector) {
        # For each sector (in each column), make a new Y matrix to be used for the calculation.
        Y_mat %>%
          matsbyname::select_cols_byname(Hmisc::escapeRegex(this_sector))
      })

    # Create a list with new Y matrices for all products and sectors
    new_Y_list <- c(new_Y_products, new_Y_sectors)

    # For each item in this list, make a new set of ECC matrices
    ecc_prime <- new_Y_list %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_new_Y) {
        with_io %>%
          append(list(this_new_Y) %>% magrittr::set_names(Y_prime_colname)) %>%
          # Calculate all the new ECC matrices,
          # accepting the default names for intermediate
          # vectors and matrices.
          # We can accept default names for L_ixp, L_pxp, Z, Z_feed, D, and O,
          # because we didn't change those names in the call to calc_io_mats().
          # This gives the new (prime) description of the ECC.
          new_Y(Y_prime = Y_prime_colname,
                R_prime = R_prime_colname,
                U_prime = U_prime_colname,
                U_feed_prime = U_feed_prime_colname,
                U_eiou_prime = U_eiou_prime_colname,
                r_eiou_prime = r_eiou_prime_colname,
                V_prime = V_prime_colname)
      })

    # Verify that energy is balanced.
    # The sum of the ECCs associated with new_Y_products should be equal to the original ECC.
    product_prime_mats <- ecc_prime[product_names] %>%
      purrr::transpose()
    product_prime_balanced <- verify_chop_energy_sum(tol = tol_chop_sum,
                                                     R_mat = R_mat,
                                                     U_mat = U_mat,
                                                     U_feed_mat = U_feed_mat,
                                                     V_mat = V_mat,
                                                     Y_mat = Y_mat,
                                                     R_chop_list = product_prime_mats[[R_prime_colname]],
                                                     U_chop_list = product_prime_mats[[U_prime_colname]],
                                                     U_feed_chop_list = product_prime_mats[[U_feed_prime_colname]],
                                                     V_chop_list = product_prime_mats[[V_prime_colname]],
                                                     Y_chop_list = product_prime_mats[[Y_prime_colname]])
    assertthat::assert_that(product_prime_balanced, msg = "Products not balanced in footprint_aggregates()")

    # The sum of the ECCs associated with new_Y_sectors should be equal to the original ECC.
    sector_prime_mats <- ecc_prime[sector_names] %>%
      purrr::transpose()
    sector_prime_balanced <- verify_chop_energy_sum(tol = tol_chop_sum,
                                                    R_mat = R_mat,
                                                    U_mat = U_mat,
                                                    U_feed_mat = U_feed_mat,
                                                    V_mat = V_mat,
                                                    Y_mat = Y_mat,
                                                    R_chop_list = sector_prime_mats[[R_prime_colname]],
                                                    U_chop_list = sector_prime_mats[[U_prime_colname]],
                                                    U_feed_chop_list = sector_prime_mats[[U_feed_prime_colname]],
                                                    V_chop_list = sector_prime_mats[[V_prime_colname]],
                                                    Y_chop_list = sector_prime_mats[[Y_prime_colname]])
    assertthat::assert_that(sector_prime_balanced, msg = "Sectors not balanced in footprint_aggregates()")

    # Calculate primary and final demand aggregates for each of the new ECCs.
    calc_aggregates_from_ecc_prime(ecc_prime,
                                   calc_pfd_aggs = calc_pfd_aggs,
                                   p_industries = p_industries,
                                   fd_sectors = fd_sectors,
                                   pattern_type = pattern_type,
                                   aggregate_primary = aggregate_primary,
                                   gross_aggregate_demand = gross_aggregate_demand,
                                   net_aggregate_demand = net_aggregate_demand,
                                   chop_df = chop_df,
                                   product_sector = product_sector,
                                   R_prime_colname = R_prime_colname,
                                   U_prime_colname = U_prime_colname,
                                   U_feed_prime_colname = U_feed_prime_colname,
                                   U_eiou_prime_colname = U_eiou_prime_colname,
                                   r_eiou_prime_colname = r_eiou_prime_colname,
                                   V_prime_colname = V_prime_colname,
                                   Y_prime_colname = Y_prime_colname)
  }

  out <- matsindf::matsindf_apply(.sut_data,
                                  FUN = footprint_func,
                                  R_mat = R,
                                  U_mat = U,
                                  U_feed_mat = U_feed,
                                  V_mat = V,
                                  Y_mat = Y,
                                  S_units_mat = S_units)

  # If .sut_data is a data frame, unnest if desired.
  if (is.data.frame(.sut_data) & unnest) {
    out <- out %>%
      tidyr::unnest(cols = chop_df)
  }
  return(out)
}


#' @export
#' @rdname chop-doc
chop_R <- function(.sut_data = NULL,
                   calc_pfd_aggs = TRUE,
                   p_industries = NULL,
                   fd_sectors = NULL,
                   pattern_type = c("exact", "leading", "trailing", "anywhere"),
                   unnest = FALSE,
                   method = c("solve", "QR", "SVD"),
                   tol_invert = .Machine$double.eps,
                   tol_chop_sum = 1e-4,
                   # Input names or matrices
                   R = Recca::psut_cols$R,
                   U = Recca::psut_cols$U,
                   U_feed = Recca::psut_cols$U_feed,
                   V = Recca::psut_cols$V,
                   Y = Recca::psut_cols$Y,
                   S_units = Recca::psut_cols$S_units,
                   # Output names
                   chop_df = Recca::aggregate_cols$chop_df,
                   product_sector = Recca::aggregate_cols$product_sector,
                   aggregate_primary = Recca::aggregate_cols$aggregate_primary,
                   net_aggregate_demand = Recca::aggregate_cols$net_aggregate_demand,
                   gross_aggregate_demand = Recca::aggregate_cols$gross_aggregate_demand,
                   # Other internal names
                   .prime = "_prime",
                   R_colname = Recca::psut_cols$R,
                   U_colname = Recca::psut_cols$U,
                   U_feed_colname = Recca::psut_cols$U_feed,
                   U_eiou_colname = Recca::psut_cols$U_eiou,
                   r_eiou_colname = Recca::psut_cols$r_eiou,
                   V_colname = Recca::psut_cols$V,
                   Y_colname = Recca::psut_cols$Y,
                   R_prime_colname = paste0(R_colname, .prime),
                   U_prime_colname = paste0(U_colname, .prime),
                   U_feed_prime_colname = paste0(U_feed_colname, .prime),
                   U_eiou_prime_colname = paste0(U_eiou_colname, .prime),
                   r_eiou_prime_colname = paste0(r_eiou_colname, .prime),
                   V_prime_colname = paste0(V_colname, .prime),
                   Y_prime_colname = paste0(Y_colname, .prime)) {

  effects_func <- function(R_mat, U_mat, U_feed_mat, V_mat, Y_mat, S_units_mat) {

    # Get the column names in R. Those are the Products we want to evaluate.
    product_names <- matsbyname::getcolnames_byname(R_mat)
    new_R_products <- product_names %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_product) {
        # For each product (in each column), make a new Y matrix to be used for the calculation.
        R_mat %>%
          matsbyname::select_cols_byname(Hmisc::escapeRegex(this_product))
      })

    # For each item in this list, make a new set of ECC matrices
    with_qf <- list(R = R_mat, U = U_mat, U_feed = U_feed_mat,
                    V = V_mat, Y = Y_mat, S_units = S_units_mat) %>%
      calc_yqfgW()
    ecc_prime <- new_R_products %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_new_R) {
        with_qf %>%
          append(list(this_new_R) %>% magrittr::set_names(R_prime_colname)) %>%
          # Calculate all the new ECC matrices,
          # giving the new (prime) description of the ECC.
          new_R_ps(R_prime = R_prime_colname,
                   U_prime = U_prime_colname,
                   V_prime = V_prime_colname,
                   Y_prime = Y_prime_colname)
      })

    # Verify that energy is balanced.
    # The sum of the ECCs associated with new_R_products should be equal to the original ECC.
    product_prime_mats <- ecc_prime[product_names] %>%
      purrr::transpose()
    product_prime_balanced <- verify_chop_energy_sum(tol = tol_chop_sum,
                                                     R_mat = R_mat,
                                                     U_mat = U_mat,
                                                     U_feed_mat = U_feed_mat,
                                                     V_mat = V_mat,
                                                     Y_mat = Y_mat,
                                                     R_chop_list = product_prime_mats[[R_prime_colname]],
                                                     U_chop_list = product_prime_mats[[U_prime_colname]],
                                                     U_feed_chop_list = product_prime_mats[[U_feed_prime_colname]],
                                                     V_chop_list = product_prime_mats[[V_prime_colname]],
                                                     Y_chop_list = product_prime_mats[[Y_prime_colname]])
    assertthat::assert_that(product_prime_balanced, msg = "Products not balanced in effects_aggregates()")

    # Calculate primary and final demand aggregates for each of the new ECCs.
    calc_aggregates_from_ecc_prime(ecc_prime,
                                   calc_pfd_aggs = calc_pfd_aggs,
                                   p_industries = p_industries,
                                   fd_sectors = fd_sectors,
                                   pattern_type = pattern_type,
                                   aggregate_primary = aggregate_primary,
                                   gross_aggregate_demand = gross_aggregate_demand,
                                   net_aggregate_demand = net_aggregate_demand,
                                   chop_df = chop_df,
                                   product_sector = product_sector,
                                   R_prime_colname = R_prime_colname,
                                   U_prime_colname = U_prime_colname,
                                   U_feed_prime_colname = U_feed_prime_colname,
                                   U_eiou_prime_colname = U_eiou_prime_colname,
                                   r_eiou_prime_colname = r_eiou_prime_colname,
                                   V_prime_colname = V_prime_colname,
                                   Y_prime_colname = Y_prime_colname)
  }

  out <- matsindf::matsindf_apply(.sut_data,
                                  FUN = effects_func,
                                  R_mat = R,
                                  U_mat = U,
                                  U_feed_mat = U_feed,
                                  V_mat = V,
                                  Y_mat = Y,
                                  S_units_mat = S_units)

  # If .sut_data is a data frame, unnest if desired.
  if (is.data.frame(.sut_data) & unnest) {
    out <- out %>%
      tidyr::unnest(cols = chop_df)
  }
  return(out)
}


#' Verify energy sum after chop calculations
#'
#' **R** and **Y** chop calculations involve
#' isolating rows or columns of the **R** and **Y** matrices,
#' performing downstream swims (with `new_R_ps()`) and
#' upstream swims (with `new_Y()`), and
#' creating the ECC portions that
#' follow from the row or column of **R** or
#' support the creation of the row or column of **Y**.
#' After performing that downstream or upstream swim, the sum of the
#' isolated (chopped) ECCs should equal the original ECC.
#' This function performs that energy balance verification.
#'
#' The various `*_chop_list` arguments should be lists of matrices
#' formed by isolating (chopping) different parts of **R** or **Y**.
#' The matrices in `R_chop_list`, `U_chop_list`, `U_feed_chop_list`
#' `V_chop_list`, and `Y_chop_list` should sum to
#' `R`, `U`, `U_feed`, `V`, and `Y`, respectively.
#'
#' This is not a public function.
#' It is an internal helper function
#' for `chop_R()` and `chop_Y()`.
#'
#' @param .sut_data An optional data frame of energy conversion chain matrices.
#' @param tol The tolerance within which energy balance is assumed to be OK. Default is `1e-4`.
#' @param R_mat,U_mat,U_feed_mat,V_mat,Y_mat The matrices of the original ECC.
#' @param R_chop_list,U_chop_list,U_feed_chop_list,V_chop_list,Y_chop_list Lists of matrices from different upstream swims corresponding to different rows or columns of **Y**.
#'
#' @return `TRUE` if energy balance is observed, `FALSE` otherwise.
verify_chop_energy_sum <- function(.sut_data = NULL,
                                   tol = 1e-4,
                                   R_mat, U_mat, U_feed_mat, V_mat, Y_mat,
                                   R_chop_list, U_chop_list, U_feed_chop_list, V_chop_list, Y_chop_list) {

  verify_func <- function(chop_list, mat) {
    mat_sum <- matsbyname::sum_byname(chop_list, .summarise = TRUE)[[1]] %>%
      matsbyname::clean_byname()
    err <- matsbyname::difference_byname(mat_sum, mat)
    OK <- matsbyname::iszero_byname(err, tol = tol)
    if (!OK) {
      warning("energy balance not observed in verify_chop_energy_sum()")
    }
    return(OK)
  }

  # Build lists of matrices
  chop_list <- list(R_chop_list, U_chop_list, U_feed_chop_list, V_chop_list, Y_chop_list)
  mat_list <- list(R_mat, U_mat, U_feed_mat, V_mat, Y_mat)
  # Map across each list to ensure the chop_list sums to the matrix.
  Map(f = verify_func, chop_list, mat_list) %>%
    unlist() %>%
    all()
}


#' Calculate aggregates from list of reconstructed ECCs
#'
#' This is a helper function for `footprint_aggregates()` and `effects_aggregates()`.
#' It calculates the primary and final demand aggregates for a list of
#' reconstructed energy conversion chains (ECCs) in `ecc_prime`.
#'
#' This is not a public function.
#' It is an internal helper function
#' for `footprint_aggregates()` and `effects_aggregates()`.
#'
#' @param ecc_prime A list of reconstructed energy conversion chains.
#' @param calc_pfd_aggs Tells whether to calculate and add primary and final demand aggregates
#'                      to the nested data frame.
#' @param p_industries A vector of names of industries to be aggregated as "primary."
#'                     See `footprint_aggregates()` for details.
#' @param fd_sectors A vector of names of sectors in final demand.
#'                   See `footprint_aggregates()` for details.
#' @param pattern_type One of "exact", "leading", "trailing", or "anywhere" which specifies
#'                     how matches are made for `p_industries`.
#'                     See `footprint_aggregates()` for details.
#' @param product_sector The name of the output column that contains the product, industry, or sector
#'                       for which footprint aggregates are given.
#' @param chop_df,aggregate_primary,net_aggregate_demand,gross_aggregate_demand Names of output columns.
#'                                                                                    See `Recca::aggregate_cols`.
#' @param R_prime_colname,U_prime_colname,U_feed_prime_colname,U_eiou_prime_colname,r_eiou_prime_colname,V_prime_colname,Y_prime_colname Names of output matrices in the return value.
#'                                                                                                                                       Default values are constructed from
#' @return A data frame containing reconstructed (prime) matrices and
#'         primary and final demand aggregates in a list suitable for use in `matsindf::matsindf_apply()`.
calc_aggregates_from_ecc_prime <- function(ecc_prime,
                                           calc_pfd_aggs,
                                           p_industries,
                                           fd_sectors,
                                           pattern_type,
                                           product_sector,
                                           chop_df,
                                           aggregate_primary,
                                           gross_aggregate_demand,
                                           net_aggregate_demand,
                                           R_prime_colname,
                                           U_prime_colname,
                                           U_feed_prime_colname,
                                           U_eiou_prime_colname,
                                           r_eiou_prime_colname,
                                           V_prime_colname,
                                           Y_prime_colname) {

  # Callers can opt to not include the aggregates on output,
  # but calculate them here anyway.
  # It is not an expensive operation and it sets up the structure
  # for adding the matrices to the output.

  # Create a data frame of "prime" ECC matrices (to be nested on output)

  ecc_prime_transpose <- purrr::transpose(ecc_prime)

  ecc_primes <- tibble::tibble(names(ecc_prime)) %>%
    magrittr::set_names(product_sector) %>%
    dplyr::mutate(
      "{R_prime_colname}" := ecc_prime_transpose[[R_prime_colname]],
      "{U_prime_colname}" := ecc_prime_transpose[[U_prime_colname]],
      "{U_feed_prime_colname}" := ecc_prime_transpose[[U_feed_prime_colname]],
      "{U_eiou_prime_colname}" := ecc_prime_transpose[[U_eiou_prime_colname]],
      "{r_eiou_prime_colname}" := ecc_prime_transpose[[r_eiou_prime_colname]],
      "{V_prime_colname}" := ecc_prime_transpose[[V_prime_colname]],
      "{Y_prime_colname}" := ecc_prime_transpose[[Y_prime_colname]]
    )

  if (calc_pfd_aggs) {
    # Now that we have the new (prime) ECCs, calculate primary and final demand aggregates
    p_aggregates <- ecc_prime %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_new_ecc) {
        this_new_ecc %>%
          primary_aggregates(p_industries = p_industries,
                             R = R_prime_colname,
                             V = V_prime_colname,
                             Y = Y_prime_colname,
                             pattern_type = pattern_type,
                             by = "Total",
                             aggregate_primary = aggregate_primary)
      }) %>%
      # Transpose to pull EX.p to the top level with products and sectors beneath.
      purrr::transpose()
    fd_aggregates <- ecc_prime %>%
      sapply(simplify = FALSE, USE.NAMES = TRUE, FUN = function(this_new_ecc) {
        this_new_ecc %>%
          finaldemand_aggregates(fd_sectors = fd_sectors,
                                 U = U_prime_colname,
                                 U_feed = U_feed_prime_colname,
                                 Y = Y_prime_colname,
                                 pattern_type = pattern_type,
                                 by = "Total",
                                 net_aggregate_demand = net_aggregate_demand,
                                 gross_aggregate_demand = gross_aggregate_demand)
      }) %>%
      # Transpose to pull EX.fd_net and EX.fd_gross to the top level with products and sectors beneath.
      purrr::transpose()

    # Create data frames that can be later unnested if needed.
    p_chop_df <- tibble::tibble(
      "{product_sector}" := p_aggregates[[aggregate_primary]] %>% names(),
      "{aggregate_primary}" := p_aggregates[[aggregate_primary]] %>% unname() %>% unlist()
    )
    net_fd_chop_df <- tibble::tibble(
      "{product_sector}" := fd_aggregates[[net_aggregate_demand]] %>% names(),
      "{net_aggregate_demand}" := fd_aggregates[[net_aggregate_demand]] %>% unname() %>% unlist()
    )
    gross_fd_chop_df <- tibble::tibble(
      "{product_sector}" := fd_aggregates[[gross_aggregate_demand]] %>% names(),
      "{gross_aggregate_demand}" := fd_aggregates[[gross_aggregate_demand]] %>% unname() %>% unlist()
    )
    # Join the data frames by the product_sector column.
    primary_net_gross <- p_chop_df %>%
      dplyr::full_join(gross_fd_chop_df, by = product_sector) %>%
      dplyr::full_join(net_fd_chop_df, by = product_sector)
  }

  if (calc_pfd_aggs) {
    out <- dplyr::full_join(ecc_primes, primary_net_gross, by = product_sector)
  } else {
    out <- ecc_primes
  }

  # Make a list and return it so that the data frame is nested
  # inside the column of the data frame.
  list(out) %>%
    magrittr::set_names(chop_df)
}
