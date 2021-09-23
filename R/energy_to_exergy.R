#' Extend an ECC in PSUT format from energy to exergy
#'
#' An energy conversion chain can be represented in energy or exergy quantifications of energy.
#' This function moves from an energy quantification to an exergy quantification,
#' given the matrices for the energy quantification and
#' phi (exergy-to-energy ratio) vectors.
#'
#' Internally, this function uses `matsindf::apply`, so
#' the ECC matrices can be provided
#' as individual matrices
#' or in a data frame
#' (in which case the arguments should
#' give the string names of columns in the `.sutmats` data frame).
#'
#'
#' @param .sutmats
#'
#' @return
#' @export
#'
#' @examples
extend_to_exergy <- function(.sutmats) {

}
