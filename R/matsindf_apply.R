#' Apply a function to a \code{matsindf} data frame (and more)
#'
#' Applies \code{FUN} to \code{.DF} or
#' performs the calculation specified by \code{FUN}
#' on numbers or matrices.
#' \code{FUN} must return a named list.
#'
#' If \code{...} are all named numbers or matrices
#' of the form \code{argname = m},
#' \code{.DF} is ignored, and
#' \code{m}s are passed to \code{FUN} by \code{argname}s.
#' The return value is a named list provided by \code{FUN}.
#'
#' If \code{...} are all lists of numbers or matrices
#' of the form \code{argname = l},
#' \code{.DF} is ignored, and
#' \code{FUN} is \code{Map}ped across the various \code{l}s
#' to obtain a list of named lists returned from \code{FUN}.
#' The return value is a data frame
#' whose rows are the named lists returned from \code{FUN} and
#' whose column names are the names of the list items returned from \code{FUN}.
#' The series of named lists are \code{rbind}-ed to create the output data frame.
#' Columns of \code{.DF} are not present in the return value.
#'
#' If \code{...} are all named character strings
#' of the form \code{argname = string},
#' \code{.DF} is required,
#' \code{string}s are expected to be column names in \code{.DF}, and
#' \code{argname}s are expected to be names of arguments to \code{FUN}.
#' The return value is \code{.DF} with additional columns (at right)
#' whose names are the names of list items returned from \code{FUN}.
#'
#' @param .DF the \code{matsindf} data frame
#' @param FUN the function to be applied to \code{.DF}
#' @param ... named arguments to be passed by name to \code{FUN}.
#'
#' @return a named list or a data frame. (See details.)
#'
#' @export
#'
#' @examples
matsindf_apply <- function(.DF = NULL, FUN, ...){
  dots <- list(...)
  all_dots_num  <- all(lapply(dots, FUN = is.numeric) %>% as.logical())
  all_dots_mats <- all(lapply(dots, FUN = is.matrix) %>% as.logical())
  all_dots_list <- all(lapply(dots, FUN = is.list) %>% as.logical())
  all_dots_char <- all(lapply(dots, FUN = is.character) %>% as.logical())

  if (is.null(.DF) & (all_dots_num | all_dots_mats)) {
    return(FUN(...))
  }
  if (is.null(.DF) & all_dots_list) {
    return(Map(f = FUN, ...) %>% bind_rows())
  }
  if (all_dots_char) {
    args <- lapply(dots, FUN = function(colname){
      return(.DF[[colname]])
    })
    return(matsindf_apply(FUN = FUN, ... = args) %>%
             bind_rows() %>%
             bindcols(.DF, .))
  }
  stop("unknown state in matsindf_apply. ... must be all same type, all numeric, all matrices, all lists, or all character")
}






