#' Succinct summary of Modeltime Tables
#'
#' `type_sum` controls how objects are shown when inside tibble
#'  columns.
#' @param x	A `mdl_time_ensemble` object to summarise.
#' @return A character value.
#' @importFrom tibble type_sum
#' @export
type_sum.mdl_time_ensemble <- function(x) {
    glue::glue("ensemble [{x$n_models}]")
}
