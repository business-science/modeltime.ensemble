
#' @importFrom tibble type_sum
#' @export
type_sum.mdl_time_ensemble <- function(x) {
    glue::glue("ensemble [{x$n_models}]")
}


