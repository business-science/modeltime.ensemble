
#' @importFrom tibble type_sum
#' @export
type_sum.mdl_time_ensemble <- function(x) {
    cli::cli_text("ensemble [{x$n_models}]")
}


