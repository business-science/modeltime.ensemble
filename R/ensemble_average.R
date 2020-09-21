#' Creates an Average Esemble Model
#'
#' @param object A Modeltime Table
#' @param type Specify the type of average ("mean" or "median")
#'
#' @export
ensemble_average <- function(object, type = c("mean", "median")) {

    # Checks
    if (nrow(object) < 2) rlang::abort("An average ensemble requires two or more models in the Modeltime Table.")
    if (!type[[1]] %in% c("mean", "median")) rlang::abort("An average ensemble requires type to be either 'mean' or 'median'.")

    UseMethod("ensemble_average", object)
}

#' @export
ensemble_average.mdl_time_tbl <- function(object, type = c("mean", "median")) {

    # Create
    ensemble_average <- list(
        model_tbl = object,
        type      = type[[1]],
        n_models  = nrow(object)
    )

    ensemble_average <- structure(
        ensemble_average,
        class = c("mdl_time_ensemble_avg", "mdl_time_ensemble", class(ensemble_average))
    )

    return(ensemble_average)
}

#' @export
print.mdl_time_ensemble_avg <- function(x, ...) {

    rlang::inform(cli::rule("Modeltime Ensemble", width = min(65, cli::console_width())))

    if (x$type == "mean") {
        msg <- glue::glue("Average of {x$n_models} Models")
    } else {
        msg <- glue::glue("Median of {x$n_models} Models")
    }

    cat(msg)

    cli::cat_line()
    cli::cat_line()

    print(x$model_tbl)
}
