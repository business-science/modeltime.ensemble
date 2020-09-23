# ENSEMBLE AVERAGE ----

#' Creates an Ensemble Model using Mean/Median Averaging
#'
#' @param object A Modeltime Table
#' @param type Specify the type of average ("mean" or "median")
#'
#' @details
#'
#' The input to an `ensemble_average()` model is always a Modeltime Table,
#' which contains the models that you will ensemble.
#'
#' __Averaging Methods__
#'
#' The average method uses an un-weighted average using `type` of either:
#'
#' - `"mean"`: Performs averaging using `mean(x, na.rm = TRUE)` to aggregate each
#'   underlying models forecast at each timestamp
#' - `"median"`: Performs averaging using `stats::median(x, na.rm = TRUE)` to aggregate each
#'   underlying models forecast at each timestamp
#'
#' @examples
#' library(tidymodels)
#' library(modeltime)
#' library(modeltime.ensemble)
#' library(tidyverse)
#' library(timetk)
#'
#' # Make an ensemble from a Modeltime Table
#' ensemble_fit <- m750_models %>%
#'     ensemble_average(type = "mean")
#'
#' ensemble_fit
#'
#' # Forecast with the Ensemble
#' modeltime_table(
#'     ensemble_fit
#' ) %>%
#'     modeltime_forecast(
#'         new_data    = testing(m750_splits),
#'         actual_data = m750
#'     ) %>%
#'     plot_modeltime_forecast(
#'         .interactive = FALSE,
#'         .conf_interval_show = FALSE
#'     )
#'
#' @export
ensemble_average <- function(object, type = c("mean", "median")) {

    # Checks
    if (rlang::is_missing(object)) rlang::abort("'object' is missing. Please provide a Modeltime Table with multiple models.")
    if (!inherits(object, "mdl_time_tbl")) rlang::abort("object must be a Modeltime Table.")
    if (nrow(object) < 2) rlang::abort("An average ensemble requires two or more models in the Modeltime Table.")
    if (!tolower(type[[1]]) %in% c("mean", "median")) rlang::abort("An average ensemble requires type to be either 'mean' or 'median'.")

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
        class = c("mdl_time_ensemble_avg", "mdl_time_ensemble")
    )

    ensemble_average$desc <- get_model_description(ensemble_average)

    return(ensemble_average)
}

#' @export
print.mdl_time_ensemble_avg <- function(x, ...) {

    print(cli::rule("Modeltime Ensemble", width = min(65, cli::console_width())))

    if (x$type == "mean") {
        msg <- glue::glue("Ensemble of {x$n_models} Models (MEAN)")
    } else {
        msg <- glue::glue("Ensemble of {x$n_models} Models (MEDIAN)")
    }

    print(msg)

    cli::cat_line()

    print(x$model_tbl)

    invisible(x)
}
