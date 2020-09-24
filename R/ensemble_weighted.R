# ENSEMBLE WEIGHTED ----

#' Creates a Weighted Ensemble Model
#'
#' @param object A Modeltime Table
#' @param loadings Either "auto" or a vector of weights corresponding to the loadings
#' @param resamples NULL.
#'
#' @details
#'
#' The input to an `ensemble_weighted()` model is always a Modeltime Table,
#' which contains the models that you will ensemble.
#'
#' __Weighting Methods__
#'
#' The average method uses an un-weighted average using `loadings` of either:
#'
#' - `"auto"`: Performs weighting using Penalized Regression (using and Elastic Net via `glmnet`)
#' - `<vector>`:
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
#'     ensemble_weighted(type = "mean")
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
ensemble_weighted <- function(object, loadings = "auto", resamples = NULL) {

    # Checks
    if (rlang::is_missing(object)) rlang::abort("'object' is missing. Please provide a Modeltime Table with multiple models.")
    if (!inherits(object, "mdl_time_tbl")) rlang::abort("'object' must be a Modeltime Table.")
    if (nrow(object) < 2) rlang::abort("An average ensemble requires two or more models in the Modeltime Table.")
    if (is.numeric(loadings)) {
        if (nrow(object) != length(loadings)) rlang::abort("The length of 'loadings' must must match the number of rows in 'object'.")
    } else {
        if (tolower(loadings) != 'auto') {
            rlang::warn("'loadings' is invalid. Setting to loadings = 'auto'.")
            loadings <- "auto"
        }
    }
    if (tolower(loadings) == "auto" && is.null(resamples)) rlang::abort("'resamples' must be provided to use the loadings = 'auto'.")


    UseMethod("ensemble_weighted", object)
}

#' @export
ensemble_weighted.mdl_time_tbl <- function(object, loadings = "auto", resamples = NULL) {

    # Calculate the loadings
    if (is.numeric(loadings)) {
        loadings_scaled <- loadings / sum(loadings)

    } else {
        # Placeholder for 'auto' using glmnet
        message("Calculating weights using 'auto'.")

        stop("Not ready yet...")

        # loadings_scaled <- calculate_loadings_from_resamples(object, resamples)

        # loadings_scaled <- rep(1, nrow(object)) / nrow(object)
    }

    # Create loadings table
    loadings_tbl <- object %>%
        dplyr::select(.model_id) %>%
        dplyr::mutate(.loadings = loadings_scaled)

    # Create Weighted Ensemble
    ensemble_weighted <- list(
        model_tbl      = object,
        loadings_info  = list(
            loadings_user = loadings,
            loadings_tbl  = loadings_tbl
        ),
        n_models       = nrow(object)
    )

    ensemble_weighted <- structure(
        ensemble_weighted,
        class = c("mdl_time_ensemble_wt", "mdl_time_ensemble")
    )

    ensemble_weighted$desc <- get_model_description(ensemble_weighted)

    return(ensemble_weighted)
}

#' @export
print.mdl_time_ensemble_wt <- function(x, ...) {

    print(cli::rule("Modeltime Ensemble", width = min(65, cli::console_width())))

    msg <- glue::glue("Ensemble of {x$n_models} Models (WEIGHTED)")

    print(msg)

    cli::cat_line()

    print(dplyr::left_join(x$model_tbl, x$loadings_info$loadings_tbl, by = ".model_id"))

    invisible(x)
}





