# ENSEMBLE WEIGHTED ----

#' Creates a Weighted Ensemble Model
#'
#' Makes an ensemble by applying `loadings` to weight sub-model predictions
#'
#' @param object A Modeltime Table
#' @param loadings A vector of weights corresponding to the loadings
#' @param scale_loadings If TRUE, divides by the sum of the loadings
#'  to proportionally weight the submodels.
#'
#' @return A `mdl_time_ensemble` object.
#'
#' @details
#'
#' The input to an `ensemble_weighted()` model is always a Modeltime Table,
#' which contains the models that you will ensemble.
#'
#' __Weighting Method__
#'
#' The weighted method uses uses `loadings` by applying a
#' _loading x model prediction_ for each submodel.
#'
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
#'     ensemble_weighted(
#'         loadings = c(3, 3, 1),
#'         scale_loadings = TRUE
#'     )
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
ensemble_weighted <- function(object, loadings, scale_loadings = TRUE) {

    # Checks
    if (rlang::is_missing(object)) rlang::abort("'object' is missing. Please provide a Modeltime Table with multiple models.")
    if (!inherits(object, "mdl_time_tbl")) rlang::abort("'object' must be a Modeltime Table.")
    if (nrow(object) < 2) rlang::abort("An ensemble requires two or more models in the Modeltime Table.")
    if (!is.numeric(loadings)) rlang::abort("Loadings must be numeric.")
    if (nrow(object) != length(loadings)) rlang::abort("The length of 'loadings' must must match the number of rows in 'object'.")


    UseMethod("ensemble_weighted", object)
}

#' @export
ensemble_weighted.mdl_time_tbl <- function(object,
                                           loadings,
                                           scale_loadings = TRUE) {


    loadings_original <- loadings

    # Scale loadings
    if (scale_loadings) {
        loadings <- loadings / sum(loadings)
    }

    # Create loadings table
    loadings_tbl <- object %>%
        dplyr::select(.model_id) %>%
        dplyr::mutate(.loadings = loadings) %>%
        dplyr::filter(.loadings > 0)

    # Remove models with no loading
    model_tbl <- object %>%
        dplyr::filter(.model_id %in% loadings_tbl$.model_id)


    # Create Weighted Ensemble
    ensemble_weighted <- list(
        model_tbl      = model_tbl,
        parameters     = list(
            loadings       = loadings_original,
            scale_loadings = scale_loadings
        ),
        fit            = list(loadings_tbl  = loadings_tbl),
        n_models       = nrow(model_tbl)
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

    print(dplyr::left_join(x$model_tbl, x$fit$loadings_tbl, by = ".model_id"))

    invisible(x)
}





