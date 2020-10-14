# RESAMPLE ACCURACY ----

#' Calculate Accuracy Metrics from Modeltime Resamples
#'
#' This is a wrapper for `yardstick` that simplifies time
#' series regression accuracy metric calculations from
#' a Modeltime Table that has been resampled and fitted using
#' [modeltime_fit_resamples()].
#'
#' @inheritParams modeltime::modeltime_accuracy
#' @param object a Modeltime Table with a column '.resample_results' (the output of [modeltime_fit_resamples()])
#' @param summary_fns One or more functions that is passed to `dplyr::across(.fns)`.
#'  Possible values are:
#'  * NULL, to returns the resamples untransformed.
#'  * A function, e.g. mean.
#'  * A purrr-style lambda, e.g. ~ mean(.x, na.rm = TRUE)
#'  * A list of functions/lambdas, e.g. list(mean = mean, sd = sd)
#'
#' @details
#' Returns the _average_ accuracy metrics for each resample prediction.
#'
#' @examples
#' library(tidymodels)
#' library(modeltime)
#' library(modeltime.ensemble)
#' library(timetk)
#' library(tidyverse)
#'
#' resamples_tscv <- training(m750_splits) %>%
#'     time_series_cv(
#'         assess      = "2 years",
#'         initial     = "5 years",
#'         skip        = "2 years",
#'         slice_limit = 1
#'     )
#'
#' m750_models_resample <- m750_models %>%
#'     modeltime_fit_resamples(
#'         resamples = resamples_tscv,
#'         control   = control_resamples(verbose = TRUE)
#'     )
#'
#' m750_models_resample
#'
#' # Average
#' m750_models_resample %>%
#'     modeltime_resample_accuracy() %>%
#'     table_modeltime_accuracy(.interactive = FALSE)
#'
#' # Mean and Standard Deviation
#' m750_models_resample %>%
#'     modeltime_resample_accuracy(
#'         summary_fns = list(mean = mean, sd = sd)
#'     ) %>%
#'     table_modeltime_accuracy(.interactive = FALSE)
#'
#' @export
modeltime_resample_accuracy <- function(object, summary_fns = mean, metric_set = default_forecast_accuracy_metric_set()) {

    # Checks
    if (!inherits(object, "data.frame")) rlang::abort("object must be a data.frame")
    if (!".resample_results" %in% names(object)) rlang::abort("object must contain a column, '.resample_results'. Try using `modeltime_fit_resamples()` first.")

    # Unnest resamples column
    predictions_tbl <- unnest_resamples(object)

    # Target Variable is the name in the data
    target_text <- names(predictions_tbl) %>% utils::tail(1)
    target_var  <- rlang::sym(target_text)

    ret <- predictions_tbl %>%
        dplyr::mutate(.type = "Resamples") %>%
        dplyr::group_by(.model_id, .model_desc, .resample_id, .type) %>%
        modeltime::summarize_accuracy_metrics(!! target_var, .pred, metric_set = metric_set) %>%
        dplyr::select(-.resample_id) %>%
        dplyr::group_by(.model_id, .model_desc, .type) %>%
        dplyr::mutate(n = dplyr::n()) %>%
        dplyr::group_by(.model_id, .model_desc, .type, n) %>%
        dplyr::summarise(
            dplyr::across(.fns = summary_fns),
            .groups = "drop"
        ) %>%
        dplyr::ungroup()

    return(ret)


}




