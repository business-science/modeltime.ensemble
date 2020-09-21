#' Modeltime Forecast Helpers
#'
#' Used for low-level forecasting of modeltime, parnsip and workflow models.
#' These functions are not intended for user use.
#'
#' @inheritParams modeltime::modeltime_forecast
#' @param calibration_data Data that has been calibrated from a testing set
#' @param bind_actual Logical. Whether or not to skip rowwise binding of `actual_data``
#'
#' @return A tibble with forecast features
#'
#' @keywords internal
#'
#' @export
mdl_time_forecast.mdl_time_ensemble <- function(object, calibration_data, new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE, ...) {


    if (inherits(object, "mdl_time_ensemble_avg")) {

        return(

            mdl_time_forecast_ensemble_avg(
                object = object,
                calibration_data = calibration_data,
                new_data = new_data,
                h = h,
                actual_data = actual_data,
                bind_actual = bind_actual,
                ...
            )

        )

    } else if (inherits(object, "mdl_time_ensemble_stack")) {

    } else {
        rlang::abort("No method for this modeltime ensemble class.")
    }
}

mdl_time_forecast_ensemble_avg <- function(object, calibration_data, new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE, ...) {

    model_tbl <- object$model_tbl
    type      <- object$type

    # Get the raw forecast results for each of the models
    modeltime_fcast <- modeltime::modeltime_forecast(
        object        = model_tbl,
        new_data      = new_data,
        h             = h,
        actual_data   = actual_data,
        conf_interval = NULL,
        ...
    ) %>%
        dplyr::select(.key, .index, .value)

    # .key contains "actual"
    contains_actual <- "actual" %in% unique(modeltime_fcast$.key)
    if (contains_actual) {
        actual_data <- modeltime_fcast %>%
            dplyr::filter(.key == "actual")

        modeltime_fcast <- modeltime_fcast %>%
            dplyr::filter(.key != "actual")
    }

    # Calculate Ensemble
    if (type == "mean") {
        summary_fun <- mean
    } else {
        summary_fun <- median
    }

    modeltime_fcast <- modeltime_fcast %>%
        dplyr::group_by(.index) %>%
        dplyr::summarise(.value = summary_fun(.value)) %>%
        dplyr::ungroup() %>%
        tibble::add_column(.key = "prediction", .before = 1)

    # Recombine with actual
    if (contains_actual) {
        modeltime_fcast <- actual_data %>%
            dplyr::bind_rows(modeltime_fcast)
    }

    # FINALIZE
    ret <- modeltime_fcast %>%
        dplyr::mutate(.key = factor(.key, levels = c("actual", "prediction"))) %>%
        dplyr::arrange(.key, .index)

    return(ret)

}


