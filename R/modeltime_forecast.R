# ENSEMBLE FORECAST DISPATCH ----

# 1.0 AVERAGE ENSEMBLE ----

#' @export
#' @importFrom modeltime mdl_time_forecast
mdl_time_forecast.mdl_time_ensemble_avg <- function(object, calibration_data, new_data = NULL,
                                                    h = NULL, actual_data = NULL, bind_actual = TRUE, ...) {

    model_tbl <- object$model_tbl
    type      <- tolower(object$type)

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

    # Select correct summary function
    if (type == "mean") {
        summary_fun <- mean
    } else {
        summary_fun <- stats::median
    }

    # Calculate Ensemble
    modeltime_fcast <- modeltime_fcast %>%
        dplyr::group_by(.index) %>%
        dplyr::summarise(.value = summary_fun(.value, na.rm = TRUE)) %>%
        dplyr::ungroup() %>%
        tibble::add_column(.key = "prediction", .before = 1)

    # Recombine with actual
    if (contains_actual && bind_actual) {
        modeltime_fcast <- actual_data %>%
            dplyr::bind_rows(modeltime_fcast)
    }

    # FINALIZE
    ret <- modeltime_fcast %>%
        dplyr::mutate(.key = factor(.key, levels = c("actual", "prediction"))) %>%
        dplyr::arrange(.key, .index)

    return(ret)
}


# 2.0 WEIGHTED ENSEMBLE ----

#' @export
#' @importFrom modeltime mdl_time_forecast
mdl_time_forecast.mdl_time_ensemble_wt <- function(object, calibration_data, new_data = NULL,
                                                   h = NULL, actual_data = NULL, bind_actual = TRUE, ...) {

    model_tbl    <- object$model_tbl
    loadings_tbl <- object$loadings_info$loadings_tbl

    # Get the raw forecast results for each of the models
    modeltime_fcast <- modeltime::modeltime_forecast(
        object        = model_tbl,
        new_data      = new_data,
        h             = h,
        actual_data   = actual_data,
        conf_interval = NULL,
        ...
    ) %>%
        dplyr::select(.model_id, .key, .index, .value)

    # .key contains "actual"
    contains_actual <- "actual" %in% unique(modeltime_fcast$.key)
    if (contains_actual) {
        actual_data <- modeltime_fcast %>%
            dplyr::filter(.key == "actual") %>%
            dplyr::select(-.model_id)

        modeltime_fcast <- modeltime_fcast %>%
            dplyr::filter(.key != "actual")
    }


    # Calculate Ensemble
    modeltime_fcast <- modeltime_fcast %>%
        dplyr::left_join(loadings_tbl, by = ".model_id") %>%
        dplyr::mutate(.value = .value * .loadings) %>%
        dplyr::select(-.model_id, -.loadings) %>%
        dplyr::group_by(.index) %>%
        dplyr::summarise(.value = sum(.value, na.rm = TRUE)) %>%
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





