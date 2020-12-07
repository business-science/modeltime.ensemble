# ENSEMBLE FORECAST DISPATCH ----

# 1.0 AVERAGE ENSEMBLE ----

#' @export
#' @importFrom modeltime mdl_time_forecast
mdl_time_forecast.mdl_time_ensemble_avg <- function(object, calibration_data,
                                                    new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE,
                                                    keep_data = FALSE, arrange_index = FALSE, ...) {

    # SUB-MODELS ----

    model_tbl <- object$model_tbl
    type      <- object$parameters$type

    # Get the raw forecast results for each of the models
    modeltime_fcast <- modeltime::modeltime_forecast(
        object        = model_tbl,
        new_data      = new_data,
        h             = h,
        actual_data   = actual_data,
        conf_interval = NULL,
        keep_data     = FALSE
    ) %>%
        dplyr::select(-.model_desc)

    # CONVERT CALIBRATION or H TO NEW DATA ----
    new_data <- convert_calib_h_to_new_data(new_data, h, calibration_data, actual_data)

    # For combining new data
    actual_data_unprocessed <- actual_data
    new_data_unprocessed    <- new_data

    # SEPARATE ACTUAL & FORECAST ----

    # .key contains "actual"
    contains_actual <- "actual" %in% unique(modeltime_fcast$.key)
    if (contains_actual) {
        actual_data <- modeltime_fcast %>%
            dplyr::filter(.key == "actual") %>%
            dplyr::select(-.model_id)

        modeltime_fcast <- modeltime_fcast %>%
            dplyr::filter(.key != "actual")
    }

    # ENSEMBLE CALCULATION ----

    # Select correct summary function
    if (type == "mean") {
        summary_fun <- mean
    } else {
        summary_fun <- stats::median
    }

    # Calculate Ensemble
    modeltime_fcast <- modeltime_fcast %>%

        # Add row id's
        dplyr::group_by(.model_id) %>%
        dplyr::group_split() %>%
        purrr::map(.f = function(df) {
            df %>%
                tibble::rowid_to_column(var = ".row_id")
        }) %>%
        dplyr::bind_rows() %>%

        # Pivot to Wide
        tidyr::pivot_wider(
            names_from   = .model_id,
            names_prefix = ".model_id_",
            values_from  = .value
        ) %>%

        dplyr::rowwise() %>%
        dplyr::mutate(.value = summary_fun( dplyr::c_across( dplyr::starts_with(".model_id_") ), na.rm = FALSE )) %>%
        dplyr::ungroup() %>%

        dplyr::select(-dplyr::starts_with(".model_id_"), -.row_id)

    # FINALIZE ----

    # Recombine with actual
    if (contains_actual && bind_actual) {
        modeltime_fcast <- actual_data %>%
            dplyr::bind_rows(modeltime_fcast)
    }

    ret <- finalize_mdl_time_forecast(modeltime_fcast, keep_data,
                                      actual_data_unprocessed, new_data_unprocessed,
                                      contains_actual, bind_actual,
                                      arrange_index)

    return(ret)
}


# 2.0 WEIGHTED ENSEMBLE ----

#' @export
mdl_time_forecast.mdl_time_ensemble_wt <- function(object, calibration_data,
                                                   new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE,
                                                   keep_data = FALSE, arrange_index = FALSE, ...) {

    # SUB-MODELS -----

    model_tbl    <- object$model_tbl
    loadings_tbl <- object$fit$loadings_tbl

    # Get the raw forecast results for each of the sub-models
    modeltime_fcast <- modeltime::modeltime_forecast(
        object        = model_tbl,
        new_data      = new_data,
        h             = h,
        actual_data   = actual_data,
        conf_interval = NULL,
        keep_data     = FALSE
    ) %>%
        dplyr::select(-.model_desc)


    # CONVERT CALIBRATION or H TO NEW DATA ----
    new_data <- convert_calib_h_to_new_data(new_data, h, calibration_data, actual_data)

    # For combining new data
    actual_data_unprocessed <- actual_data
    new_data_unprocessed    <- new_data

    # SEPARATE ACTUAL & FORECAST ----

    # .key contains "actual"
    contains_actual <- "actual" %in% unique(modeltime_fcast$.key)
    if (contains_actual) {
        actual_data <- modeltime_fcast %>%
            dplyr::filter(.key == "actual") %>%
            dplyr::select(-.model_id)

        modeltime_fcast <- modeltime_fcast %>%
            dplyr::filter(.key != "actual")
    }

    # ENSEMBLE CALCULATION -----

    # Calculate Ensemble
    modeltime_fcast <- modeltime_fcast %>%

        # Add row id's
        dplyr::group_by(.model_id) %>%
        dplyr::group_split() %>%
        purrr::map(.f = function(df) {
            df %>%
                tibble::rowid_to_column(var = ".row_id")
        }) %>%
        dplyr::bind_rows() %>%

        # Apply loadings
        dplyr::left_join(loadings_tbl, by = ".model_id") %>%
        dplyr::mutate(.value = .value * .loadings) %>%
        dplyr::select(-.loadings) %>%

        # Pivot to Wide
        tidyr::pivot_wider(
            names_from   = .model_id,
            names_prefix = ".model_id_",
            values_from  = .value
        ) %>%

        dplyr::rowwise() %>%
        dplyr::mutate(.value = sum( dplyr::c_across( dplyr::starts_with(".model_id_") ), na.rm = FALSE )) %>%
        dplyr::ungroup() %>%

        dplyr::select(-dplyr::starts_with(".model_id_"), -.row_id)


    # FINALIZE -----

    # Recombine with actual
    if (contains_actual && bind_actual) {
        modeltime_fcast <- actual_data %>%
            dplyr::bind_rows(modeltime_fcast)
    }

    ret <- finalize_mdl_time_forecast(modeltime_fcast, keep_data,
                                      actual_data_unprocessed, new_data_unprocessed,
                                      contains_actual, bind_actual,
                                      arrange_index)

    return(ret)

}


# 3.0 MODEL SPEC ENSEMBLE ----

#' @export
mdl_time_forecast.mdl_time_ensemble_model_spec <- function(object, calibration_data,
                                                           new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE,
                                                           keep_data = FALSE, arrange_index = FALSE, ...) {

    # SUB-MODELS ----

    model_tbl <- object$model_tbl
    wflw_fit  <- object$fit$fit

    # Get the raw forecast results for each of the models
    modeltime_fcast <- modeltime::modeltime_forecast(
        object        = model_tbl,
        new_data      = new_data,
        h             = h,
        actual_data   = actual_data,
        conf_interval = NULL,
        keep_data     = FALSE
    ) %>%
        dplyr::select(-.model_desc)

    # CONVERT CALIBRATION or H TO NEW DATA ----
    new_data <- convert_calib_h_to_new_data(new_data, h, calibration_data, actual_data)

    # For combining new data
    actual_data_unprocessed <- actual_data
    new_data_unprocessed    <- new_data

    # SEPARATE ACTUAL & FORECAST ----

    # .key contains "actual"
    contains_actual <- "actual" %in% unique(modeltime_fcast$.key)
    if (contains_actual) {
        actual_data <- modeltime_fcast %>%
            dplyr::filter(.key == "actual") %>%
            dplyr::select(-.model_id)

        modeltime_fcast <- modeltime_fcast %>%
            dplyr::filter(.key != "actual")
    }

    # ENSEMBLE CALCULATION ----

    # Calculate Ensemble
    data_prepared_tbl <- modeltime_fcast %>%

        # Add row id's
        dplyr::group_by(.model_id) %>%
        dplyr::group_split() %>%
        purrr::map(.f = function(df) {
            df %>%
                tibble::rowid_to_column(var = ".row_id")
        }) %>%
        dplyr::bind_rows() %>%

        # Pivot to Wide
        tidyr::pivot_wider(
            names_from   = .model_id,
            names_prefix = ".model_id_",
            values_from  = .value
        ) %>%

        dplyr::select(-.row_id)

    pred_vec <- stats::predict(wflw_fit, new_data = data_prepared_tbl) %>%
        dplyr::pull(.pred)

    modeltime_fcast <- data_prepared_tbl %>%
        dplyr::select(.key, .index) %>%
        dplyr::mutate(.value = pred_vec)


    # FINALIZE ----

    # Recombine with actual
    if (contains_actual && bind_actual) {
        modeltime_fcast <- actual_data %>%
            dplyr::bind_rows(modeltime_fcast)
    }

    ret <- finalize_mdl_time_forecast(modeltime_fcast, keep_data,
                                      actual_data_unprocessed, new_data_unprocessed,
                                      contains_actual, bind_actual,
                                      arrange_index)

    return(ret)


}

# UTILITIES ----

convert_calib_h_to_new_data <- function(new_data, h, calibration_data, actual_data, ...) {

    # If no 'new_data', forecast 'calibration_data'
    if (is.null(new_data) && is.null(h)) {
        if (is.data.frame(calibration_data)) {
            new_data <- calibration_data
        } else if (is.data.frame(actual_data)) {
            new_data <- actual_data
        } else {
            rlang::abort("Forecast requires 'new_data', 'calibration_data', or 'actual_data'.")
        }
    }

    # Convert 'h' to 'new_data'
    if (!is.null(h)) {
        if (is.data.frame(calibration_data)) {
            tryCatch({
                # Suppress date selection
                suppressMessages(new_data <- timetk::future_frame(calibration_data, .length_out = h, ...))
            }, error = function(e) {
                rlang::abort("Attempt to extend '.calibration_data' into the future using 'h' has failed.")
            })
        } else if (is.data.frame(actual_data)) {
            tryCatch({
                # Suppress date selection
                suppressMessages(new_data <- timetk::future_frame(actual_data, .length_out = h, ...))
            }, error = function(e) {
                rlang::abort("Attempt to extend 'actual_data' into the future using 'h' has failed.")
            })
        } else {
            rlang::abort("Forecast requires 'new_data', '.calibration_data', or 'actual_data'.")
        }
    }

    return(new_data)

}

finalize_mdl_time_forecast <- function(modeltime_fcast, keep_data,
                                       actual_data_unprocessed, new_data_unprocessed,
                                       contains_actual, bind_actual,
                                       arrange_index) {


    ret <- modeltime_fcast %>%
        dplyr::select(.key, .index, .value) %>%
        dplyr::mutate(.key = factor(.key, levels = c("actual", "prediction")))

    # Keep Data
    act_tbl  <- NULL
    pred_tbl <- NULL
    if (keep_data) {

        if (contains_actual && bind_actual) {
            act_tbl <- ret %>%
                dplyr::filter(.key == "actual") %>%
                dplyr::bind_cols(actual_data_unprocessed)
        }

        pred_tbl <- ret %>%
            dplyr::filter(.key == "prediction") %>%
            dplyr::bind_cols(new_data_unprocessed)

        ret <- dplyr::bind_rows(act_tbl, pred_tbl)

    }

    if (arrange_index) {
        ret <- ret %>%
            dplyr::arrange(.key, .index)
    }

    return(ret)

}


