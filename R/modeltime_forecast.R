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

# 4.0 RECURSIVE ----

#' @export
mdl_time_forecast.recursive_ensemble <- function(object, calibration_data,
                                                 new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE,
                                                 keep_data = FALSE, arrange_index = FALSE, ...){

    if (inherits(object, "recursive")){
        ret <- mdl_time_forecast_recursive_ensemble(object = object, calibration_data = calibration_data,
                                                    new_data = new_data, h = h, actual_data = actual_data,
                                                    bind_actual = bind_actual, keep_data = keep_data,
                                                    arrange_index = arrange_index, ...)
    }

    if (inherits(object, "recursive_panel")){
        ret <- mdl_time_forecast_recursive_ensemble_panel(object = object, calibration_data = calibration_data,
                                                          new_data = new_data, h = h, actual_data = actual_data,
                                                          bind_actual = bind_actual, keep_data = keep_data,
                                                          arrange_index = arrange_index, ...)
    }

    return(ret)

}


mdl_time_forecast_recursive_ensemble <- function(object, calibration_data,
                                                 new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE,
                                                 keep_data = FALSE, arrange_index = FALSE, ...){
    # SETUP ----
    y_var <- object$spec$y_var

    class(object) <- class(object)[3:length(class(object))]

    .transform <- object$spec[["transform"]]
    train_tail <- object$spec$train_tail
    chunk_size <- object$spec$chunk_size

    idx_sets <- split(x = seq_len(nrow(new_data)),
                      f = (seq_len(nrow(new_data)) - 1) %/% chunk_size)

    # LOOP LOGIC ----
    .first_slice <- new_data %>%
        dplyr::slice_head(n = chunk_size)

    .forecasts <- modeltime::mdl_time_forecast(
        object,
        new_data = .first_slice,
        h = h,
        actual_data = actual_data,
        keep_data = keep_data,
        arrange_index = arrange_index,
        ...
    )

    .forecast_from_model <- .forecasts %>%
        dplyr::filter(.key == "prediction")

    new_data[idx_sets[[1]], y_var] <- .forecast_from_model$.value

    .temp_new_data <- dplyr::bind_rows(
        train_tail,
        new_data
    )

    n_train_tail <- nrow(train_tail)

    if (length(idx_sets) > 1){
        for (i in 2:length(idx_sets)) {

            transform_window_start <- min(idx_sets[[i]])
            transform_window_end   <- max(idx_sets[[i]]) + n_train_tail

            # .nth_slice <- .transform(.temp_new_data, nrow(new_data), i)
            .nth_slice <- .transform(.temp_new_data[transform_window_start:transform_window_end,], length(idx_sets[[i]]))

            # print(.nth_slice)

            .nth_forecast <- modeltime::mdl_time_forecast(
                object,
                new_data = .nth_slice,
                h = h,
                actual_data = actual_data,
                keep_data = keep_data,
                arrange_index = arrange_index,
                ...
            )

            # print(.nth_forecast)

            .nth_forecast_from_model <- .nth_forecast %>%
                dplyr::filter(.key == "prediction") %>%
                .[1,]

            .forecasts <- dplyr::bind_rows(
                .forecasts, .nth_forecast_from_model
            )

            new_data[idx_sets[[i]], y_var] <- .nth_forecast_from_model$.value
        }
    }

    return(.forecasts)
}



mdl_time_forecast_recursive_ensemble_panel <- function(object, calibration_data,
                                                       new_data = NULL, h = NULL, actual_data = NULL, bind_actual = TRUE,
                                                       keep_data = FALSE, arrange_index = FALSE, ...){
    # SETUP ----
    y_var <- object$spec$y_var

    class(object) <- class(object)[3:length(class(object))]

    .transform <- object$spec[["transform"]]
    train_tail <- object$spec$train_tail
    id         <- object$spec$id
    chunk_size <- object$spec$chunk_size

    .id <- dplyr::ensym(id)

    unique_id_new_data <- new_data %>% dplyr::select(!! .id) %>% unique() %>% dplyr::pull()

    unique_id_train_tail <- train_tail %>% dplyr::select(!! .id) %>% unique() %>% dplyr::pull()

    if (length(dplyr::setdiff(unique_id_train_tail, unique_id_new_data)) >= 1){
        train_tail <- train_tail %>% dplyr::filter(!! .id %in% unique_id_new_data)
    }

    n_groups <- dplyr::n_distinct(new_data[[id]])
    group_size <- max(table(new_data[[id]]))

    idx_sets <- split(x = seq_len(group_size),
                      f = (seq_len(group_size) - 1) %/% chunk_size)

    # LOOP LOGIC ----

    .preds <- tibble::tibble(.id = new_data %>% dplyr::pull(!! .id),
                             .pred = numeric(nrow(new_data))) %>%
        dplyr::group_by(.id) %>%
        dplyr::mutate(rowid.. = dplyr::row_number()) %>%
        dplyr::ungroup()

    .first_slice <- new_data %>%
        dplyr::group_by(!! .id) %>%
        dplyr::slice_head(n = chunk_size) %>%
        dplyr::ungroup()

    new_data <- new_data %>%
        dplyr::group_by(!! .id) %>%
        dplyr::mutate(rowid.. = dplyr::row_number()) %>%
        dplyr::ungroup()

    if ("rowid.." %in% names(.first_slice)) {
        .first_slice <- .first_slice %>% dplyr::select(-rowid..)
    }

    .forecasts <- modeltime::mdl_time_forecast(
            object,
            new_data = .first_slice,
            h = h,
            actual_data = actual_data,
            keep_data = keep_data,
            arrange_index = arrange_index,
            ...
        ) %>%
        dplyr::filter(!is.na(.value))

    .forecast_from_model <- .forecasts %>%
        dplyr::filter(.key == "prediction")

    .preds[.preds$rowid.. %in% idx_sets[[1]], 2] <- new_data[.preds$rowid.. %in% idx_sets[[1]], y_var] <- .forecast_from_model$.value

    .groups <- new_data %>%
        dplyr::group_by(!! .id) %>%
        dplyr::count(!! .id) %>%
        dim() %>%
        .[1]

    new_data_size <- nrow(.preds)/.groups

    .temp_new_data <- dplyr::bind_rows(
        train_tail,
        new_data
    )

    n_train_tail <- max(table(train_tail[[id]]))

    if (length(idx_sets) > 1){
        for (i in 2:length(idx_sets)) {

            transform_window_start <- min(idx_sets[[i]])
            transform_window_end   <- max(idx_sets[[i]]) + n_train_tail



            .nth_slice <- .transform(.temp_new_data %>%
                                         dplyr::group_by(!! .id) %>%
                                         dplyr::slice(transform_window_start:transform_window_end),
                                     idx_sets[[i]], id)

            # Fix - When ID is dummied
            if (!is.null(object$spec$remove_id)) {
                if (object$spec$remove_id) {
                    .nth_slice <- .nth_slice %>%
                        dplyr::select(-(!! .id))
                }
            }

            if ("rowid.." %in% names(.nth_slice)) {
                .nth_slice <- .nth_slice %>% dplyr::select(-rowid..)
            }

            .nth_slice <- .nth_slice[names(.first_slice)]

            .nth_forecast <- modeltime::mdl_time_forecast(
                object,
                new_data = .nth_slice,
                h = h,
                actual_data = actual_data,
                keep_data = keep_data,
                arrange_index = arrange_index,
                ...
            ) %>%
                dplyr::filter(!is.na(.value))

            .nth_forecast_from_model <- .nth_forecast %>%
                dplyr::filter(.key == "prediction")

            .forecasts <- dplyr::bind_rows(
                .forecasts, .nth_forecast_from_model
            )


            .preds[.preds$rowid.. %in% idx_sets[[i]], 2] <- .temp_new_data[.temp_new_data$rowid.. %in% idx_sets[[i]], y_var] <- .nth_forecast_from_model$.value
        }
    }

    return(.forecasts)
}
