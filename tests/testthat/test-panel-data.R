context("PANEL DATA")

# NOTE: Do not fit models at top-level. CRAN sources test files and any error
# outside of `test_that()` aborts the entire test run.

get_panel_data <- function() {
    data_env <- new.env(parent = emptyenv())
    utils::data("m4_monthly", package = "timetk", envir = data_env)
    dplyr::arrange(data_env$m4_monthly, dplyr::desc(date))
}

get_panel_recipe <- function(data_set) {
    recipes::recipe(value ~ date + id, data_set) %>%
        recipes::step_mutate(date_num = as.numeric(date)) %>%
        recipes::step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
        recipes::step_dummy(recipes::all_nominal(), one_hot = TRUE)
}

get_panel_fits <- local({
    cache <- NULL

    function() {
        if (!is.null(cache)) return(cache)

        testthat::skip_if_not_installed("xgboost")
        testthat::skip_if_not_installed("prophet")

        # modeltime < 1.3.3 can error in prophet_xgboost panel workflows due to
        # xgboost objective handling; require the fixed version on CRAN.
        if (utils::packageVersion("modeltime") < "1.3.3") {
            testthat::skip("Requires modeltime >= 1.3.3 for prophet_xgboost panel support.")
        }

        data_set <- get_panel_data()
        recipe_spec <- get_panel_recipe(data_set)

        set.seed(123)
        wflw_fit_prophet_boost <- workflow() %>%
            add_model(
                prophet_boost(
                    seasonality_yearly = FALSE,
                    seasonality_weekly = FALSE,
                    seasonality_daily  = FALSE
                ) %>%
                    set_engine("prophet_xgboost")
            ) %>%
            add_recipe(recipe_spec) %>%
            fit(data_set)

        set.seed(123)
        wflw_fit_xgb <- workflow() %>%
            add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
            add_recipe(recipe_spec %>% recipes::step_rm(date)) %>%
            fit(data_set)

        cache <<- list(
            data_set = data_set,
            wflw_fit_prophet_boost = wflw_fit_prophet_boost,
            wflw_fit_xgb = wflw_fit_xgb
        )

        cache
    }
})

# AVERAGE ENSEMBLE ----

test_that("ensemble_average(): Forecast Jumbled", {

    fits <- get_panel_fits()
    data_set <- fits$data_set

    submodel_tbl <- modeltime_table(
        fits$wflw_fit_prophet_boost,
        fits$wflw_fit_xgb
    )

    # submodel_tbl %>%
    #     modeltime_calibrate(data_set) %>%
    #     modeltime_accuracy()

    model_tbl <- submodel_tbl %>%
        ensemble_average() %>%
        modeltime_table()

    # Forecast
    fcast <- model_tbl %>%
        modeltime_forecast(data_set)

    expect_equal(nrow(fcast), nrow(data_set))
    expect_equal(fcast$.index, data_set$date)

    # Calibration
    calibration_tbl <- model_tbl %>%
        modeltime_calibrate(data_set)

    expect_equal(calibration_tbl$.type, c("Test"))
    expect_contains(names(calibration_tbl), c(".type", ".calibration_data"))
    expect_equal(nrow(data_set), calibration_tbl %>% purrr::pluck(".calibration_data", 1) %>% nrow())

    # Accuracy
    accuracy_tbl <- calibration_tbl %>%
        modeltime_accuracy()

    expect_false(anyNA(accuracy_tbl$mae))
    expect_true(all(is.double(accuracy_tbl$mae)))

    expect_true(accuracy_tbl$mae < 500)

    # * Forecast ----
    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data       = data_set,
            actual_data    = data_set,
            keep_data      = TRUE,
            arrange_index  = FALSE
        )
    # forecast_tbl %>% group_by(id) %>% plot_modeltime_forecast()

    # * Test Actual ----
    actual_tbl <- forecast_tbl %>%
        dplyr::filter(.key == "actual")

    expect_equal(nrow(actual_tbl), nrow(data_set))
    expect_equal(actual_tbl$.value, actual_tbl$value)

    # * Test Ensemble ----
    ensemble_tbl <- forecast_tbl %>%
        dplyr::filter(.key == "prediction")

    expect_equal(nrow(ensemble_tbl), nrow(data_set))
    expect_equal(ensemble_tbl$.index, ensemble_tbl$date)

})

# WEIGHTED ENSEMBLE ----

test_that("ensemble_weighted(): Forecast Jumbled", {

    loadings <- c(3, 1)

    model_tbl <- modeltime_table(
        get_panel_fits()$wflw_fit_prophet_boost,
        get_panel_fits()$wflw_fit_xgb
    ) %>%
        ensemble_weighted(loadings) %>%
        modeltime_table()

    # Forecast
    data_set <- get_panel_fits()$data_set
    fcast <- model_tbl %>%
        modeltime_forecast(data_set)

    expect_equal(nrow(fcast), nrow(data_set))
    expect_equal(fcast$.index, data_set$date)

    # Calibration
    calibration_tbl <- model_tbl %>%
        modeltime_calibrate(data_set)

    expect_equal(calibration_tbl$.type, c("Test"))
    expect_contains(names(calibration_tbl), c(".type", ".calibration_data"))
    expect_equal(nrow(data_set), calibration_tbl %>% purrr::pluck(".calibration_data", 1) %>% nrow())

    # Accuracy
    accuracy_tbl <- calibration_tbl %>%
        modeltime_accuracy()

    expect_false(anyNA(accuracy_tbl$mae))
    expect_true(all(is.double(accuracy_tbl$mae)))
    expect_true(accuracy_tbl$mae < 400)

    # * Forecast ----
    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data       = data_set,
            actual_data    = data_set,
            keep_data      = TRUE,
            arrange_index  = FALSE
        )
    # forecast_tbl %>% group_by(id) %>% plot_modeltime_forecast()

    # * Test Actual ----
    actual_tbl <- forecast_tbl %>%
        dplyr::filter(.key == "actual")

    expect_equal(nrow(actual_tbl), nrow(data_set))
    expect_equal(actual_tbl$.value, actual_tbl$value)

    # * Test Ensemble ----
    ensemble_tbl <- forecast_tbl %>%
        dplyr::filter(.key == "prediction")

    expect_equal(nrow(ensemble_tbl), nrow(data_set))
    expect_equal(ensemble_tbl$.index, ensemble_tbl$date)

})

# STACKED ENSEMBLE ----

test_that("ensemble_model_spec(): Forecast Jumbled", {

    fits <- get_panel_fits()
    data_set <- fits$data_set

    resamples_tscv <- data_set %>%
        time_series_cv(assess = "2 years", initial = "5 years", skip = "2 years", slice_limit = 2)

    resample_tscv <- modeltime_table(
        fits$wflw_fit_prophet_boost,
        fits$wflw_fit_xgb
    ) %>%
        modeltime_fit_resamples(resamples_tscv, control = control_resamples(verbose = FALSE))

    ensemble_fit <- resample_tscv %>%
        ensemble_model_spec(
            model_spec = linear_reg() %>% set_engine("lm"),
            grid       = 3,
            control    = control_grid(verbose = FALSE)
        )

    model_tbl <- modeltime_table(
        ensemble_fit
    )

    # Forecast
    fcast <- model_tbl %>%
        modeltime_forecast(data_set)

    expect_equal(nrow(fcast), nrow(data_set))
    expect_equal(fcast$.index, data_set$date)

    # Calibration
    calibration_tbl <- model_tbl %>%
        modeltime_calibrate(data_set, quiet = FALSE)

    expect_equal(calibration_tbl$.type, c("Test"))
    expect_contains(names(calibration_tbl), c(".type", ".calibration_data"))
    expect_equal(nrow(data_set), calibration_tbl %>% purrr::pluck(".calibration_data", 1) %>% nrow())

    # Accuracy
    accuracy_tbl <- calibration_tbl %>%
        modeltime_accuracy()

    expect_false(anyNA(accuracy_tbl$mae))
    expect_true(all(is.double(accuracy_tbl$mae)))

    # * Forecast ----
    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data       = data_set,
            actual_data    = data_set,
            keep_data      = TRUE,
            arrange_index  = FALSE
        )

    # * Test Actual ----
    actual_tbl <- forecast_tbl %>%
        dplyr::filter(.key == "actual")

    expect_equal(nrow(actual_tbl), nrow(data_set))
    expect_equal(actual_tbl$.value, actual_tbl$value)

    # * Test Ensemble ----
    ensemble_tbl <- forecast_tbl %>%
        dplyr::filter(.key == "prediction")

    expect_equal(nrow(ensemble_tbl), nrow(data_set))
    expect_equal(ensemble_tbl$.index, ensemble_tbl$date)

})
