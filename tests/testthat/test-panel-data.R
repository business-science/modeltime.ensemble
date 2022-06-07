context("PANEL DATA")

# SETUP ----

m4_monthly_jumbled <- m4_monthly %>%
    arrange(desc(date))

data_set <- m4_monthly_jumbled

recipe_spec <- recipe(value ~ date + id, data_set) %>%
    step_mutate(date_num = as.numeric(date)) %>%
    step_mutate(month_lbl = lubridate::month(date, label = TRUE)) %>%
    step_dummy(all_nominal(), one_hot = TRUE)

set.seed(123)
wflw_fit_prophet_boost <- workflow() %>%
    add_model(
        prophet_boost(
            seasonality_yearly = F,
            seasonality_weekly = F,
            seasonality_daily  = F
        ) %>%
            set_engine("prophet_xgboost")) %>%
    add_recipe(recipe_spec) %>%
    fit(data_set)

set.seed(123)
wflw_fit_xgb <- workflow() %>%
    add_model(boost_tree() %>% set_engine("xgboost")) %>%
    add_recipe(recipe_spec %>% step_rm(date)) %>%
    fit(data_set)

# AVERAGE ENSEMBLE ----

test_that("ensemble_average(): Forecast Jumbled", {

    submodel_tbl <- modeltime_table(
        wflw_fit_prophet_boost,
        wflw_fit_xgb
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
    expect_true(all(c(".type", ".calibration_data") %in% names(calibration_tbl)))
    expect_equal(nrow(data_set), calibration_tbl %>% pluck(".calibration_data", 1) %>% nrow())

    # Accuracy
    accuracy_tbl <- calibration_tbl %>%
        modeltime_accuracy()

    expect_true(all(!is.na(accuracy_tbl$mae)))
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
        filter(.key == "actual")

    expect_equal(nrow(actual_tbl), nrow(data_set))
    expect_equal(actual_tbl$.value, actual_tbl$value)

    # * Test Ensemble ----
    ensemble_tbl <- forecast_tbl %>%
        filter(.key == "prediction")

    expect_equal(nrow(ensemble_tbl), nrow(data_set))
    expect_equal(ensemble_tbl$.index, ensemble_tbl$date)

})

# WEIGHTED ENSEMBLE ----

test_that("ensemble_weighted(): Forecast Jumbled", {

    loadings <- c(3, 1)

    model_tbl <- modeltime_table(
        wflw_fit_prophet_boost,
        wflw_fit_xgb
    ) %>%
        ensemble_weighted(loadings) %>%
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
    expect_true(all(c(".type", ".calibration_data") %in% names(calibration_tbl)))
    expect_equal(nrow(data_set), calibration_tbl %>% pluck(".calibration_data", 1) %>% nrow())

    # Accuracy
    accuracy_tbl <- calibration_tbl %>%
        modeltime_accuracy()

    expect_true(all(!is.na(accuracy_tbl$mae)))
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
        filter(.key == "actual")

    expect_equal(nrow(actual_tbl), nrow(data_set))
    expect_equal(actual_tbl$.value, actual_tbl$value)

    # * Test Ensemble ----
    ensemble_tbl <- forecast_tbl %>%
        filter(.key == "prediction")

    expect_equal(nrow(ensemble_tbl), nrow(data_set))
    expect_equal(ensemble_tbl$.index, ensemble_tbl$date)

})

# STACKED ENSEMBLE ----

test_that("ensemble_model_spec(): Forecast Jumbled", {

    resamples_tscv <- data_set %>%
        time_series_cv(assess = "2 years", initial = "5 years", skip = "2 years", slice_limit = 2)

    resample_tscv <- modeltime_table(
        wflw_fit_prophet_boost,
        wflw_fit_xgb
    ) %>%
        modeltime_fit_resamples(resamples_tscv, control = control_resamples(verbose = F))

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
    expect_true(all(c(".type", ".calibration_data") %in% names(calibration_tbl)))
    expect_equal(nrow(data_set), calibration_tbl %>% pluck(".calibration_data", 1) %>% nrow())

    # Accuracy
    accuracy_tbl <- calibration_tbl %>%
        modeltime_accuracy()

    expect_true(all(!is.na(accuracy_tbl$mae)))
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
        filter(.key == "actual")

    expect_equal(nrow(actual_tbl), nrow(data_set))
    expect_equal(actual_tbl$.value, actual_tbl$value)

    # * Test Ensemble ----
    ensemble_tbl <- forecast_tbl %>%
        filter(.key == "prediction")

    expect_equal(nrow(ensemble_tbl), nrow(data_set))
    expect_equal(ensemble_tbl$.index, ensemble_tbl$date)

})


