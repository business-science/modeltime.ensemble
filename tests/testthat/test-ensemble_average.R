context("USED TO SET UP MODELS FOR TESTS")
context("TEST: ensemble_average()")

library(testthat)

# USED FOR TESTS ----
wflw_fit_arima <- workflow() %>%
    add_model(
        spec = arima_reg(seasonal_period = 12) %>% set_engine("auto_arima")
    ) %>%
    add_recipe(
        recipe = recipe(value ~ date, data = training(m750_splits))
    ) %>%
    fit(training(m750_splits))

wflw_fit_prophet <- workflow() %>%
    add_model(
        spec = prophet_reg() %>% set_engine("prophet")
    ) %>%
    add_recipe(
        recipe = recipe(value ~ date, data = training(m750_splits))
    ) %>%
    fit(training(m750_splits))

rec_glmnet <- recipe(value ~ date, data = training(m750_splits)) %>%
    step_timeseries_signature(date) %>%
    step_rm(matches("(iso$)|(xts$)|(am.pm)|(hour$)|(minute)|(second)")) %>%
    step_zv(all_predictors()) %>%
    step_normalize(all_numeric_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
    step_rm(date)

# rec_glmnet %>% prep() %>% juice() %>% glimpse()


wflw_fit_glmnet <- workflow() %>%
    add_model(
        spec = linear_reg(penalty = 0.1) %>% set_engine("glmnet")
    ) %>%
    add_recipe(
        recipe = rec_glmnet
    ) %>%
    fit(training(m750_splits))

m750_models_2 <- modeltime_table(
    wflw_fit_arima,
    wflw_fit_prophet,
    wflw_fit_glmnet
)


# TEST ENSEMBLE AVERAGE ----

# Median ----
test_that("ensemble_average(type = 'median')", {

    skip_on_cran()

    ensemble_fit_median <- m750_models_2 %>%
        ensemble_average(type = "median")

    # Structure
    expect_s3_class(ensemble_fit_median, "mdl_time_ensemble")
    expect_s3_class(ensemble_fit_median, "mdl_time_ensemble_avg")

    expect_s3_class(ensemble_fit_median$model_tbl, "mdl_time_tbl")
    expect_equal(ensemble_fit_median$parameters$type, "median")
    expect_equal(ensemble_fit_median$n_models, 3)
    expect_equal(ensemble_fit_median$desc, "ENSEMBLE (MEDIAN): 3 MODELS")

    # Print
    expect_equal(print(ensemble_fit_median), ensemble_fit_median)

    # Modeltime Table
    expect_equal(
        modeltime_table(ensemble_fit_median) %>% pull(.model_desc),
        "ENSEMBLE (MEDIAN): 3 MODELS"
    )

    # Calibration
    calibration_tbl <- ensemble_fit_median %>%
        modeltime_calibrate(testing(m750_splits))

    expect_false(is.na(calibration_tbl$.type))

    # Accuracy
    accuracy_tbl <- calibration_tbl %>% modeltime_accuracy()

    expect_false(is.na(accuracy_tbl$mae))
    expect_true(accuracy_tbl$mae < 300)

    # Forecast
    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data    = testing(m750_splits),
            actual_data = m750
        )

    n_actual <- nrow(m750)

    expect_equal(nrow(forecast_tbl), 24 + n_actual)
    expect_equal(ncol(forecast_tbl), 7)

    # Forecast - Test Keep New Data
    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data    = testing(m750_splits),
            actual_data = m750,
            keep_data   = TRUE
        )
    # forecast_tbl %>% group_by(id) %>% plot_modeltime_forecast()

    expect_equal(nrow(forecast_tbl), 24 + n_actual)
    expect_equal(ncol(forecast_tbl), 10)

    # Refit
    refit_tbl <- calibration_tbl %>%
        modeltime_refit(m750, control = control_refit())

    # Refit in Parallel ----
    parallel_start(2)
    refit_tbl <- calibration_tbl %>%
        modeltime_refit(
            m750,
            control = control_refit(
                verbose = TRUE,
                allow_par = TRUE,
                cores = 2,
                packages = "modeltime.ensemble")
        )
    parallel_stop()

    training_results_tbl <- refit_tbl %>%
        pluck(".model", 1, "model_tbl", ".model", 1, "fit", "fit", "fit", "data")

    expect_equal(nrow(training_results_tbl), nrow(m750))

})

# Mean ----
test_that("ensemble_average(type = 'mean')", {

    ensemble_fit_mean <- m750_models_2 %>%
        ensemble_average(type = "mean")

    # Structure
    expect_s3_class(ensemble_fit_mean, "mdl_time_ensemble")
    expect_s3_class(ensemble_fit_mean, "mdl_time_ensemble_avg")

    expect_s3_class(ensemble_fit_mean$model_tbl, "mdl_time_tbl")
    expect_equal(ensemble_fit_mean$parameters$type, "mean")
    expect_equal(ensemble_fit_mean$n_models, 3)
    expect_equal(ensemble_fit_mean$desc, "ENSEMBLE (MEAN): 3 MODELS")


    # Forecast
    fcast <- modeltime_table(ensemble_fit_mean) %>%
        modeltime_forecast(testing(m750_splits))

    expect_equal(nrow(fcast), nrow(testing(m750_splits)))
    expect_equal(fcast$.index, testing(m750_splits)$date)


})

# Checks/Errors ----
test_that("Checks/Errors: ensemble_average()", {

    # Object is Missing
    expect_error(ensemble_average())

    # Incorrect Object
    expect_error(ensemble_average(1))

    # Incorrect Type
    expect_error(ensemble_average(m750_models_2, type = "blah"))

    # Needs more than 1 model
    expect_error({
        m750_models_2 %>%
            dplyr::slice(1) %>%
            ensemble_average(type = "mean")
    })


})



