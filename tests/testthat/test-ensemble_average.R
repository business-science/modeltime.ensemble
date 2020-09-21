context("TEST: ensemble_average()")

# TEST ENSEMBLE AVERAGE ----

# Median ----
test_that("ensemble_average(type = 'median')", {

    ensemble_fit_median <- m750_models %>%
        ensemble_average(type = "median")

    # Structure
    expect_s3_class(ensemble_fit_median, "mdl_time_ensemble")
    expect_s3_class(ensemble_fit_median, "mdl_time_ensemble_avg")

    expect_s3_class(ensemble_fit_median$model_tbl, "mdl_time_tbl")
    expect_equal(ensemble_fit_median$type, "median")
    expect_equal(ensemble_fit_median$n_models, 3)

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

    # Forecast
    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data = testing(m750_splits)
        )

    expect_equal(nrow(forecast_tbl), 24)
    expect_equal(ncol(forecast_tbl), 7)

    # Refit
    refit_tbl <- calibration_tbl %>%
        modeltime_refit(m750)

    training_results_tbl <- refit_tbl %>%
        pluck(".model", 1, "model_tbl", ".model", 1, "fit", "fit", "fit", "data")

    expect_equal(nrow(training_results_tbl), nrow(m750))

})

# Mean ----
test_that("ensemble_average(type = 'mean')", {

    ensemble_fit_mean <- m750_models %>%
        ensemble_average(type = "mean")

    # Structure
    expect_s3_class(ensemble_fit_mean, "mdl_time_ensemble")
    expect_s3_class(ensemble_fit_mean, "mdl_time_ensemble_avg")

    expect_s3_class(ensemble_fit_mean$model_tbl, "mdl_time_tbl")
    expect_equal(ensemble_fit_mean$type, "mean")
    expect_equal(ensemble_fit_mean$n_models, 3)


})

# Checks/Errors ----
test_that("Checks/Errors: ensemble_average()", {

    # Object is Missing
    expect_error(ensemble_average())

    # Incorrect Object
    expect_error(ensemble_average(1))

    # Incorrect Type
    expect_error(ensemble_average(m750_models, type = "blah"))

    # Needs more than 1 model
    expect_error({
        m750_models %>%
            dplyr::slice(1) %>%
            ensemble_average(type = "mean")
    })


})



