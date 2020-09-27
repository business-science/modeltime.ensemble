context("TEST: ensemble_linear_stack()")

# TEST ENSEMBLE AVERAGE ----

# Median ----
test_that("ensemble_linear_stack()", {

    skip_on_cran()
    skip_on_travis()

    resamples_tscv <- training(m750_splits) %>%
        time_series_cv(assess = "2 years", initial = "5 years", skip = "2 years", slice_limit = 1)

    ensemble_fit_ls <- m750_models %>%
        ensemble_linear_stack(
            resamples = resamples_tscv,
            grid_size = 3,
            control = control_resamples(verbose = TRUE)
        )

    # Structure
    expect_s3_class(ensemble_fit_ls, "mdl_time_ensemble")
    expect_s3_class(ensemble_fit_ls, "mdl_time_ensemble_linear_stack")

    expect_s3_class(ensemble_fit_ls$model_tbl, "mdl_time_tbl")

    expect_equal(ensemble_fit_ls$n_models, 3)
    expect_equal(ensemble_fit_ls$desc, "ENSEMBLE (LINEAR STACK): 3 MODELS")

    # Print
    expect_equal(print(ensemble_fit_ls), ensemble_fit_ls)

    # Modeltime Table
    expect_equal(
        modeltime_table(ensemble_fit_ls) %>% pull(.model_desc),
        "ENSEMBLE (LINEAR STACK): 3 MODELS"
    )

    # Calibration
    calibration_tbl <- ensemble_fit_ls %>%
        modeltime_calibrate(testing(m750_splits))

    expect_false(is.na(calibration_tbl$.type))

    # Accuracy
    accuracy_tbl <- calibration_tbl %>% modeltime_accuracy()

    expect_false(is.na(accuracy_tbl$mae))

    # Forecast
    forecast_tbl <- calibration_tbl %>%
        modeltime_forecast(
            new_data    = testing(m750_splits),
            actual_data = m750
        )

    n_actual <- nrow(m750)

    expect_equal(nrow(forecast_tbl), 24 + n_actual)
    expect_equal(ncol(forecast_tbl), 7)

    # Refit
    refit_tbl <- calibration_tbl %>%
        modeltime_refit(m750)

    training_results_tbl <- refit_tbl %>%
        pluck(".model", 1, "model_tbl", ".model", 1, "fit", "fit", "fit", "data")

    expect_equal(nrow(training_results_tbl), nrow(m750))

})



# Checks/Errors ----
test_that("Checks/Errors: ensemble_linear_stack()", {

    # Object is Missing
    expect_error(ensemble_linear_stack())

    # Incorrect Object
    expect_error(ensemble_linear_stack(1))

    # No resamples
    expect_error(ensemble_linear_stack(m750_models))

    # Needs 'rset'
    expect_error({
        m750_models %>%
            ensemble_linear_stack(resamples = 1)
    })

    # Needs more than one model
    expect_error({
        m750_models %>%
            slice(1) %>%
            ensemble_linear_stack(m750_models, resamples = m750_training_resamples)
    })

})



