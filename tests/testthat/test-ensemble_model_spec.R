context("TEST: ensemble_model_spec() & modeltime_fit_resamples()")

resamples_tscv <- training(m750_splits) %>%
    time_series_cv(assess = "2 years", initial = "5 years", skip = "2 years", slice_limit = 1)

full_resamples_tscv <- m750 %>%
    time_series_cv(assess = "2 years", initial = "5 years", skip = "2 years", slice_limit = 1)

m750_models_resample <- m750_models %>%
    modeltime_fit_resamples(resamples_tscv, control = control_resamples(verbose = F))

# TEST ENSEMBLE AVERAGE ----

# * No Tuning ----
test_that("ensemble_model_spec(): Linear Regression (No Tuning)", {

   ensemble_fit_lm <- m750_models_resample %>%
        ensemble_model_spec(
            model_spec = linear_reg() %>% set_engine("lm"),
            grid       = 3,
            control    = control_grid(verbose = TRUE)
        )

    # Structure
    expect_s3_class(ensemble_fit_lm, "mdl_time_ensemble")
    expect_s3_class(ensemble_fit_lm, "mdl_time_ensemble_model_spec")

    expect_s3_class(ensemble_fit_lm$model_tbl, "mdl_time_tbl")
    expect_s3_class(ensemble_fit_lm$fit$fit, "workflow")

    expect_equal(ensemble_fit_lm$n_models, 3)
    expect_equal(ensemble_fit_lm$desc, "ENSEMBLE (LM STACK): 3 MODELS")

    # Print
    expect_equal(print(ensemble_fit_lm), ensemble_fit_lm)

    # Modeltime Table
    expect_equal(
        modeltime_table(ensemble_fit_lm) %>% pull(.model_desc),
        "ENSEMBLE (LM STACK): 3 MODELS"
    )

    # Calibration
    calibration_tbl <- ensemble_fit_lm %>%
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

    # Refit - NO RESAMPLE

    expect_warning({
        # Expect warning when resamples are not provided
        refit_tbl <- calibration_tbl %>%
            combine_modeltime_tables(m750_models) %>%
            modeltime_refit(m750)
    })



    # Refit - WITH RESAMPLES

    refit_tbl <- calibration_tbl %>%
        combine_modeltime_tables(m750_models) %>%
        modeltime_refit(m750, resamples = full_resamples_tscv, control = control_resamples(verbose = TRUE))

    future_tbl <- refit_tbl %>% modeltime_forecast(h = "2 years", actual_data = m750)

    expect_equal(nrow(m750) + 4*24, nrow(future_tbl) )




})


# * Tuning ----
test_that("ensemble_model_spec(): GLMNET (Tuning)", {

    ensemble_fit_glmnet <- m750_models_resample %>%
        ensemble_model_spec(
            model_spec = linear_reg(penalty = tune(), mixture = tune()) %>%
                set_engine("glmnet"),
            grid       = 1,
            control    = control_grid(verbose = TRUE)
        )

    # Structure
    expect_s3_class(ensemble_fit_glmnet, "mdl_time_ensemble")
    expect_s3_class(ensemble_fit_glmnet, "mdl_time_ensemble_model_spec")

    expect_s3_class(ensemble_fit_glmnet$model_tbl, "mdl_time_tbl")
    expect_s3_class(ensemble_fit_glmnet$fit$fit, "workflow")

    expect_equal(ensemble_fit_glmnet$n_models, 3)
    expect_equal(ensemble_fit_glmnet$desc, "ENSEMBLE (GLMNET STACK): 3 MODELS")

    # Print
    expect_equal(print(ensemble_fit_glmnet), ensemble_fit_glmnet)

    # Modeltime Table
    expect_equal(
        modeltime_table(ensemble_fit_glmnet) %>% pull(.model_desc),
        "ENSEMBLE (GLMNET STACK): 3 MODELS"
    )

    # Calibration
    calibration_tbl <- ensemble_fit_glmnet %>%
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

    # Multi-Level Stacking
    model_tbl <- m750_models %>%
        add_modeltime_model(ensemble_fit_glmnet)

    ensemble_fit_wt <- model_tbl %>%
        ensemble_weighted(loadings = c(4, 1, 1, 4))

    multi_level_model_tbl <- modeltime_table(
        ensemble_fit_wt
    ) %>%
        combine_modeltime_tables(m750_models)

    accuracy_tbl <- multi_level_model_tbl %>%
        modeltime_accuracy(testing(m750_splits))

    refit_tbl <- multi_level_model_tbl %>%
        modeltime_refit(m750)

    forecast_tbl <- refit_tbl %>%
        modeltime_forecast(h = "2 years", actual_data = m750)

    expect_false(all(is.na(accuracy_tbl$mae)))
    expect_equal(nrow(forecast_tbl), 24*4 + n_actual)

})



# * Checks/Errors ----
test_that("Checks/Errors: ensemble_model_spec()", {

    # Object is Missing
    expect_error(ensemble_model_spec())

    # Incorrect Object
    expect_error(ensemble_model_spec(1))

    # No resamples
    expect_error(ensemble_model_spec(m750_models))

    # Needs 'model_spec'
    expect_error({
        m750_models_resample %>%
            ensemble_model_spec()
    })

    # Needs 'set_engine()'
    expect_error({
        m750_models_resample %>%
            ensemble_model_spec(
                model_spec = linear_reg()
            )
    })

    # Needs more than one model
    expect_error({
        m750_models_resample %>%
            slice(1) %>%
            ensemble_model_spec(
                model_spec = linear_reg() %>% set_engine("lm")
            )
    })

})

# MODELTIME FIT RESAMPLES ----

test_that("Structure: modeltime_fit_resamples()", {

    # Structure

    expect_true(".resample_results" %in% names(m750_models_resample))

    resamples_unnested <- unnest_resamples(m750_models_resample)
    expect_true(all(c(".model_id", ".model_desc", ".pred") %in% names(resamples_unnested)))

})

# * Checks/Errors ----
test_that("Checks/Errors: modeltime_fit_resamples()", {

    # Object is Missing
    expect_error(modeltime_fit_resamples())

    # Incorrect Object
    expect_error(modeltime_fit_resamples(1))

    # No resamples
    expect_error(modeltime_fit_resamples(m750_models))

    # Needs 'model_spec'
    expect_error({
        modeltime_fit_resamples(m750_models, 1)
    })

})

# MODELTIME RESAMPLE ACCURACY ----

test_that("Structure:: modeltime_resample_accuracy()", {

    # Structure
    resample_accuracy <- m750_models_resample %>%
        modeltime_resample_accuracy()

    expect_equal(nrow(resample_accuracy), 3)

})

