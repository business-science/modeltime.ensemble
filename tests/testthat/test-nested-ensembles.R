context("TEST: nested ensembles")

# 1- SEQUENTIAL ----
testthat::test_that("Nested Ensembles Work - sequential", {

    data_tbl <- walmart_sales_weekly %>%
        select(id, Date, Weekly_Sales) %>%
        set_names(c("id", "date", "value")) %>%
        filter(id %in% c("1_1", "1_3"))

    nested_data_tbl <- data_tbl %>%
        # Step 1: Extend
        extend_timeseries(
            .id_var        = id,
            .date_var      = date,
            .length_future = 52
        ) %>%
        # Step 2: Nest
        nest_timeseries(
            .id_var        = id,
            .length_future = 52,
            .length_actual = 52*2
        ) %>%
        # Step 3: Split Train/Test
        split_nested_timeseries(
            .length_test = 52
        )

    # Submodels ----

    rec_prophet <- recipe(value ~ date, extract_nested_train_split(nested_data_tbl))

    wflw_prophet <- workflow() %>%
        add_model(
            prophet_reg("regression", seasonality_yearly = TRUE) %>%
                set_engine("prophet")
        ) %>%
        add_recipe(rec_prophet)


    rec_xgb <- recipe(value ~ ., extract_nested_train_split(nested_data_tbl)) %>%
        step_timeseries_signature(date) %>%
        step_rm(date) %>%
        step_zv(all_predictors()) %>%
        step_dummy(all_nominal_predictors(), one_hot = TRUE)

    wflw_xgb <- workflow() %>%
        add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
        add_recipe(rec_xgb)

    # Bad Model
    #   - Xgboost can't handle dates

    recipe_bad <- recipe(value ~ ., extract_nested_train_split(nested_data_tbl))

    wflw_bad <- workflow() %>%
        add_model(boost_tree("regression")) %>%
        add_recipe(recipe_bad)


    # EXPECT WARNING DUE TO BAD MODEL ----
    testthat::expect_warning({
        nested_modeltime_tbl <- modeltime_nested_fit(
            # Nested data
            nested_data = nested_data_tbl,

            # Add workflows
            wflw_prophet,
            wflw_xgb,
            wflw_bad
        )
    })


    # Ensembles ----

    # Average Ensemble ----

    nested_ensemble_1_tbl <- nested_modeltime_tbl %>%
        ensemble_nested_average(
            type           = "mean",
            keep_submodels = TRUE,
            control = control_nested_fit(verbose = TRUE)
        )

    model_table <- nested_ensemble_1_tbl %>%
        extract_nested_modeltime_table()

    testthat::expect_equal(
        model_table$.model_desc[4], "ENSEMBLE (MEAN): 2 MODELS"
    )

    model_accuracy <- nested_ensemble_1_tbl %>%
        extract_nested_test_accuracy() %>%
        filter(.model_desc == "ENSEMBLE (MEAN): 2 MODELS")

    testthat::expect_is(
        model_accuracy %>% pull(mae),
        "numeric"
    )

    # Weighted Ensemble ----

    # * TEST LOADINGS TOO LONG ----
    nested_ensemble_2_tbl <- nested_ensemble_1_tbl %>%
        ensemble_nested_weighted(
            loadings       = c(2,1,1,0),
            metric         = "rsq",

            model_ids      = c(1,2,3),
            control        = control_nested_fit(allow_par = FALSE, verbose = TRUE)
        )

    loadings_tbl <- nested_ensemble_2_tbl %>%
        extract_nested_modeltime_table(1) %>%
        pluck(".model", 5, "fit", "loadings_tbl")

    testthat::expect_is(
        loadings_tbl$.loadings,
        "numeric"
    )

    testthat::expect_equal(
        length(loadings_tbl$.loadings),
        2
    )

    # * TEST LOADINGS TOO SHORT ----
    nested_ensemble_2_tbl <- nested_ensemble_1_tbl %>%
        ensemble_nested_weighted(
            loadings       = c(2),
            metric         = "rsq",

            model_ids      = c(1,2),
            control        = control_nested_fit(allow_par = FALSE, verbose = TRUE)
        )

    loadings_tbl <- nested_ensemble_2_tbl %>%
        extract_nested_modeltime_table(1) %>%
        pluck(".model", 5, "fit", "loadings_tbl")

    testthat::expect_is(
        loadings_tbl$.loadings,
        "numeric"
    )

    testthat::expect_equal(
        length(loadings_tbl$.loadings),
        1
    )

    # CHECK METRICS

    # Should maximize rsq
    metrics_rsq <- nested_ensemble_2_tbl %>%
        extract_nested_test_accuracy() %>%
        filter(.model_id %in% c(1,2,5)) %>%
        select(id, .model_id, rsq) %>%
        pivot_wider(
            id_cols      = c(id),
            names_from   = .model_id,
            values_from  = rsq,
            names_prefix = "model_"
        ) %>%
        rowwise() %>%
        mutate(check = max(c_across(model_1:model_2))) %>%
        ungroup()

    testthat::expect_equivalent(metrics_rsq$model_5, metrics_rsq$check)



    # Should minimize rmse

    nested_ensemble_2_tbl <- nested_ensemble_1_tbl %>%
        ensemble_nested_weighted(
            loadings       = c(2),

            metric         = "rmse",

            model_ids      = c(1,2),
            control        = control_nested_fit(allow_par = FALSE, verbose = TRUE)
        )

    metrics_rmse <- nested_ensemble_2_tbl %>%
        extract_nested_test_accuracy() %>%
        filter(.model_id %in% c(1,2,5)) %>%
        select(id, .model_id, rmse) %>%
        pivot_wider(
            id_cols      = c(id),
            names_from   = .model_id,
            values_from  = rmse,
            names_prefix = "model_"
        ) %>%
        rowwise() %>%
        mutate(check = min(c_across(model_1:model_2))) %>%
        ungroup()

    testthat::expect_equivalent(metrics_rmse$model_5, metrics_rmse$check)

})

# 2. PARALLEL ----

testthat::test_that("Nested Ensembles Work - parallel", {

    testthat::skip_on_cran()

    parallel_start(2)

    data_tbl <- walmart_sales_weekly %>%
        select(id, Date, Weekly_Sales) %>%
        set_names(c("id", "date", "value")) %>%
        filter(id %in% c("1_1", "1_3"))

    nested_data_tbl <- data_tbl %>%
        # Step 1: Extend
        extend_timeseries(
            .id_var        = id,
            .date_var      = date,
            .length_future = 52
        ) %>%
        # Step 2: Nest
        nest_timeseries(
            .id_var        = id,
            .length_future = 52,
            .length_actual = 52*2
        ) %>%
        # Step 3: Split Train/Test
        split_nested_timeseries(
            .length_test = 52
        )

    # Submodels ----

    rec_prophet <- recipe(value ~ date, extract_nested_train_split(nested_data_tbl))

    wflw_prophet <- workflow() %>%
        add_model(
            prophet_reg("regression", seasonality_yearly = TRUE) %>%
                set_engine("prophet")
        ) %>%
        add_recipe(rec_prophet)


    rec_xgb <- recipe(value ~ ., extract_nested_train_split(nested_data_tbl)) %>%
        step_timeseries_signature(date) %>%
        step_rm(date) %>%
        step_zv(all_predictors()) %>%
        step_dummy(all_nominal_predictors(), one_hot = TRUE)

    wflw_xgb <- workflow() %>%
        add_model(boost_tree("regression") %>% set_engine("xgboost")) %>%
        add_recipe(rec_xgb)

    # Bad Model
    #   - Xgboost can't handle dates

    recipe_bad <- recipe(value ~ ., extract_nested_train_split(nested_data_tbl))

    wflw_bad <- workflow() %>%
        add_model(boost_tree("regression")) %>%
        add_recipe(recipe_bad)


    # EXPECT WARNING DUE TO BAD MODEL ----
    testthat::expect_warning({
        nested_modeltime_tbl <- modeltime_nested_fit(
            # Nested data
            nested_data = nested_data_tbl,

            # Add workflows
            wflw_prophet,
            wflw_xgb,
            wflw_bad
        )
    })


    # Ensembles ----

    # Average Ensemble ----

    nested_ensemble_1_tbl <- nested_modeltime_tbl %>%
        ensemble_nested_average(
            type           = "mean",
            keep_submodels = TRUE,
            control = control_nested_fit(allow_par = TRUE, verbose = TRUE)
        )

    model_table <- nested_ensemble_1_tbl %>%
        extract_nested_modeltime_table()

    testthat::expect_equal(
        model_table$.model_desc[4], "ENSEMBLE (MEAN): 2 MODELS"
    )

    model_accuracy <- nested_ensemble_1_tbl %>%
        extract_nested_test_accuracy() %>%
        filter(.model_desc == "ENSEMBLE (MEAN): 2 MODELS")

    testthat::expect_is(
        model_accuracy %>% pull(mae),
        "numeric"
    )

    # Weighted Ensemble ----

    # * TEST LOADINGS TOO LONG ----
    nested_ensemble_2_tbl <- nested_ensemble_1_tbl %>%
        ensemble_nested_weighted(
            loadings       = c(2,1,0,0),
            metric         = "rsq",

            model_ids      = c(1,2),
            control        = control_nested_fit(allow_par = TRUE, verbose = TRUE)
        )

    loadings_tbl <- nested_ensemble_2_tbl %>%
        extract_nested_modeltime_table(1) %>%
        pluck(".model", 5, "fit", "loadings_tbl")

    testthat::expect_is(
        loadings_tbl$.loadings,
        "numeric"
    )

    testthat::expect_equal(
        length(loadings_tbl$.loadings),
        2
    )

    # * TEST LOADINGS TOO SHORT ----
    nested_ensemble_2_tbl <- nested_ensemble_1_tbl %>%
        ensemble_nested_weighted(
            loadings       = c(2),
            metric         = "rsq",

            model_ids      = c(1,2),
            control        = control_nested_fit(allow_par = FALSE, verbose = TRUE)
        )

    loadings_tbl <- nested_ensemble_2_tbl %>%
        extract_nested_modeltime_table(1) %>%
        pluck(".model", 5, "fit", "loadings_tbl")

    testthat::expect_is(
        loadings_tbl$.loadings,
        "numeric"
    )

    testthat::expect_equal(
        length(loadings_tbl$.loadings),
        1
    )

    # CHECK METRICS

    # Should maximize rsq
    metrics_rsq <- nested_ensemble_2_tbl %>%
        extract_nested_test_accuracy() %>%
        filter(.model_id %in% c(1,2,5)) %>%
        select(id, .model_id, rsq) %>%
        pivot_wider(
            id_cols      = c(id),
            names_from   = .model_id,
            values_from  = rsq,
            names_prefix = "model_"
        ) %>%
        rowwise() %>%
        mutate(check = max(c_across(model_1:model_2))) %>%
        ungroup()

    testthat::expect_equivalent(metrics_rsq$model_5, metrics_rsq$check)



    # Should minimize rmse

    nested_ensemble_2_tbl <- nested_ensemble_1_tbl %>%
        ensemble_nested_weighted(
            loadings       = c(2),

            metric         = "rmse",

            model_ids      = c(1,2),
            control        = control_nested_fit(allow_par = TRUE, verbose = TRUE)
        )

    metrics_rmse <- nested_ensemble_2_tbl %>%
        extract_nested_test_accuracy() %>%
        filter(.model_id %in% c(1,2,5)) %>%
        select(id, .model_id, rmse) %>%
        pivot_wider(
            id_cols      = c(id),
            names_from   = .model_id,
            values_from  = rmse,
            names_prefix = "model_"
        ) %>%
        rowwise() %>%
        mutate(check = min(c_across(model_1:model_2))) %>%
        ungroup()

    testthat::expect_equivalent(metrics_rmse$model_5, metrics_rmse$check)

    parallel_stop()

})

