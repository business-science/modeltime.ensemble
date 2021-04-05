context("TEST: RECURSIVE ENSEMBLES")


# SINGLE / RECIPE / PARSNIP ----

test_that("recursive ensemble 1 - single / recipe / parsnip", {

    skip_on_cran()

    FORECAST_HORIZON <- 24

    m750_extended <- m750 %>%
        group_by(id) %>%
        future_frame(
            .length_out = FORECAST_HORIZON,
            .bind_data  = TRUE
        ) %>%
        ungroup()

    # Lag Recipe
    recipe_lag <- recipe(value ~ date, m750_extended) %>%
        step_lag(value, lag = 1:FORECAST_HORIZON)

    # Data Transformation
    m750_lagged <- recipe_lag %>% prep() %>% juice()

    train_data <- m750_lagged %>%
        drop_na()

    future_data <- m750_lagged %>%
        filter(is.na(value))


    # * Recursive Modeling ----
    model_fit_lm <- linear_reg() %>%
        set_engine("lm") %>%
        fit(value ~ ., data = train_data)

    model_fit_mars <- mars("regression") %>%
        set_engine("earth", endspan = 24) %>%
        fit(value ~ ., data = train_data)

    recursive_ensemble <- modeltime_table(
        model_fit_lm,
        model_fit_mars
    ) %>%
        ensemble_average(type = "mean") %>%
        recursive(
            transform  = recipe_lag,
            train_tail = tail(train_data, FORECAST_HORIZON)
        )

    expect_s3_class(recursive_ensemble, "recursive_ensemble")
    expect_s3_class(recursive_ensemble, "recursive")

    # * Modeltime Forecast  ----
    forecast_tbl <- modeltime_table(
        recursive_ensemble
    ) %>%
        modeltime_forecast(
            new_data    = future_data,
            actual_data = m750,
            keep_data   = TRUE
        )

    # Visualize
    # forecast_tbl %>% plot_modeltime_forecast()

    preds <- forecast_tbl %>% filter(.model_id == 1) %>% pull(.value)
    expect_equal(
        length(future_data$value),
        length(preds)
    )

    expect_lt(max(preds), 11500)
    expect_gt(min(preds), 9650)

    # * Modeltime Refit ----

    retrain_tbl <- m750_lagged %>% dplyr::slice(1:200) %>% drop_na()
    future_tbl  <- m750_lagged %>% dplyr::slice(201:224)

    refit_tbl <- modeltime_table(
        recursive_ensemble
    ) %>%
        modeltime_refit(
            data = retrain_tbl
        )

    forecast_refit_tbl <- refit_tbl %>%
        modeltime_forecast(
            new_data    = future_tbl,
            actual_data = retrain_tbl
        )

    # forecast_refit_tbl %>% plot_modeltime_forecast()

    preds <- forecast_refit_tbl %>% filter(.model_id == 1) %>% pull(.value)
    expect_equal(
        length(future_tbl$value),
        length(preds)
    )

    expect_lt(max(preds), 10600)
    expect_gt(min(preds), 8700)

})


# PANEL / FUNCTION / PARSNIP & WORKFLOW ----

test_that("recursive ensemble 2 - panel / function / parsnip + workflow", {

    # Jumble the data to make sure it forecasts properly
    FORECAST_HORIZON <- 24

    m4_extended <- m4_monthly %>%
        group_by(id) %>%
        future_frame(
            .length_out = FORECAST_HORIZON,
            .bind_data  = TRUE
        ) %>%
        ungroup()

    # Transformation Function
    lag_transformer_grouped <- function(data){
        data %>%
            group_by(id) %>%
            tk_augment_lags(value, .lags = 1:FORECAST_HORIZON) %>%
            ungroup()
    }

    m4_lags <- m4_extended %>%
        lag_transformer_grouped()

    train_data <- m4_lags %>%
        drop_na()

    future_data <- m4_lags %>%
        filter(is.na(value))

    # * Recursive Modeling ----

    wflw_fit_glmnet <- workflow() %>%
        add_recipe(
            recipe(value ~ ., train_data) %>%
                step_rm(date) %>%
                step_dummy(id, one_hot = TRUE)
        ) %>%
        add_model(linear_reg(penalty = 1) %>% set_engine("glmnet")) %>%
        fit(train_data)

    wflw_fit_mars <- workflow() %>%
        add_recipe(
            recipe(value ~ ., train_data) %>%
                step_rm(date) %>%
                step_dummy(id, one_hot = TRUE)
        ) %>%
        add_model(
            mars("regression") %>%
                set_engine("earth", endspan = 24)
        ) %>%
        fit(train_data)

    ensemble_panel <- modeltime_table(
        wflw_fit_glmnet,
        wflw_fit_mars
    ) %>%
        ensemble_average(type = "mean")

    recursive_ensemble_panel <- ensemble_panel %>%
        recursive(
            transform  = lag_transformer_grouped,
            train_tail = panel_tail(train_data, id, FORECAST_HORIZON),
            id         = "id"
        )

    expect_s3_class(recursive_ensemble_panel, "recursive_ensemble")
    expect_s3_class(recursive_ensemble_panel, "recursive_panel")


    # * Forecasting ----

    # recursive_ensemble_panel %>% mdl_time_forecast(new_data = future_data)

    forecast_tbl <- modeltime_table(
        recursive_ensemble_panel
    ) %>%
        modeltime_forecast(
            new_data    = future_data,
            actual_data = m4_lags %>% drop_na(),
            keep_data   = TRUE
        )

    # forecast_tbl %>% group_by(id) %>% plot_modeltime_forecast()

    # forecast_tbl %>% group_by(id) %>% plot_modeltime_forecast()
    preds_1 <- forecast_tbl %>% filter(.model_id == 1) %>% pull(.value)
    expect_equal(
        length(future_data$value),
        length(preds_1)
    )

    expect_type(preds_1, "double")

    # * Modeltime Refit ----

    refit_tbl <- modeltime_table(
        recursive_ensemble_panel
    ) %>%
        modeltime_refit(train_data)

    expect_s3_class(refit_tbl$.model[[1]], "recursive_ensemble")
    expect_s3_class(refit_tbl$.model[[1]], "recursive_panel")

    forecast_refit_tbl <- refit_tbl %>%
        modeltime_forecast(
            new_data    = future_data,
            actual_data = m4_lags %>% drop_na(),
            keep_data   = TRUE
        )

    # forecast_refit_tbl %>% group_by(id) %>% plot_modeltime_forecast()

    preds_1 <- forecast_refit_tbl %>% filter(.model_id == 1) %>% pull(.value)

    expect_equal(
        length(future_data$value),
        length(preds_1)
    )

    expect_type(preds_1, "double")

})

