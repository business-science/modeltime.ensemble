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
wflw_fit_svm <- workflow() %>%
    add_model(svm_rbf() %>% set_engine("kernlab")) %>%
    add_recipe(recipe_spec %>% update_role(date, new_role = "ID")) %>%
    fit(data_set)



test_that("ensemble_average(): Forecast Jumbled", {

    model_tbl <- modeltime_table(
        wflw_fit_prophet_boost,
        wflw_fit_svm
    ) %>%
        ensemble_average() %>%
        modeltime_table()

    forecast_tbl <- model_tbl %>%
        modeltime_forecast(
            new_data      = data_set,
            actual_data   = data_set,
            keep_data     = TRUE,
            arrange_index = FALSE
        )

    actual_tbl <- forecast_tbl %>%
        filter(.key == "actual")

    expect_equal(actual_tbl$.value, actual_tbl$value)

    ensemble_tbl <- forecast_tbl %>%
        filter(.model_id == 1)

    expect_equal(ensemble_tbl$.index, ensemble_tbl$date)

})

test_that("ensemble_average(): Forecast Jumbled", {

    loadings <- c(3, 1)

    model_tbl <- modeltime_table(
        wflw_fit_prophet_boost,
        wflw_fit_svm
    ) %>%
        ensemble_weighted(loadings) %>%
        modeltime_table()

    forecast_tbl <- model_tbl %>%
        modeltime_forecast(
            new_data      = data_set,
            actual_data   = data_set,
            keep_data     = TRUE,
            arrange_index = FALSE
        )

    actual_tbl <- forecast_tbl %>%
        filter(.key == "actual")

    expect_equal(actual_tbl$.value, actual_tbl$value)

    ensemble_tbl <- forecast_tbl %>%
        filter(.model_id == 2)

    expect_equal(ensemble_tbl$.index, ensemble_tbl$date)

})
