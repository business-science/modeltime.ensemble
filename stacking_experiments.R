


# Time Series ML
library(tidymodels)
library(modeltime)
library(stacks)

# Timing & Parallel Processing
library(tictoc)
library(future)
library(doFuture)

# Core
library(plotly)
library(tidyverse)
library(lubridate)
library(timetk)

# Parallel Processing ----

registerDoFuture()

n_cores <- availableCores()

plan(
    strategy = cluster,
    workers  = parallel::makeCluster(n_cores)
)

# plan(sequential)

# DATA ----

m750 <- m4_monthly %>%
    filter(id == "M750")

m750

# TRAIN / TEST ----

splits <- time_series_split(m750, assess = "2 years", cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, value)

# STACKING ----

# Cross Validation ----

resamples_tscv <- time_series_cv(training(splits), assess = "2 years", skip = "2 years", cumulative = TRUE, slice_limit = 6)

resamples_tscv %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(date, value, .facet_ncol = 2)

# Recipe ----

recipe_spec <- recipe(value ~ date, training(splits)) %>%
    step_timeseries_signature(date) %>%
    step_rm(matches("(.iso$)|(.xts$)")) %>%
    step_normalize(matches("(index.num$)|(_year$)")) %>%
    step_dummy(all_nominal()) %>%
    step_fourier(date, K = 1, period = 12)

recipe_spec %>% prep() %>% juice() %>% glimpse()



# ARIMA ----

model_spec_arima <- arima_reg() %>%
    set_engine("auto_arima")

wflw_fit_arima <- workflow() %>%
    add_model(model_spec_arima) %>%
    add_recipe(recipe_spec %>% step_rm(all_predictors(), -date)) %>%
    fit(training(splits))


# PROPHET ----

model_spec_prophet <- prophet_reg() %>%
    set_engine("prophet")

wflw_fit_prophet <- workflow() %>%
    add_model(model_spec_prophet) %>%
    add_recipe(recipe_spec %>% step_rm(all_predictors(), -date)) %>%
    fit(training(splits))


# GLMNET -----

model_spec_glmnet <- linear_reg(
    mixture = tune(),
    penalty = tune()
) %>%
    set_engine("glmnet")

wflw_spec_glmnet <- workflow() %>%
    add_model(model_spec_glmnet) %>%
    add_recipe(recipe_spec %>% step_rm(date))

tune_results_glmnet <- wflw_spec_glmnet %>%
    tune_grid(
        resamples  = resamples_tscv,
        param_info = parameters(
            mixture(),
            penalty()
        ),
        grid       = 15,
        metrics    = default_forecast_accuracy_metric_set(),
        control    = control_grid(save_pred = TRUE, allow_par = TRUE)
    )

tune_results_glmnet %>% show_best("rmse", n = Inf)

tune_results_glmnet %>% autoplot() + geom_smooth(method = "loess")

wflw_fit_glmnet <- wflw_spec_glmnet %>%
    finalize_workflow(
        tune_results_glmnet %>%
            show_best("rsq", n = 1)
    ) %>%
    fit(training(splits))


# MODELTIME ----


calibration_tbl <- modeltime_table(
    wflw_fit_arima,
    wflw_fit_prophet,
    wflw_fit_glmnet
) %>%
    modeltime_calibrate(testing(splits))

calibration_tbl %>%
    modeltime_accuracy()

calibration_tbl %>%
    modeltime_forecast(
        new_data    = testing(splits),
        actual_data = m750
    ) %>%
    plot_modeltime_forecast()


# STACKING ----

fit_results_arima <- wflw_fit_arima %>%
    fit_resamples(
        resamples = resamples_tscv,
        metrics   = default_forecast_accuracy_metric_set(),
        control   = control_resamples(save_pred = TRUE, allow_par = TRUE, save_workflow = TRUE)
    )

fit_results_arima %>%
    show_best("rmse")

fit_results_prophet <- wflw_fit_prophet %>%
    fit_resamples(
        resamples = resamples_tscv,
        metrics   = default_forecast_accuracy_metric_set(),
        control   = control_resamples(save_pred = TRUE, allow_par = TRUE, save_workflow = TRUE)
    )

fit_results_glmnet <- wflw_fit_glmnet %>%
    fit_resamples(
        resamples = resamples_tscv,
        metrics   = default_forecast_accuracy_metric_set(),
        control   = control_resamples(save_pred = TRUE, allow_par = TRUE, save_workflow = TRUE)
    )

stacks_1 <- stacks() %>%
    add_candidates(fit_results_arima) %>%
    add_candidates(fit_results_glmnet) %>%
    add_candidates(fit_results_prophet)

stacks_1 %>% as_tibble() %>% arrange(-value)

stacks_blended_1 <- stacks_1 %>%
    blend_predictions(verbose = TRUE)


stacks_fitted_1 <- stacks_blended_1 %>%
    fit_members()

stacks_fitted_1 %>%
    predict(new_data = testing(splits))


# SHIFTING INTO A WORKFLOW ----

resample_tbl <- calibration_tbl %>%
    mutate(.resample_data = map(.model, .f = function(model) {
        model %>%
            fit_resamples(
                resamples = resamples_tscv,
                metrics   = default_forecast_accuracy_metric_set(),
                control   = control_resamples(save_pred = TRUE, allow_par = TRUE, save_workflow = TRUE)
            )
    }))

parse_model_name_from_description <- function(x) {
    x %>%
        str_split(pattern = "([:punct:])|( )") %>%
        pluck(1, 1) %>%
        str_trim()
}

data_stack <- stacks::stacks()
for (i in seq_along(resample_tbl$.model_id)) {
    # print(resample_tbl$.resample_data[[i]])

    model      <- resample_tbl$.model[[i]]
    model_id   <- resample_tbl$.model_id[[i]]
    model_nm   <- get_model_description(model) %>%
        parse_model_name_from_description()


    model_name <- str_glue("model_{model_id}_{model_desc}")
    # print(model_name)

    data_stack <- data_stack %>%
        add_candidates(
            resample_tbl$.resample_data[[i]],
            name = model_name
        )
}


data_stack





