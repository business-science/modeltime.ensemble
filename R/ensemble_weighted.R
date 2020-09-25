# ENSEMBLE WEIGHTED ----

#' Creates a Weighted Ensemble Model
#'
#' @inheritParams modeltime_fit_resamples
#' @param object A Modeltime Table
#' @param loadings Either "auto" or a vector of weights corresponding to the loadings
#' @param resamples NULL. Required to use the automated functionality.
#'   See [timetk::time_series_cv()] for making time series resamples.
#'
#' @details
#'
#' The input to an `ensemble_weighted()` model is always a Modeltime Table,
#' which contains the models that you will ensemble.
#'
#' __Weighting Methods__
#'
#' The weighted method uses uses `loadings` of either:
#'
#' - `"auto"`: Performs weighting using Penalized Regression (using and Elastic Net via `glmnet`).
#'   This method requires `resamples`. See discussion below.
#' - `<numeric>`: A vector of weights corresponding to the weighting to apply to each model.
#'
#' __Automatic Ensemble Process__
#'
#' The "auto" feature uses an ensembling process with the following basic steps:
#'
#' 1. __Make cross-validation predictions for each model.__
#'   The user provides the cross validation as `resamples` (using a function like [timetk::time_series_cv()].
#'
#' 2. __Apply Penalized Regression.__ The out-of-sample cross validation predictions are then
#'   Modeled using Penalized Regresstion (Elastic Net). This process uses tuning to find an
#'   optimal `penalty` and `mixture`. The model is then fitted to the full data set.
#'
#' 3. __Use Coefficients as Loadings.__ The penalized regression is performed without an intercept
#'   so the coefficients returned can be used to weight the models.
#'
#'
#' @examples
#' library(tidymodels)
#' library(modeltime)
#' library(modeltime.ensemble)
#' library(tidyverse)
#' library(timetk)
#'
#' # Make an ensemble from a Modeltime Table
#' ensemble_fit <- m750_models %>%
#'     ensemble_weighted(loadings = c(3, 3, 1) / 7)
#'
#' ensemble_fit
#'
#' # Forecast with the Ensemble
#' modeltime_table(
#'     ensemble_fit
#' ) %>%
#'     modeltime_forecast(
#'         new_data    = testing(m750_splits),
#'         actual_data = m750
#'     ) %>%
#'     plot_modeltime_forecast(
#'         .interactive = FALSE,
#'         .conf_interval_show = FALSE
#'     )
#'
#' @export
ensemble_weighted <- function(object, loadings = "auto", resamples = NULL, control = control_resamples()) {

    # Checks
    if (rlang::is_missing(object)) rlang::abort("'object' is missing. Please provide a Modeltime Table with multiple models.")
    if (!inherits(object, "mdl_time_tbl")) rlang::abort("'object' must be a Modeltime Table.")
    if (nrow(object) < 2) rlang::abort("An average ensemble requires two or more models in the Modeltime Table.")
    if (is.numeric(loadings)) {
        # Numeric Loadings Provided
        if (nrow(object) != length(loadings)) rlang::abort("The length of 'loadings' must must match the number of rows in 'object'.")
    } else {
        # Auto-calculate loadings
        if (tolower(loadings) != 'auto') {
            rlang::warn("'loadings' is invalid. Setting to loadings = 'auto'.")
            loadings <- "auto"
        }
        if (tolower(loadings) == "auto" && is.null(resamples)) rlang::abort("'resamples' must be provided to use the loadings = 'auto'. Try using timetk::time_series_cv().")
    }


    UseMethod("ensemble_weighted", object)
}

#' @export
ensemble_weighted.mdl_time_tbl <- function(object, loadings = "auto", resamples = NULL, control = control_resamples()) {

    # Calculate the loadings
    if (is.numeric(loadings)) {

        # Create loadings table
        loadings_tbl <- object %>%
            dplyr::select(.model_id) %>%
            dplyr::mutate(.loadings = loadings)

    } else {

        loadings_tbl <- get_auto_loadings(object, resamples, control)

    }


    # Create Weighted Ensemble
    ensemble_weighted <- list(
        model_tbl      = object,
        loadings_info  = list(
            loadings_user = loadings,
            loadings_tbl  = loadings_tbl
        ),
        n_models       = nrow(object)
    )

    ensemble_weighted <- structure(
        ensemble_weighted,
        class = c("mdl_time_ensemble_wt", "mdl_time_ensemble")
    )

    ensemble_weighted$desc <- get_model_description(ensemble_weighted)

    return(ensemble_weighted)
}

#' @export
print.mdl_time_ensemble_wt <- function(x, ...) {

    print(cli::rule("Modeltime Ensemble", width = min(65, cli::console_width())))

    msg <- glue::glue("Ensemble of {x$n_models} Models (WEIGHTED)")

    print(msg)

    cli::cat_line()

    print(dplyr::left_join(x$model_tbl, x$loadings_info$loadings_tbl, by = ".model_id"))

    invisible(x)
}


# AUTO ----

get_auto_loadings <- function(object, resamples, control = control_resamples()) {

    # 1. Fit Resamples ----

    if (control$verbose) {
        tictoc::tic()
        print(cli::rule("Fitting Resamples", width = 65))
        cli::cat_line()
    }


    resamples_results_tbl <- object %>%
        modeltime_fit_resamples(
            resamples = resamples,
            control   = tune::control_resamples(
                verbose       = control$verbose,
                allow_par     = control$allow_par,
                extract       = NULL,
                save_pred     = TRUE,
                pkgs          = control$pkgs,
                save_workflow = FALSE
            )
        )

    if (control$verbose) {
        cli::cat_line()
    }

    # 2. Wrangle Predictions ----
    predictions_tbl <- resamples_results_tbl %>%
        dplyr::select(-.model) %>%
        tidyr::unnest(.resample_results) %>%
        dplyr::select(.model_id, .model_desc, .predictions) %>%
        tidyr::unnest(.predictions) %>%
        dplyr::group_split(.model_id) %>%
        purrr::map( tibble::rowid_to_column, var = ".row_id") %>%
        dplyr::bind_rows() %>%
        dplyr::select(.row_id, .model_id, .pred, value)

    # * Actuals By Row ID ----
    actuals_by_rowid_tbl <- predictions_tbl %>%
        dplyr::filter(.model_id %in% unique(.model_id)[1]) %>%
        dplyr::select(.row_id, value)

    # * Get Predictions by Row ID ----
    predictions_by_rowid_tbl <- predictions_tbl %>%
        dplyr::select(.row_id, .model_id, .pred) %>%
        dplyr::mutate(.model_id = stringr::str_c(".model_id_", .model_id)) %>%
        tidyr::pivot_wider(
            names_from = .model_id,
            values_from = .pred
        )

    # * Join Actuals & Predictions ----
    data_prepared_tbl <- actuals_by_rowid_tbl %>%
        dplyr::left_join(predictions_by_rowid_tbl, by = ".row_id")

    # 3. Build GLMNET Model ----

    if (control$verbose) {
        print(cli::rule("Tuning Penalized Regression Model", width = 65))
        cli::cat_line()
    }

    model_spec <- parsnip::linear_reg(
        mixture = tune::tune(),
        penalty = tune::tune()
    ) %>%
        parsnip::set_engine("glmnet", intercept = FALSE)

    recipe_spec <- recipes::recipe(value ~ ., data = data_prepared_tbl) %>%
        recipes::step_rm(.row_id)

    wflw_spec <- workflows::workflow() %>%
        workflows::add_model(model_spec) %>%
        workflows::add_recipe(recipe_spec)

    # 4. Tune Model ----
    tune_results_tbl <- tune::tune_grid(
        object     = wflw_spec,
        resamples  = rsample::vfold_cv(data_prepared_tbl, v = 5),
        param_info = dials::parameters(
            dials::penalty(),
            dials::mixture()
        ),
        grid       = 6,
        metrics    = yardstick::metric_set(rmse, rsq),
        control    = tune::control_grid(
            verbose       = control$verbose,
            allow_par     = control$allow_par,
            extract       = NULL,
            save_pred     = FALSE,
            pkgs          = control$pkgs,
            save_workflow = FALSE
        )
    )

    if (control$verbose) {
        cli::cli_alert_success(str_glue("Finished tuning Penalized Regression Model."))
        cli::cat_line()
    }

    # 5. Fit Best Model ----

    if (control$verbose) print(cli::rule("Selecting Loadings", width = 65))

    metric <- "rmse"

    final_model <- wflw_spec %>%
        tune::finalize_workflow(
            tune_results_tbl %>% tune::select_best(metric)
        ) %>%
        generics::fit(data_prepared_tbl)

    # 6. Get Coefficients ----
    penalty <- final_model %>%
        purrr::pluck("fit", "fit", "spec", "args", "penalty")

    # 7. Produce Loading Table ----
    loadings_tbl <- final_model %>%
        purrr::pluck("fit", "fit", "fit") %>%
        glmnet::coef.glmnet(s = penalty) %>%
        as.matrix() %>%
        tibble::as_tibble(rownames = ".model_id") %>%
        purrr::set_names(c(".model_id", ".loadings")) %>%
        dplyr::slice(-1) %>%
        dplyr::mutate(.model_id = stringr::str_replace(.model_id, "^.model_id_", "")) %>%
        dplyr::mutate(.model_id = as.integer(.model_id))

    if (control$verbose) {
        cli::cat_line()
        print(loadings_tbl)
        cli::cat_line()
        tictoc::toc()
    }


    return(loadings_tbl)

}





