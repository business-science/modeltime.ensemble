# ENSEMBLE LINEAR STACK ----

#' Creates a Linear Stacked Ensemble Model
#'
#' Uses Penalized Regression (Elastic Net) to select optimal
#' weights for stacking sub-models.
#'
#' @inheritParams modeltime_fit_resamples
#' @param object A Modeltime Table. Used for ensemble sub-models.
#' @param resamples An `rset` resample object.
#'   Used to generate sub-model predictions for the meta-learner.
#'   See [timetk::time_series_cv()] for making time series resamples.
#' @param kfolds Controls the number of folds used in the
#'  meta-learner's cross-validation.
#' @param grid_size Controls the grid size for penalty and mixture
#'  used in the meta-learner's cross validation.
#' @param penalty_range Controls the range of acceptable penalty values
#'  on a Log Base-10 Scale. Used in the meta-learner's cross validation.
#' @param mixture_range Controls the range of acceptable mixtures.
#'  Used in the meta-learner's cross validation.
#'
#' @details
#'
#' The input to an `ensemble_linear_stack()` model is always a Modeltime Table,
#' which contains the sub-models that you will ensemble.
#'
#' __Ensemble Process__
#'
#' The Meta-Learner Ensembling Process uses the following basic steps:
#'
#' 1. __Make cross-validation predictions for each sub-model.__
#'   The user provides the cross validation as `resamples`
#'   (using a function like [timetk::time_series_cv()].
#'   The sub-model predictions are needed to made as the input for the
#'   meta-learner.
#'
#' 2. __Apply Penalized Regression (Meta-Learner).__ The sub-model out-of-sample cross validation predictions are then
#'   Modeled using Penalized Regresstion (Elastic Net). This process uses tuning to find an
#'   optimal `penalty` and `mixture`. The model is then fitted to the full data set.
#'
#' 3. __Use Coefficients as Loadings.__ The penalized regression is performed without an intercept
#'   so the coefficients returned can be used to weight the models.
#'
#' __Progress__
#'
#' The best way to follow the training process and watch progress is to use
#' `control = control_resamples(verbose = TRUE)` to see progress.
#'
#' __Parallelize__
#'
#' Portions of the process can be parallelized. To parallelize, set
#' up parallelization using `tune` via one of the backends such as
#' `doFuture`. Then set `control = control_resamples(allow_par = TRUE)`
#'
#' @examples
#' library(tidymodels)
#' library(modeltime)
#' library(modeltime.ensemble)
#' library(tidyverse)
#' library(timetk)
#'
#' \dontrun{
#' resamples_tscv <- training(m750_splits) %>%
#'     time_series_cv(
#'         assess  = "2 years",
#'         initial = "5 years",
#'         skip    = "2 years",
#'         slice_limit = 6
#'     )
#'
#' ensemble_fit_ls <- m750_models %>%
#'     ensemble_linear_stack(
#'         resamples = resamples_tscv,
#'         control   = control_resamples(verbose = TRUE)
#'     )
#' }
#'
#' @export
ensemble_linear_stack <- function(object,
                                  resamples,
                                  kfolds        = 5,
                                  grid_size     = 6,
                                  penalty_range = c(-10, 0),
                                  mixture_range = c(0, 1),
                                  control       = control_resamples()) {

    # Checks
    if (rlang::is_missing(object)) rlang::abort("'object' is missing. Please provide a Modeltime Table with multiple models.")
    if (!inherits(object, "mdl_time_tbl")) rlang::abort("'object' must be a Modeltime Table.")

    if (rlang::is_missing(resamples)) rlang::abort("'resamples' must be provided. Try creating samples using 'timetk::time_series_cv()'.")
    if (!inherits(resamples, "rset")) rlang::abort("'resamples' must be an `rset` object. Trying creating samples using 'timetk::time_series_cv()'")

    if (nrow(object) < 2) rlang::abort("An ensemble requires two or more models in the Modeltime Table.")


    UseMethod("ensemble_linear_stack", object)
}

#' @export
ensemble_linear_stack.mdl_time_tbl <- function(object,
                                               resamples,
                                               kfolds        = 5,
                                               grid_size     = 6,
                                               penalty_range = c(-10, 0),
                                               mixture_range = c(0, 1),
                                               control       = control_resamples()) {

    # Calculate the loadings
    loadings_tbl <- calculate_stacking_coefficients(
        object        = object,
        resamples     = resamples,
        kfolds        = kfolds,
        grid_size     = grid_size,
        penalty_range = penalty_range,
        mixture_range = mixture_range,
        control       = control
    )


    # Create Weighted Ensemble
    ensemble_linear_stack <- list(
        model_tbl      = object,
        parameters = list(
            resamples     = resamples,
            kfolds        = kfolds,
            grid_size     = grid_size,
            penalty_range = penalty_range,
            mixture_range = mixture_range,
            control       = control
        ),
        loadings_tbl  = loadings_tbl,
        n_models       = nrow(object)
    )

    ensemble_linear_stack <- structure(
        ensemble_linear_stack,
        class = c("mdl_time_ensemble_linear_stack", "mdl_time_ensemble")
    )

    ensemble_linear_stack$desc <- get_model_description(ensemble_linear_stack)

    return(ensemble_linear_stack)
}

#' @export
print.mdl_time_ensemble_linear_stack <- function(x, ...) {

    print(cli::rule("Modeltime Ensemble", width = min(65, cli::console_width())))

    msg <- glue::glue("Ensemble of {x$n_models} Models (LINEAR STACK)")

    print(msg)

    cli::cat_line()

    print(dplyr::left_join(x$model_tbl, x$loadings_tbl, by = ".model_id"))

    invisible(x)
}


# CALCULATE LOADINGS USING GLMNET ----

calculate_stacking_coefficients <- function(object,
                                            resamples,
                                            kfolds        = 5,
                                            grid_size     = 6,
                                            penalty_range = c(-10, 0),
                                            mixture_range = c(0, 1),
                                            control = control_resamples()) {

    # 1. Fit Resamples ----

    if (control$verbose) {
        tictoc::tic()
        print(cli::rule("Fitting Resamples", width = 65))
        cli::cat_line()
    }

    suppressWarnings({
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
    })

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
        dplyr::bind_rows()

    # Target Variable is the name in the data
    target_text <- names(predictions_tbl) %>% utils::tail(1)
    target_var  <- rlang::sym(target_text)

    predictions_tbl <- predictions_tbl %>%
        dplyr::select(.row_id, .model_id, .pred, !! target_var)

    # * Actuals By Row ID ----
    actuals_by_rowid_tbl <- predictions_tbl %>%
        dplyr::filter(.model_id %in% unique(.model_id)[1]) %>%
        dplyr::select(.row_id, !! target_var)

    # * Get Predictions by Row ID ----
    predictions_by_rowid_tbl <- predictions_tbl %>%
        dplyr::select(.row_id, .model_id, .pred) %>%
        dplyr::mutate(.model_id = stringr::str_c(".model_id_", .model_id)) %>%
        tidyr::pivot_wider(
            names_from  = .model_id,
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

    form <- stats::formula(stringr::str_glue("{target_text} ~ ."))

    recipe_spec <- recipes::recipe(form, data = data_prepared_tbl) %>%
        recipes::step_rm(.row_id)

    wflw_spec <- workflows::workflow() %>%
        workflows::add_model(model_spec) %>%
        workflows::add_recipe(recipe_spec)

    # 4. Tune Model ----
    metric <- "rmse"

    tune_results_tbl <- tune::tune_grid(
        object     = wflw_spec,
        resamples  = rsample::vfold_cv(data_prepared_tbl, v = kfolds),
        param_info = dials::parameters(
            dials::penalty(range = penalty_range),
            dials::mixture(range = mixture_range)
        ),
        grid       = grid_size,
        metrics    = yardstick::metric_set(rmse),
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
        cli::cli_alert_success("Finished tuning Penalized Regression Model.")
        cli::cat_line()
        print(tune_results_tbl %>% tune::show_best(metric, n = 1))
        cli::cat_line()
    }

    # 5. Fit Best Model ----

    if (control$verbose) print(cli::rule("Selecting Loadings", width = 65))

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
        print(object %>% dplyr::left_join(loadings_tbl, by = ".model_id"))
        cli::cat_line()
        tictoc::toc()
    }

    return(loadings_tbl)

}





