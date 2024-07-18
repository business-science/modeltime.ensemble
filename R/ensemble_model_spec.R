# MODEL SPEC STACK ----

#' Creates a Stacked Ensemble Model from a Model Spec
#'
#'
#' A 2-stage stacking regressor that follows:
#' 1. Stage 1: Sub-Model's are Trained & Predicted using `modeltime.resample::modeltime_fit_resamples()`.
#' 2. Stage 2: A Meta-learner (`model_spec`) is trained on Out-of-Sample Sub-Model
#'   Predictions using `ensemble_model_spec()`.
#'
#' @param object A Modeltime Table. Used for ensemble sub-models.
#' @param model_spec A `model_spec` object defining the
#'  meta-learner stacking model specification to be used.
#'
#'  Can be either:
#'  1. __A non-tunable `model_spec`:__ Parameters are specified and are not optimized via tuning.
#'  2. __A tunable `model_spec`:__ Contains parameters identified for tuning with
#'   `tune::tune()`
#'
#' @param kfolds K-Fold Cross Validation for tuning the Meta-Learner.
#'  Controls the number of folds used in the meta-learner's cross-validation.
#'  Gets passed to `rsample::vfold_cv()`.
#' @param param_info A `dials::parameters()` object or `NULL`. If none is given, a
#'  parameters set is derived from other arguments. Passing this argument
#'  can be useful when parameter ranges need to be customized.
#' @param grid Grid specification or grid size for tuning the Meta Learner.
#'  Gets passed to `tune::tune_grid()`.
#' @param control An object used to modify the tuning process.
#'  Uses `tune::control_grid()` by default.
#'  Use `control_grid(verbose = TRUE)` to follow the training process.
#'
#' @return A `mdl_time_ensemble` object.
#'
#' @details
#'
#' __Stacked Ensemble Process__
#'
#' - Start with a _Modeltime Table_ to define your sub-models.
#'
#' - Step 1: Use `modeltime.resample::modeltime_fit_resamples()` to perform the submodel resampling procedure.
#'
#' - Step 2: Use [ensemble_model_spec()] to define and train the meta-learner.
#'
#' __What goes on inside the Meta Learner?__
#'
#' The Meta-Learner Ensembling Process uses the following basic steps:
#'
#' 1. __Make Cross-Validation Predictions.__
#'   Cross validation predictions are made for each sub-model with `modeltime.resample::modeltime_fit_resamples()`.
#'   The out-of-sample sub-model predictions contained in `.resample_results`
#'   are used as the input to the meta-learner.
#'
#' 2. __Train a Stacked Regressor (Meta-Learner).__
#'   The sub-model out-of-sample cross validation predictions are then
#'   modeled using a `model_spec` with options:
#'
#'     - __Tuning:__ If the `model_spec` does include tuning parameters via `tune::tune()`
#'       then the meta-learner will be hypeparameter tuned using K-Fold Cross Validation. The
#'       parameters and grid can adjusted using `kfolds`, `grid`, and `param_info`.
#'     - __No-Tuning:__ If the `model_spec` does _not_ include tuning parameters via `tune::tune()`
#'       then the meta-learner will not be hypeparameter tuned and will have the model
#'       fitted to the sub-model predictions.
#'
#' 3. __Final Model Selection.__
#'
#'     - __If tuned__, the final model is selected based on RMSE, then
#'       retrained on the full set of out of sample predictions.
#'     - __If not-tuned__, the fitted model from Stage 2 is used.
#'
#' __Progress__
#'
#' The best way to follow the training process and watch progress is to use
#' `control = control_grid(verbose = TRUE)` to see progress.
#'
#' __Parallelize__
#'
#' Portions of the process can be parallelized. To parallelize, set
#' up parallelization using `tune` via one of the backends such as
#' `doFuture`. Then set `control = control_grid(allow_par = TRUE)`
#'
#' @examples
#' \donttest{
#' library(tidymodels)
#' library(modeltime)
#' library(modeltime.ensemble)
#' library(dplyr)
#' library(timetk)
#' library(glmnet)
#'
#' # Step 1: Make resample predictions for submodels
#' resamples_tscv <- training(m750_splits) %>%
#'     time_series_cv(
#'         assess  = "2 years",
#'         initial = "5 years",
#'         skip    = "2 years",
#'         slice_limit = 1
#'     )
#'
#' submodel_predictions <- m750_models %>%
#'     modeltime_fit_resamples(
#'         resamples = resamples_tscv,
#'         control   = control_resamples(verbose = TRUE)
#'     )
#'
#' # Step 2: Metalearner ----
#'
#' # * No Metalearner Tuning
#' ensemble_fit_lm <- submodel_predictions %>%
#'     ensemble_model_spec(
#'         model_spec = linear_reg() %>% set_engine("lm"),
#'         control    = control_grid(verbose = TRUE)
#'     )
#'
#' ensemble_fit_lm
#'
#' # * With Metalearner Tuning ----
#' ensemble_fit_glmnet <- submodel_predictions %>%
#'     ensemble_model_spec(
#'         model_spec = linear_reg(
#'             penalty = tune(),
#'             mixture = tune()
#'         ) %>%
#'             set_engine("glmnet"),
#'         grid       = 2,
#'         control    = control_grid(verbose = TRUE)
#'     )
#'
#' ensemble_fit_glmnet
#'
#' }
#'
#' @export
ensemble_model_spec <- function(object,
                                model_spec,
                                kfolds        = 5,
                                param_info    = NULL,
                                grid          = 6,
                                control       = control_grid()) {

    # Checks
    if (rlang::is_missing(object)) rlang::abort("'object' is missing. Please provide a Modeltime Table with multiple models.")
    if (!inherits(object, "mdl_time_tbl")) rlang::abort("'object' must be a Modeltime Table.")

    if (!".resample_results" %in% names(object)) {
        rlang::abort("object does not contain '.resample_results'. Try adding resamples with `modeltime_fit_resamples()`.")
    }

    if (rlang::is_missing(model_spec)) rlang::abort("'model_spec' must be provided. Try creating a model_spec using parsnip or modeltime models.")
    if (!inherits(model_spec, "model_spec")) rlang::abort("'model_spec' must be a `model_spec` object. Try creating a model_spec using parsnip or modeltime models.")
    if (is.null(model_spec$engine)) cli::cli_abort("'model_spec' does not have an engine set. Try setting an engine using {.fn parsnip::set_engine}.")


    if (nrow(object) < 2) rlang::abort("An ensemble requires two or more models in the Modeltime Table.")


    UseMethod("ensemble_model_spec", object)
}

#' @export
ensemble_model_spec.mdl_time_tbl <- function(object,
                                             model_spec,
                                             kfolds        = 5,
                                             param_info    = NULL,
                                             grid          = 6,
                                             control       = control_grid()) {

    if (control$verbose) {
        tictoc::tic()
    }

    # Calculate the loadings
    stacking_results <- generate_stacking_results(
        object        = object,
        model_spec    = model_spec,
        kfolds        = kfolds,
        param_info    = param_info,
        grid          = grid,
        control       = control
    )


    # Create Weighted Ensemble
    ensemble_model_spec <- list(
        model_tbl      = object %>% dplyr::select(-.resample_results),
        parameters = list(
            model_spec    = model_spec,
            kfolds        = kfolds,
            param_info    = param_info,
            grid          = grid,
            control       = control
        ),
        fit            = stacking_results,
        n_models       = nrow(object)
    )

    ensemble_model_spec <- structure(
        ensemble_model_spec,
        class = c("mdl_time_ensemble_model_spec", "mdl_time_ensemble")
    )

    ensemble_model_spec$desc <- get_model_description(ensemble_model_spec)

    if (control$verbose) {
        tictoc::toc()
        cli::cat_line()
    }

    return(ensemble_model_spec)
}

#' @export
print.mdl_time_ensemble_model_spec <- function(x, ...) {

    print(cli::rule("Modeltime Ensemble", width = min(65, cli::console_width())))

    desc <- x$fit$fit %>% modeltime::get_model_description()
    msg  <- cli::format_inline("Ensemble of {x$n_models} Models ({stringr::str_c(desc, ' STACK')})")

    cat(msg, "\n\n")

    print(x$model_tbl)

    invisible(x)
}


# STACKING RESULTS ----

generate_stacking_results <- function(object,
                                      model_spec,
                                      kfolds        = 5,
                                      param_info    = NULL,
                                      grid          = 6,
                                      control       = control_grid()) {

    # 1. Fit Resamples ----
    # - This is now performed separately with modeltime_fit_resamples()

    # 2. Wrangle Predictions ----
    predictions_tbl <- modeltime.resample::unnest_modeltime_resamples(object)

    # Target Variable is the name in the data
    target_text <- predictions_tbl %>%
        modeltime.resample::get_target_text_from_resamples(column_before_target = ".row")
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

    # 3. Build Model ----

    form <- stats::formula(stringr::str_glue("{target_text} ~ ."))

    recipe_spec <- recipes::recipe(
        formula = form,
        data    = data_prepared_tbl %>% dplyr::select(-.row_id)
    )

    wflw_spec <- workflows::workflow() %>%
        workflows::add_model(model_spec) %>%
        workflows::add_recipe(recipe_spec)

    # **** Split Paths (Tuned vs Non-Tuned) **** ----

    tune_args_tbl <- wflw_spec %>% tune::tune_args()
    tuning_required <- nrow(tune_args_tbl) > 0

    # 4A. Tune Model ----
    if (tuning_required) {

        if (control$verbose) {
            print(cli::rule("Tuning Model Specification", width = 65))
            cli::cli_alert_info(stringr::str_glue("Performing {kfolds}-Fold Cross Validation."))
            cli::cat_line()
        }

        metric <- "rmse"

        tune_results_tbl <- tune::tune_grid(
            object     = wflw_spec,
            resamples  = rsample::vfold_cv(data_prepared_tbl, v = kfolds),
            param_info = param_info,
            grid       = grid,
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

        best_params_tbl <- tune_results_tbl %>% tune::show_best(metric = metric, n = 1)

        if (control$verbose) {
            cli::cli_alert_success("Finished tuning Model Specification.")
            cli::cat_line()
            cli::cli_alert_info("Model Parameters:")
            print(best_params_tbl)
            cli::cat_line()
        }

        final_model <- wflw_spec %>%
            tune::finalize_workflow(
                best_params_tbl
            ) %>%
            generics::fit(data_prepared_tbl)

    }

    # 4B. No Tuning -----
    if (!tuning_required) {

        if (control$verbose) {
            print(cli::rule("Fitting Non-Tunable Model Specification", width = 65))
            cli::cli_alert_info(stringr::str_glue("Fitting model spec to submodel cross-validation predictions."))
            cli::cat_line()
        }

        best_params_tbl <- NULL

        final_model <- wflw_spec %>%
            generics::fit(data_prepared_tbl)

    }



    # 5. Fit Best Model ----

    pred_tbl <- data_prepared_tbl %>%
        dplyr::bind_cols(
            stats::predict(final_model, data_prepared_tbl)
        )

    cv_comparison_tbl <- pred_tbl %>%
        dplyr::rename(.model_id_ensemble = .pred) %>%
        tidyr::pivot_longer(
            cols = dplyr::starts_with(".model_id"),
            names_to  = ".model_id",
            values_to = ".preds"
        ) %>%
        dplyr::group_by(.model_id) %>%
        dplyr::summarise(rmse = yardstick::rmse_vec(!! target_var, .preds), .groups = "drop") %>%
        dplyr::mutate(.model_id = stringr::str_remove(.model_id, ".model_id_")) %>%
        dplyr::left_join(
            object %>%
                dplyr::select(.model_id, .model_desc) %>%
                dplyr::mutate(.model_id = as.character(.model_id)),
            by = ".model_id"
        ) %>%
        dplyr::mutate(.model_desc = ifelse(is.na(.model_desc), "ENSEMBLE (MODEL SPEC)", .model_desc))

    if (control$verbose) {
        cli::cli_alert_info("Prediction Error Comparison:")
        print(cv_comparison_tbl)
        cli::cat_line()
    }

    if (control$verbose) print(cli::rule("Final Model", width = 65))

    if (control$verbose) {

        cli::cat_line()
        cli::cli_alert_info("Model Workflow:")
        print(final_model)
        cli::cat_line()
    }

    # Return ----
    ret <- list(
        fit                  = final_model,
        fit_params           = best_params_tbl,
        prediction_tbl       = pred_tbl,
        prediction_error_tbl = cv_comparison_tbl
    )

    return(ret)

}




