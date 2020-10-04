# FIT RESAMPLES ----

#' Fits Models in a Modeltime Table to Resamples
#'
#' Resampled predictions are commonly needed as part of the Ensembling Process
#' for Stacked Ensembles, which use meta-learners.
#' Refer to [ensemble_model_spec()].
#'
#' @param object A Modeltime Table
#' @param resamples An `rset` resample object.
#'   Used to generate sub-model predictions for the meta-learner.
#'   See [timetk::time_series_cv()] or [rsample::vfold_cv()] for making resamples.
#' @param control A [tune::control_resamples()] object to provide
#'  control over the resampling process.
#'
#' @return A Modeltime Table (`mdl_time_tbl`) object with a column containing
#'  resample results (`.resample_results`)
#'
#' @details
#'
#' The function uses `tune::fit_resamples()` to iteratively train and predict models
#' contained in a Modeltime Table on resample objects. This is useful in
#' creating Stacked Ensembles using [ensemble_model_spec()] because the submodel
#' cross-validation predictions are used as the input to the meta-learner model.
#'
#' One difference between `tune::fit_resamples()` and `modeltime_fit_resamples()`
#' is that predictions are always returned
#' (i.e. `control = tune::control_resamples(save_pred = TRUE)`). This is needed for
#' `ensemble_model_spec()`.
#'
#' @examples
#' library(tidymodels)
#' library(modeltime)
#' library(modeltime.ensemble)
#' library(timetk)
#' library(tidyverse)
#'
#' resamples_tscv <- training(m750_splits) %>%
#'     time_series_cv(
#'         assess      = "2 years",
#'         initial     = "5 years",
#'         skip        = "2 years",
#'         slice_limit = 1
#'     )
#'
#' m750_models_resample <- m750_models %>%
#'     modeltime_fit_resamples(
#'         resamples = resamples_tscv,
#'         control   = control_resamples(verbose = TRUE)
#'     )
#'
#' m750_models_resample
#'
#' @export
modeltime_fit_resamples <- function(object, resamples, control = control_resamples()) {

    # Check resamples
    if (rlang::is_missing(object)) rlang::abort("'object' is missing. Try using using 'modeltime_table()' to create a Modeltime Table.")
    if (!inherits(object, "mdl_time_tbl")) rlang::abort("'object' must be a Modeltime Table.")

    # Check resamples
    if (rlang::is_missing(resamples)) rlang::abort("'resamples' is missing. Try using using 'timetk::time_series_cv()' or 'rsample::vfold_cv()' to create a resample 'rset' object.")
    if (!inherits(resamples, "rset")) rlang::abort("'resamples' must be an rset object. Try using 'timetk::time_series_cv()' or 'rsample::vfold_cv()' to create an rset.")

    # Check Control

    UseMethod("modeltime_fit_resamples", object)
}

#' @export
modeltime_fit_resamples.mdl_time_tbl <- function(object, resamples, control = control_resamples()) {

    data <- object # object is a Modeltime Table

    if (!control$save_pred) control$save_pred <- TRUE

    if (control$verbose) {
        tictoc::tic()
        print(cli::rule("Fitting Resamples", width = 65))
        cli::cat_line()
    }

    # Map fitting of resample
    if (control$verbose) {
        ret <- map_fit_resamples(data, resamples, control)
    } else {
        suppressMessages({
            ret <- map_fit_resamples(data, resamples, control)
        })
    }

    if (control$verbose) {
        tictoc::toc()
        cli::cat_line()
    }

    return(ret)

}

map_fit_resamples <- function(data, resamples, control) {

    # Safely refit
    safe_mdl_time_fit_resamples <- purrr::safely(
        mdl_time_fit_resamples,
        otherwise = NA,
        quiet     = FALSE
    )

    # Implement progressr for progress reporting
    p <- progressr::progressor(steps = nrow(data))

    data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.resample_results = purrr::pmap(
            .l         = list(.model, .model_id, .model_desc),
            .f         = function(obj, id, desc) {

                p(stringr::str_glue("Model ID = {id} / {max(data$.model_id)}"))

                if (control$verbose) cli::cli_li(stringr::str_glue("Model ID: {cli::col_blue(as.character(id))} {cli::col_blue(desc)}"))

                suppressWarnings({
                    # Warning message:
                    # In `[.tbl_df`(x, is.finite(x <- as.numeric(x))) :
                    #     NAs introduced by coercion

                    ret <- safe_mdl_time_fit_resamples(
                        object    = obj,
                        resamples = resamples,
                        control   = control
                    )
                })

                ret <- ret %>% purrr::pluck("result")

                return(ret)
            })
        )
}

#' Modeltime Fit Resample Helpers
#'
#' Used for low-level resample fitting of modeltime, parnsip and workflow models
#' These functions are not intended for user use.
#'
#' @inheritParams ensemble_weighted
#'
#' @return A tibble with forecast features
#'
#' @keywords internal
#'
#' @export
mdl_time_fit_resamples <- function(object, resamples, control = control_resamples()) {
    UseMethod("mdl_time_fit_resamples", object)
}


#' @export
#' @importFrom yardstick rmse
mdl_time_fit_resamples.workflow <- function(object, resamples, control = control_resamples()) {

    tune::fit_resamples(
        object    = object,
        resamples = resamples,
        metrics   = yardstick::metric_set(rmse),
        control   = control
    )

}

#' @export
mdl_time_fit_resamples.model_fit <- function(object, resamples, control = control_resamples()) {

    # Get Model Spec & Parsnip Preprocessor
    model_spec  <- object$spec
    form <- object %>% modeltime::pull_parsnip_preprocessor()
    data <- resamples %>%
        dplyr::slice(1) %>%
        purrr::pluck(1, 1) %>%
        rsample::training()
    recipe_spec <- recipes::recipe(form, data = data)

    wflw <- workflows::workflow() %>%
        workflows::add_model(model_spec) %>%
        workflows::add_recipe(recipe_spec)

    ret <- tune::fit_resamples(
        object       = wflw,
        resamples    = resamples,
        metrics      = yardstick::metric_set(rmse),
        control      = control
    )

    return(ret)

}
