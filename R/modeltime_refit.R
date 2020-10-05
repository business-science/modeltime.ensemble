# REFITTING -----

# 1.0 AVERAGE & WEIGHTED -----

#' @export
mdl_time_refit.mdl_time_ensemble_avg <- function(object, data, ..., control = NULL) {

    model_tbl <- object$model_tbl

    # Get the raw forecast results for each of the models
    fit_modeltime <- modeltime::modeltime_refit(
        object        = model_tbl,
        data          = data,
        control       = control,
        ...
    )

    object$model_tbl <- fit_modeltime

    return(object)

}

#' @export
mdl_time_refit.mdl_time_ensemble_wt <- mdl_time_refit.mdl_time_ensemble_avg


# 2.0 MODEL SPEC ----

#' @export
mdl_time_refit.mdl_time_ensemble_model_spec <- function(object, data, ..., control = NULL) {

    # SETUP ----

    # Submodels
    model_tbl <- object$model_tbl

    # Meta-Learner Model Workflow
    wflw_fit  <- object$fit$fit
    model_spec <- wflw_fit %>%
        workflows::pull_workflow_spec()

    # Resample Data
    dot_list  <- rlang::dots_list(...)
    resamples <- dot_list$resamples
    resamples_provided <- !is.null(resamples)

    # Control
    if (is.null(control)) {
        control <- object$parameters$control
    } else {
        if (is.null(control$verbose)) control$verbose <- FALSE
        if (is.null(control$allow_par)) control$allow_par <- TRUE

    }
    control$extract       <- NULL
    control$save_workflow <- FALSE


    # REFITTING ----
    if (!resamples_provided) {
        # This section process submodels but does not refit a meta-learner

        warning("'resamples' not provided during refitting. Submodels will be refit, but the meta-learner will *not* be refit. You can provide 'resamples' via `modeltime_refit(object, data, resamples, control)`. Proceeding by refitting the submodels only.")

        # Get the raw forecast results for each of the models
        fit_modeltime <- modeltime::modeltime_refit(
            object        = model_tbl,
            data          = data,
            control       = control,
            ...
        )

        object$model_tbl <- fit_modeltime

        return(object)

    } else {
        # This section applies a full refitting using new resamples

        # Checks
        if (!inherits(resamples, "rset")) rlang::abort("'resamples' must be an rset object. Try using 'timetk::time_series_cv()' or 'rsample::vfold_cv()' to create an rset.")

        # Fit the resamples
        control$save_pred <- TRUE
        model_resample_tbl <- model_tbl %>%
            modeltime_fit_resamples(
                resamples = resamples,
                control   = control
            )

        # Fit the meta-learner
        control$save_pred <- FALSE
        ret <- model_resample_tbl %>%
            ensemble_model_spec(
                model_spec = model_spec,
                kfolds     = object$parameters$kfolds,
                param_info = object$parameters$param_info,
                grid       = object$parameters$grid,
                control    = control
            )

        return(ret)

    }

}
