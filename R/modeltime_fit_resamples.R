# FIT RESAMPLES ----

#' Fits Models in a Modeltime Table to Resamples
#'
#' @param object A Modeltime Table
#' @param resamples A resample `rset` created from a function like [timetk::time_series_cv()].
#' @param control A [tune::control_resamples()] object to provide
#'  control over the resampling process.
#'
#' @export
modeltime_fit_resamples <- function(object, resamples, control = control_resamples()) {

    # Check resamples
    if (!inherits(resamples, "rset")) rlang::abort("'resamples' must be an rset object. Try using `timetk::time_series_cv()` to create an rset.")

    # Check Control

    UseMethod("modeltime_fit_resamples", object)
}

#' @export
modeltime_fit_resamples.mdl_time_tbl <- function(object, resamples, control = control_resamples()) {

    data <- object # object is a Modeltime Table

    # Safely refit
    safe_mdl_time_fit_resamples <- purrr::safely(
        mdl_time_fit_resamples,
        otherwise = NA,
        quiet     = FALSE
    )

    # Implement progressr for progress reporting
    p <- progressr::progressor(steps = nrow(data))

    ret <- data %>%
        dplyr::ungroup() %>%
        dplyr::mutate(.resample_results = purrr::pmap(
            .l         = list(.model, .model_id, .model_desc),
            .f         = function(obj, id, desc) {

                p(stringr::str_glue("Model ID = {id} / {max(data$.model_id)}"))

                if (control$verbose) cli::cli_li(stringr::str_glue("Model ID: {cli::col_blue(id)} {cli::col_blue(desc)}"))

                suppressMessages({
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

    return(ret)

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
