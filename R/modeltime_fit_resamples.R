# FIT RESAMPLES ----

#' Fits Models in a Modeltime Table to Resamples
#'
#' @param object A Modeltime Table
#' @param resamples Resamples
#'
#' @export
modeltime_fit_resamples <- function(object, resamples) {

    # Check resamples

    UseMethod("modeltime_fit_resamples", object)
}

#' @export
modeltime_fit_resamples.mdl_time_tbl <- function(object, resamples) {

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
        dplyr::mutate(.resample_results = purrr::map2(
            .x         = .model,
            .y         = .model_id,
            .f         = function(obj, id) {

                p(stringr::str_glue("Model ID = {id} / {max(data$.model_id)}"))

                ret <- safe_mdl_time_fit_resamples(
                    obj,
                    resamples
                )

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
mdl_time_fit_resamples <- function(object, resamples) {
    UseMethod("mdl_time_fit_resamples", object)
}


#' @export
mdl_time_fit_resamples.workflow <- function(object, resamples) {

    tune::fit_resamples(
        object    = object,
        resamples = resamples,
        metrics   = metric_set(rmse, rsq),
        control   = tune::control_resamples(
            verbose       = FALSE,
            allow_par     = TRUE,
            extract       = NULL,
            save_pred     = FALSE,
            pkgs          = NULL,
            save_workflow = FALSE
        )
    )

}

#' @export
mdl_time_fit_resamples.model_fit <- function(object, resamples) {

    model_spec  <- object$spec

    formula     <- object %>% pull_parsnip_preprocessor()
    data        <- resamples %>%
        slice(1) %>%
        pluck(1, 1) %>%
        training()
    recipe_spec <- recipe(formula, data = data)

    wflw <- workflow() %>%
        add_model(model_spec) %>%
        add_recipe(recipe_spec)

    ret <- tune::fit_resamples(
        object       = wflw,
        resamples    = resamples,
        metrics      = metric_set(rmse, rsq),
        control      = tune::control_resamples(
            verbose       = FALSE,
            allow_par     = TRUE,
            extract       = NULL,
            save_pred     = FALSE,
            pkgs          = NULL,
            save_workflow = FALSE
        )
    )

    return(ret)

}
