

#' @export
#' @importFrom modeltime mdl_time_refit
mdl_time_refit.mdl_time_ensemble <- function(object, data, ..., control = NULL) {

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
