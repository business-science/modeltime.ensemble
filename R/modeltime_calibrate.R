

#' @export
modeltime_calibrate.mdl_time_ensemble <- function(object, new_data, id = NULL, quiet = TRUE, ...) {

    ret <- modeltime_table(object) %>%
        modeltime_calibrate(new_data = new_data, id = id, quiet = quiet, ...)

    message("Converting to Modeltime Table.")

    return(ret)

}
