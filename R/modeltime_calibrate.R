

#' @export
modeltime_calibrate.mdl_time_ensemble <- function(object, new_data, quiet = TRUE, ...) {

    ret <- modeltime_table(object) %>%
        modeltime_calibrate(new_data = new_data, quiet = quiet, ...)

    message("Converting to Modeltime Table.")

    return(ret)

}
