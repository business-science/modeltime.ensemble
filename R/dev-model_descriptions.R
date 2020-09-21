

#' @export
get_model_description.mdl_time_ensemble_avg <- function(object, indicate_training = FALSE, upper_case = TRUE) {

    type     <- object$type
    n_models <- object$n_models

    desc <- glue::glue("Ensemble ({type}): {n_models} Models")

    if (indicate_training) {
        desc <- stringr::str_c(desc, " (Trained)")
    }

    if (upper_case) {
        desc <- toupper(desc)
    } else {
        desc <- tolower(desc)
    }

    return(desc)
}
