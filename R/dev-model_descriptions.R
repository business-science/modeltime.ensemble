

#' @export
get_model_description.mdl_time_ensemble_avg <- function(object, indicate_training = FALSE, upper_case = TRUE) {

    type     <- object$parameters$type
    n_models <- object$n_models

    desc <- stringr::str_glue("Ensemble ({type}): {n_models} Models")

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

#' @export
get_model_description.mdl_time_ensemble_wt <- function(object, indicate_training = FALSE, upper_case = TRUE) {

    n_models <- object$n_models

    desc <- stringr::str_glue("Ensemble (Weighted): {n_models} Models")

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


#' @export
get_model_description.mdl_time_ensemble_linear_stack <- function(object, indicate_training = FALSE, upper_case = TRUE) {

    n_models <- object$n_models

    desc <- stringr::str_glue("Ensemble (Linear Stack): {n_models} Models")

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
