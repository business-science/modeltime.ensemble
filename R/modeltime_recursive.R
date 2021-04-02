

#' @export
#' @importFrom modeltime recursive
recursive.mdl_time_ensemble <- function(object, transform, train_tail, id = NULL, ...){

    .class_obj <- if(!is.null(id)){"recursive_panel"} else {"recursive"}

    object$spec[["forecast"]]   <- .class_obj
    object$spec[["transform"]]  <- if(!is.null(id)){.prepare_panel_transform(transform)} else {.prepare_transform(transform)}
    object$spec[["train_tail"]] <- train_tail
    object$spec[["y_var"]]      <- object$model_tbl$.model[[1]]$preproc$y_var
    object$spec[["id"]]         <- id

    .class <- class(object)
    class(object) <- c("recursive_ensemble", .class_obj,  .class)

    # Model silently wrapped with modeltime_table
    # modeltime_table(object)

    return(object)
}
