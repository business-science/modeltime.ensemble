

#' @export
#' @importFrom modeltime recursive
recursive.mdl_time_ensemble <- function(object, transform, train_tail, id = NULL, chunk_size = 1, ...){

    .class_obj <- if(!is.null(id)){"recursive_panel"} else {"recursive"}

    object$spec[["forecast"]]   <- .class_obj
    object$spec[["transform"]]  <- if(!is.null(id)){.prepare_panel_transform(transform)} else {.prepare_transform(transform)}
    object$spec[["train_tail"]] <- train_tail
    object$spec[["chunk_size"]] <- as.integer(chunk_size)

    # Workflow and Model Fit Objects store y_var (outcome) differently
    model_1 <- object$model_tbl$.model[[1]]
    if (inherits(model_1, "workflow")) {
        # mld                      <- model_1 %>% workflows::pull_workflow_mold()
        mld                      <- model_1 %>% workflows::extract_mold()
        object$spec[["y_var"]]   <- names(mld$outcomes)
    } else if (inherits(model_1, "model_fit")) {
        object$spec[["y_var"]]   <- object$model_tbl$.model[[1]]$preproc$y_var
    } else {
        rlang::abort("Recursive does not currently support multi-stacked ensembles.")
    }

    object$spec[["id"]] <- id

    .class <- class(object)
    class(object) <- c("recursive_ensemble", .class_obj,  .class)

    # Model silently wrapped with modeltime_table
    # modeltime_table(object)

    return(object)
}
