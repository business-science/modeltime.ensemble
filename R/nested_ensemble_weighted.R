

#' Nested Ensemble Weighted
#'
#' Creates an Ensemble Model using Weighted Averaging in the
#' Modeltime Nested Forecasting Workflow.
#'
#' @inheritParams ensemble_nested_average
#' @inheritParams ensemble_weighted
#'
#'
#'
#' @export
ensemble_nested_weighted <- function(object,
                                     loadings,
                                     scale_loadings = TRUE,
                                     loading_method = c("lowest_rmse", "sequential"),
                                     keep_submodels = TRUE,
                                     model_ids = NULL,
                                     control = control_nested_fit()) {

    UseMethod("ensemble_nested_weighted", object)

}

#' @export
ensemble_nested_weighted.nested_mdl_time <- function(object,
                                                     loadings,
                                                     scale_loadings = TRUE,
                                                     loading_method = c("lowest_rmse", "sequential"),
                                                     keep_submodels = TRUE,
                                                     model_ids = NULL,
                                                     control = control_nested_fit()) {

    # Parallel or Sequential
    if ((control$cores > 1) && control$allow_par) {
        ret <- ensemble_nested_weighted_parallel(
            object         = object,
            loadings       = loadings,
            scale_loadings = scale_loadings,
            loading_method = loading_method,
            keep_submodels = keep_submodels,
            model_ids      = model_ids,
            control        = control
        )
    } else {
        ret <- ensemble_nested_weighted_sequential(
            object         = object,
            loadings       = loadings,
            scale_loadings = scale_loadings,
            loading_method = loading_method,
            keep_submodels = keep_submodels,
            model_ids      = model_ids,
            control        = control
        )
    }

}

# *** PARALLEL *** ----

ensemble_nested_weighted_parallel <- function(object,
                                              loadings,
                                              scale_loadings = TRUE,
                                              loading_method = c("lowest_rmse", "sequential"),
                                              keep_submodels = TRUE,
                                              model_ids = NULL,
                                              control = control_nested_fit()) {

    t1 <- Sys.time()

    # Parallel Detection
    is_par_setup <- foreach::getDoParWorkers() > 1

    # If parallel processing is not set up, set up parallel backend
    par_setup_info <- setup_parallel_processing(control, is_par_setup, t1)
    clusters_made  <- par_setup_info$clusters_made
    cl             <- par_setup_info$cl

    # Setup Foreach
    `%op%` <- get_operator(allow_par = control$allow_par)

    # HANDLE INPUTS ----

    id_text <- attr(object, "id")

    object <- object %>%
        dplyr::select(dplyr::one_of(id_text), ".actual_data", ".future_data", ".splits", ".modeltime_tables")

    conf_interval <- attr(object, "conf_interval")

    metric_set    <- attr(object, "metric_set")

    # SETUP ITERABLES ----

    model_list  = object$.modeltime_tables

    splits_list = object$.splits

    actual_list = object$.actual_data

    id_vec      = object[[id_text]]


    # BEGIN LOOP -----
   if (control$verbose) {
        t <- Sys.time()
        message(stringr::str_glue(" Beginning Parallel Loop | {round(t-t1, 3)} seconds"))
    }

    ret <- foreach::foreach(
        x                   = model_list,
        s                   = splits_list,
        d                   = actual_list,
        id                  = id_vec,
        .inorder            = TRUE,
        .packages           = control$packages,
        .verbose            = FALSE
    ) %op% {

        # Make Ensemble -----

        # Isolate model ids
        ensem <- x
        if (!is.null(model_ids)) {
            ensem <- x %>%
                dplyr::filter(.model_id %in% model_ids)
        }

        # Sort loadings if needed
        if (loading_method == "lowest_rmse") {
            loadings <- x %>%
                modeltime::modeltime_accuracy() %>%
                tibble::rowid_to_column("..rowid") %>%
                dplyr::arrange(rmse) %>%
                dplyr::mutate(.loadings = loadings) %>%
                dplyr::arrange(..rowid) %>%
                dplyr::pull(.loadings)
        }

        # Make Ensem
        ensem <- ensem %>%
            ensemble_weighted(
                loadings = loadings,
                scale_loadings = scale_loadings
            )

        new_mod_id <- max(x$.model_id) + 1

        ret <- modeltime_table(ensem) %>%
            dplyr::mutate(.model_id = new_mod_id)


        # Add calibration
        suppressMessages({
            suppressWarnings({
                ret0 <- ret

                tryCatch({
                    co <- utils::capture.output({
                        # Use invisible to suppress print when model fails
                        ret <- ret %>%
                            modeltime_calibrate(dplyr::slice(d, s$idx_test))
                    })

                }, error=function(e){
                    # Return original modeltime table
                    ret <- ret0
                })
            })
        })

        # Test Accuracy ----
        acc_tbl <- NULL
        suppressMessages({
            suppressWarnings({

                tryCatch({
                    co <- utils::capture.output({
                        # Use invisible to suppress print when model fails
                        acc_tbl <- modeltime_accuracy(ret, metric_set = metric_set) %>%
                            tibble::add_column(!! id_text := id, .before = 1)
                    })

                }, error=function(e) {

                    # Do nothing

                })
            })
        })

        if (is.null(acc_tbl)) {
            acc_tbl <- tibble::tibble(
                !! id_text := id,
                .model_id   = ret$.model_id,
                .model_desc = "NULL"
            )
        }


        # Future Forecast ----
        fcast_tbl <- NULL
        suppressMessages({
            suppressWarnings({

                tryCatch({

                    fcast_tbl <- modeltime_forecast(
                        object        = ret,
                        new_data      = dplyr::slice(d, s$idx_test),
                        actual_data   = d,
                        conf_interval = conf_interval
                    ) %>%
                        tibble::add_column(!! id_text := id, .before = 1)

                }, error=function(e){

                    # Return nothing
                })
            })
        })


        # If keep models, merge model list
        if (keep_submodels) {
            ret <- dplyr::bind_rows(x, ret)
        }

        return(list(
            mdl_time_tbl = ret,
            acc_tbl      = acc_tbl,
            # error_list   = error_list,
            fcast_tbl    = fcast_tbl
        ))

    } # END LOOP | returns ret

    # CONSOLIDATE RESULTS

    mdl_time_list <- ret %>% purrr::map(purrr::pluck("mdl_time_tbl"))
    # error_list    <- ret %>% purrr::map(purrr::pluck("error_list"))
    acc_list      <- ret %>% purrr::map(purrr::pluck("acc_tbl"))
    fcast_list    <- ret %>% purrr::map(purrr::pluck("fcast_tbl"))

    # FORMAT RESULTS ----

    nested_modeltime <- object %>%
        dplyr::mutate(.modeltime_tables = mdl_time_list)

    # error_tbl <- error_list %>% dplyr::bind_rows()
    # if (nrow(error_tbl) > 0) {
    #     error_tbl <- error_tbl %>%
    #         tidyr::drop_na(.error_desc)
    # }

    acc_tbl   <- acc_list %>% dplyr::bind_rows()
    fcast_tbl <- fcast_list %>% dplyr::bind_rows()

    if (keep_submodels) {
        acc_tbl_old <- attr(object, "accuracy_tbl")
        acc_tbl <- dplyr::bind_rows(acc_tbl_old, acc_tbl) %>%
            dplyr::arrange(.model_id) %>%
            dplyr::arrange(!! as.name(id_text))

        fcast_tbl_old <- attr(object, "test_forecast_tbl")
        fcast_tbl <- dplyr::bind_rows(fcast_tbl_old, fcast_tbl) %>%
            dplyr::arrange(!! as.name(id_text), .key, .model_id)
    }

    # Finish Parallel Backend ----
    #   Close clusters if we set up internally.
    finish_parallel_processing(control, clusters_made, cl, t1)

    # FINISH TIMING ----
    t2 <- Sys.time()

    time_elapsed <- difftime(t2, t1, units = "auto") %>%
        utils::capture.output() %>%
        stringr::str_remove("Time difference of ")

    if (control$verbose) cli::cli_inform(stringr::str_glue("Finished in: {time_elapsed}."))

    # STRUCTURE ----

    # attr(nested_modeltime, "error_tbl")           <- error_tbl
    attr(nested_modeltime, "accuracy_tbl")        <- acc_tbl
    attr(nested_modeltime, "test_forecast_tbl")   <- fcast_tbl




    # if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
    #     rlang::warn("Some models had errors during fitting. Use `extract_nested_error_report()` to review errors.")
    # }


    return(nested_modeltime)



}

# *** SEQUENTIAL *** ----
ensemble_nested_weighted_sequential <- function(object,
                                                loadings,
                                                scale_loadings = TRUE,
                                                loading_method = c("lowest_rmse", "sequential"),
                                                keep_submodels = TRUE,
                                                model_ids = NULL,
                                                control = control_nested_fit()) {

    t1 <- Sys.time()

    # HANDLE INPUTS ----

    id_text <- attr(object, "id")

    object <- object %>%
        dplyr::select(dplyr::one_of(id_text), ".actual_data", ".future_data", ".splits", ".modeltime_tables")

    id_expr <- rlang::sym(id_text)

    n_ids   <- nrow(object)

    x_expr  <- rlang::sym(".modeltime_tables")

    d_expr  <- rlang::sym(".actual_data")

    s_expr <- rlang::sym(".splits")

    conf_interval <- attr(object, "conf_interval")
    metric_set    <- attr(object, "metric_set")


    # SETUP LOGGING ENV ----
    logging_env <- rlang::env(
        fcast_tbl = tibble::tibble(),
        acc_tbl   = tibble::tibble()

    )

    # SETUP PROGRESS

    if (!control$verbose) cli::cli_progress_bar("Fitting models on actual data...", total = nrow(object), .envir = logging_env)

    # LOOP LOGIC ----

    nested_modeltime <- object %>%
        tibble::rowid_to_column(var = '..rowid') %>%
        dplyr::mutate(
            .modeltime_tables = purrr::pmap(.l = list(x = !! x_expr, d = !! d_expr, s = !! s_expr, id = !! id_expr, i = ..rowid),
                                            .f = function(x, d, s, id, i) {

                if (control$verbose) cli::cli_alert_info(stringr::str_glue("[{i}/{n_ids}] Starting Modeltime Table: ID {id}..."))

                # Make Ensemble -----

                # Isolate model ids
                ensem <- x
                if (!is.null(model_ids)) {
                    ensem <- x %>%
                        dplyr::filter(.model_id %in% model_ids)
                }

                # Sort loadings if needed
                if (loading_method == "lowest_rmse") {
                    loadings <- x %>%
                        modeltime::modeltime_accuracy() %>%
                        tibble::rowid_to_column("..rowid") %>%
                        dplyr::arrange(rmse) %>%
                        dplyr::mutate(.loadings = loadings) %>%
                        dplyr::arrange(..rowid) %>%
                        dplyr::pull(.loadings)
                }



                # Make Ensem
                ensem <- ensem %>%
                    ensemble_weighted(
                        loadings       = loadings,
                        scale_loadings = scale_loadings
                    )

                new_mod_id <- max(x$.model_id) + 1

                ret <- modeltime_table(ensem) %>%
                    dplyr::mutate(.model_id = new_mod_id)

                # Add calibration
                suppressMessages({
                    suppressWarnings({
                        ret0 <- ret

                        tryCatch({
                            co <- utils::capture.output({
                                # Use invisible to suppress print when model fails
                                ret <- ret %>%
                                    modeltime_calibrate(dplyr::slice(d, s$idx_test))
                            })

                        }, error=function(e){
                            # Return original modeltime table
                            ret <- ret0
                        })
                    })
                })

                # Test Accuracy ----
                acc_tbl <- NULL
                suppressMessages({
                    suppressWarnings({

                        tryCatch({
                            co <- utils::capture.output({
                                # Use invisible to suppress print when model fails
                                acc_tbl <- modeltime_accuracy(ret, metric_set = metric_set) %>%
                                    tibble::add_column(!! id_text := id, .before = 1)
                            })

                            logging_env$acc_tbl <- dplyr::bind_rows(logging_env$acc_tbl, acc_tbl)

                        }, error=function(e) {

                            # Do nothing

                        })
                    })
                })

                if (is.null(acc_tbl)) {
                    acc_tbl <- tibble::tibble(
                        !! id_text := id,
                        .model_id   = ret$.model_id,
                        .model_desc = "NULL"
                    )
                }

                # Future Forecast ----
                fcast_tbl <- NULL
                suppressMessages({
                    suppressWarnings({

                        tryCatch({

                            fcast_tbl <- modeltime_forecast(
                                object        = ret,
                                new_data      = dplyr::slice(d, s$idx_test),
                                actual_data   = d,
                                conf_interval = conf_interval
                            ) %>%
                                tibble::add_column(!! id_text := id, .before = 1)

                            logging_env$fcast_tbl <- dplyr::bind_rows(logging_env$fcast_tbl, fcast_tbl)

                        }, error=function(e){

                            # Return nothing
                        })
                    })
                })

                # If keep models, merge model list
                if (keep_submodels) {
                    ret <- dplyr::bind_rows(x, ret)
                }

                # Finish ----

                if (control$verbose) cli::cli_alert_success(stringr::str_glue("[{i}/{n_ids}] Finished Modeltime Table: ID {id}"))
                if (control$verbose) cat("\n")

                if (!control$verbose) cli::cli_progress_update(.envir = logging_env)

                return(ret)
            })
        ) %>%
        dplyr::select(-..rowid)

    if (!control$verbose) cli::cli_progress_done(.envir = logging_env)

    t2 <- Sys.time()

    time_elapsed <- difftime(t2, t1, units = "auto") %>%
        utils::capture.output() %>%
        stringr::str_remove("Time difference of ")

    if (control$verbose) cli::cli_inform(stringr::str_glue("Finished in: {time_elapsed}."))

    # STRUCTURE ----

    acc_tbl   <- logging_env$acc_tbl
    fcast_tbl <- logging_env$fcast_tbl

    if (keep_submodels) {
        acc_tbl_old <- attr(object, "accuracy_tbl")
        acc_tbl <- dplyr::bind_rows(acc_tbl_old, acc_tbl) %>%
            dplyr::arrange(.model_id) %>%
            dplyr::arrange(!! as.name(id_text))

        fcast_tbl_old <- attr(object, "test_forecast_tbl")
        fcast_tbl <- dplyr::bind_rows(fcast_tbl_old, fcast_tbl) %>%
            dplyr::arrange(!! as.name(id_text), .key, .model_id)
    }

    # attr(nested_modeltime, "error_tbl")           <- logging_env$error_tbl %>% tidyr::drop_na(.error_desc)
    attr(nested_modeltime, "accuracy_tbl")      <- acc_tbl
    attr(nested_modeltime, "test_forecast_tbl") <- fcast_tbl


    # if (nrow(attr(nested_modeltime, "error_tbl")) > 0) {
    #     rlang::warn("Some models had errors during fitting. Use `extract_nested_error_report()` to review errors.")
    # }


    return(nested_modeltime)
}
