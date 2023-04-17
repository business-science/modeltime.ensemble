

#' Nested Ensemble Weighted
#'
#' Creates an Ensemble Model using Weighted Averaging in the
#' Modeltime Nested Forecasting Workflow.
#'
#' @inheritParams ensemble_nested_average
#' @inheritParams ensemble_weighted
#' @param metric The accuracy metric to rank models by the test accuracy table.
#'  Loadings are then applied in the order from best to worst models.
#'  Default: `"rmse"`.
#'
#' @returns
#' The nested modeltime table with an ensemble model added.
#'
#'
#' @details
#'
#' If we start with a nested modeltime table, we can add ensembles.
#'
#' ```r
#' nested_modeltime_tbl
#'
#' # Nested Modeltime Table
#' Trained on: .splits | Model Errors: [0]
#' # A tibble: 2 x 5
#'   id    .actual_data       .future_data      .splits         .modeltime_tables
#'   <fct> <list>             <list>            <list>          <list>
#' 1 1_1   <tibble [104 x 2]> <tibble [52 x 2]> <split [52|52]> <mdl_time_tbl [2 x 5]>
#' 2 1_3   <tibble [104 x 2]> <tibble [52 x 2]> <split [52|52]> <mdl_time_tbl [2 x 5]>
#' ```
#'
#' An ensemble can be added to a Nested modeltime table.
#'
#' ``` r
#' ensem <- nested_modeltime_tbl %>%
#'     ensemble_nested_weighted(
#'         loadings       = c(2,1),
#'         control        = control_nested_fit(allow_par = FALSE, verbose = TRUE)
#'     )
#' ```
#'
#' We can then verify the model has been added.
#'
#' ``` r
#' ensem %>% extract_nested_modeltime_table()
#' ```
#'
#' This produces an ensemble .model_id 3, which is an ensemble of the first two models.
#'
#' ```
#' # A tibble: 4 x 6
#'   id    .model_id .model         .model_desc                   .type .calibration_data
#'   <fct>     <dbl> <list>         <chr>                         <chr> <list>
#' 1 1_3           1 <workflow>     PROPHET                       Test  <tibble [52 x 4]>
#' 2 1_3           2 <workflow>     XGBOOST                       Test  <tibble [52 x 4]>
#' 3 1_3           3 <ensemble [2]> ENSEMBLE (WEIGHTED): 2 MODELS Test  <tibble [52 x 4]>
#' ```
#'
#' We can verify the loadings have been applied correctly. Note that the loadings will be
#' applied based on the model with the lowest RMSE.
#'
#' ``` r
#' ensem %>%
#'     extract_nested_modeltime_table(1) %>%
#'     slice(3) %>%
#'     pluck(".model", 1)
#' ```
#'
#' Note that the xgboost model gets the 66% loading and prophet gets 33% loading.
#' This is because xgboost has the lower RMSE in this case.
#'
#' ```r
#' -- Modeltime Ensemble -------------------------------------------
#'     Ensemble of 2 Models (WEIGHTED)
#'
#' # Modeltime Table
#' # A tibble: 2 x 6
#'   .model_id .model     .model_desc .type .calibration_data .loadings
#'       <int> <list>     <chr>       <chr> <list>                <dbl>
#' 1         1 <workflow> PROPHET     Test  <tibble [52 x 4]>     0.333
#' 2         2 <workflow> XGBOOST     Test  <tibble [52 x 4]>     0.667
#' ```
#' @export
ensemble_nested_weighted <- function(object,
                                     loadings,
                                     scale_loadings = TRUE,
                                     metric = "rmse",
                                     keep_submodels = TRUE,
                                     model_ids = NULL,
                                     control = control_nested_fit()) {

    # Check metric is in metric set
    metric_set_tbl <- tibble::as_tibble(attr(object, "metric_set"))
    if (!metric %in% metric_set_tbl$metric) {
        available_metrics_text <- stringr::str_c(metric_set_tbl$metric, collapse = ", ")
        rlang::abort(stringr::str_glue("`metric = {metric}` is not one of the available metrics. Choose from one of: {available_metrics_text}}"))
    }

    UseMethod("ensemble_nested_weighted", object)

}

#' @export
ensemble_nested_weighted.nested_mdl_time <- function(object,
                                                     loadings,
                                                     scale_loadings = TRUE,
                                                     metric = "rmse",
                                                     keep_submodels = TRUE,
                                                     model_ids = NULL,
                                                     control = control_nested_fit()) {

    # Parallel or Sequential
    if ((control$cores > 1) && control$allow_par) {
        ret <- ensemble_nested_weighted_parallel(
            object          = object,
            loadings        = loadings,
            scale_loadings  = scale_loadings,
            metric          = metric,
            keep_submodels  = keep_submodels,
            model_ids       = model_ids,
            control         = control
        )
    } else {
        ret <- ensemble_nested_weighted_sequential(
            object          = object,
            loadings        = loadings,
            scale_loadings  = scale_loadings,
            metric          = metric,
            keep_submodels  = keep_submodels,
            model_ids       = model_ids,
            control         = control
        )
    }

}

# *** PARALLEL *** ----

ensemble_nested_weighted_parallel <- function(object,
                                              loadings,
                                              scale_loadings = TRUE,
                                              metric = "rmse",
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

    metric_set <- attr(object, "metric_set")

    metric_set_tbl <- tibble::as_tibble(metric_set)

    direction <- metric_set_tbl %>%
        dplyr::filter(metric == !!metric) %>%
        dplyr::pull(direction) %>%
        purrr::pluck(1)

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
        .export             = c("generate_ensemble_weighted"),
        .verbose            = FALSE
    ) %op% {

        # Make Ensemble -----
        safe_ensemble_weighted <- purrr::safely(generate_ensemble_weighted, otherwise = NULL, quiet = TRUE)

        ret_list <- safe_ensemble_weighted(
            modeltime_table = x,
            model_ids       = model_ids,
            loadings        = loadings,
            scale_loadings  = scale_loadings,
            metric          = metric,
            metric_set      = metric_set,
            direction       = direction
        )

        ret <- ret_list %>% purrr::pluck("result")

        err <- ret_list %>% purrr::pluck("error", 1)

        mod_id <- max(x$.model_id) + 1

        error_tbl <- tibble::tibble(
            !! id_text := id,
            .model_id   = mod_id,
            .model_desc = "ENSEMBLE WEIGHTED",
            .error_desc = ifelse(is.null(err), NA_character_, err)
        )

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
            error_tbl    = error_tbl,
            fcast_tbl    = fcast_tbl
        ))

    } # END LOOP | returns ret

    # CONSOLIDATE RESULTS

    mdl_time_list <- ret %>% purrr::map(purrr::pluck("mdl_time_tbl"))
    error_list    <- ret %>% purrr::map(purrr::pluck("error_tbl"))
    acc_list      <- ret %>% purrr::map(purrr::pluck("acc_tbl"))
    fcast_list    <- ret %>% purrr::map(purrr::pluck("fcast_tbl"))

    # FORMAT RESULTS ----

    nested_modeltime <- object %>%
        dplyr::mutate(.modeltime_tables = mdl_time_list)

    error_tbl <- error_list %>%
        dplyr::bind_rows() %>%
        tidyr::drop_na(.error_desc)

    if (nrow(error_tbl) > 0) {
        rlang::warn("Some models had errors during fitting. Use `extract_nested_error_report()` to review errors.")
        error_tbl <- attr(nested_modeltime, "error_tbl") %>%
            dplyr::bind_rows(error_tbl)
    }

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

    attr(nested_modeltime, "error_tbl")           <- error_tbl
    attr(nested_modeltime, "accuracy_tbl")        <- acc_tbl
    attr(nested_modeltime, "test_forecast_tbl")   <- fcast_tbl

    return(nested_modeltime)



}

# *** SEQUENTIAL *** ----
ensemble_nested_weighted_sequential <- function(object,
                                                loadings,
                                                scale_loadings = TRUE,
                                                metric = "rmse",
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

    metric_set_tbl <- tibble::as_tibble(metric_set)

    direction <- metric_set_tbl %>%
        dplyr::filter(metric == !!metric) %>%
        dplyr::pull(direction) %>%
        purrr::pluck(1)

    # SETUP LOGGING ENV ----
    logging_env <- rlang::env(
        fcast_tbl = tibble::tibble(),
        acc_tbl   = tibble::tibble(),
        error_tbl = tibble::tibble()

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
                safe_ensemble_weighted <- purrr::safely(generate_ensemble_weighted, otherwise = NULL, quiet = TRUE)

                ret_list <- safe_ensemble_weighted(
                    modeltime_table = x,
                    model_ids       = model_ids,
                    loadings        = loadings,
                    scale_loadings  = scale_loadings,
                    metric          = metric,
                    metric_set      = metric_set,
                    direction       = direction
                )

                ret <- ret_list %>% purrr::pluck("result")

                err <- ret_list %>% purrr::pluck("error", 1)

                mod_id <- max(x$.model_id) + 1

                error_tbl <- tibble::tibble(
                    !! id_text := id,
                    .model_id   = mod_id,
                    .model_desc = "ENSEMBLE WEIGHTED",
                    .error_desc = ifelse(is.null(err), NA_character_, err)
                )
                logging_env$error_tbl <- dplyr::bind_rows(logging_env$error_tbl, error_tbl)

                if (control$verbose) {
                    if (!is.null(err)) {
                        cli::cli_alert_danger("Model {mod_id} Failed {error_tbl$.model_desc}: {err}")
                    } else {
                        cli::cli_alert_success("Model {mod_id} Passed {error_tbl$.model_desc}.")
                    }
                }


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
    err_tbl   <- logging_env$error_tbl %>%
        tidyr::drop_na(.error_desc)

    if (nrow(err_tbl) > 0) {
        rlang::warn("Some models had errors during fitting. Use `extract_nested_error_report()` to review errors.")

        err_tbl <- attr(nested_modeltime, "error_tbl") %>%
            dplyr::bind_rows(err_tbl)

    }

    if (keep_submodels) {
        acc_tbl_old <- attr(object, "accuracy_tbl")
        acc_tbl <- dplyr::bind_rows(acc_tbl_old, acc_tbl) %>%
            dplyr::arrange(.model_id) %>%
            dplyr::arrange(!! as.name(id_text))

        fcast_tbl_old <- attr(object, "test_forecast_tbl")
        fcast_tbl <- dplyr::bind_rows(fcast_tbl_old, fcast_tbl) %>%
            dplyr::arrange(!! as.name(id_text), .key, .model_id)
    }

    attr(nested_modeltime, "error_tbl")         <- err_tbl
    attr(nested_modeltime, "accuracy_tbl")      <- acc_tbl
    attr(nested_modeltime, "test_forecast_tbl") <- fcast_tbl


    return(nested_modeltime)
}


# HELPERS ----

generate_ensemble_weighted <- function(modeltime_table, model_ids,
                                       loadings, scale_loadings,
                                       metric, metric_set, direction) {

    # Make Ensemble -----

    # stop("this is a test.")
    x     <- modeltime_table

    # Isolate model ids
    ensem <- x
    if (!is.null(model_ids)) {
        ensem <- x %>%
            dplyr::filter(.model_id %in% model_ids)
    }

    # Filter out NULL models
    ensem <- ensem %>%
        dplyr::filter(!purrr::map_lgl(.model, is.null))

    # Check Loadings
    loading_len  <- length(loadings)
    submodel_len <- nrow(ensem)

    if (submodel_len == 0) rlang::abort("No submodels detected.")

    # Trim loadings if necessary
    if (submodel_len < loading_len) {
        loadings <- loadings[1:submodel_len]
    }

    # Extend loadings if necessary
    if (loading_len < submodel_len) {
        loadings <- c(loadings, rep(0, submodel_len - loading_len))
    }

    # Sort loadings
    if (direction == "minimize") {
        loadings <- ensem %>%
            modeltime::modeltime_accuracy(metric_set = metric_set) %>%
            tibble::rowid_to_column("..rowid") %>%

            dplyr::arrange(!! as.name(metric)) %>%
            dplyr::slice(1:length(loadings)) %>%

            dplyr::mutate(.loadings = loadings) %>%
            dplyr::arrange(..rowid) %>%
            dplyr::pull(.loadings)
    } else {
        loadings <- ensem %>%
            modeltime::modeltime_accuracy(metric_set = metric_set) %>%
            tibble::rowid_to_column("..rowid") %>%

            dplyr::arrange(dplyr::desc(!! as.name(metric))) %>%
            dplyr::slice(1:length(loadings)) %>%

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

    return(ret)

}
