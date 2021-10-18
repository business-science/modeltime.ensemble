

#' Nested Ensemble Average
#'
#' Creates an Ensemble Model using Mean/Median Averaging in the
#' Modeltime Nested Forecasting Workflow.
#'
#' @param object A nested modeltime object (inherits class `nested_mdl_time`)
#' @param type One of "mean" for mean averaging or "median" for median averaging
#' @param keep_submodels Whether or not to keep the submodels in the
#'  nested modeltime table results
#' @param model_ids A vector of id's (`.model_id`) identifying which submodels to
#'  use in the ensemble.
#' @param control Controls various aspects of the ensembling process. See [control_nested_fit()].
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
#'     ensemble_nested_average(
#'         type           = "mean",
#'         keep_submodels = TRUE,
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
#'   id    .model_id .model         .model_desc                 .type .calibration_data
#'   <fct>     <dbl> <list>         <chr>                       <chr> <list>
#' 1 1_1           1 <workflow>     PROPHET                     Test  <tibble [52 x 4]>
#' 2 1_1           2 <workflow>     XGBOOST                     Test  <tibble [52 x 4]>
#' 3 1_1           3 <ensemble [2]> ENSEMBLE (MEAN): 2 MODELS   Test  <tibble [52 x 4]>
#' ```
#'
#' Additional ensembles can be added by simply adding onto the nested modeltime table.
#' Notice that we make use of `model_ids` to make sure it only uses model id's 1 and 2.
#'
#' ``` r
#' ensem_2 <- ensem %>%
#'     ensemble_nested_average(
#'         type           = "median",
#'         keep_submodels = TRUE,
#'         model_ids      = c(1,2),
#'         control        = control_nested_fit(allow_par = FALSE, verbose = TRUE)
#'     )
#' ```
#'
#' This returns a 4th model that is a median ensemble of the first two models.
#'
#' ```
#' ensem_2 %>% extract_nested_modeltime_table()
#' # A tibble: 4 x 6
#'   id    .model_id .model         .model_desc                 .type .calibration_data
#'   <fct>     <dbl> <list>         <chr>                       <chr> <list>
#' 1 1_1           1 <workflow>     PROPHET                     Test  <tibble [52 x 4]>
#' 2 1_1           2 <workflow>     XGBOOST                     Test  <tibble [52 x 4]>
#' 3 1_1           3 <ensemble [2]> ENSEMBLE (MEAN): 2 MODELS   Test  <tibble [52 x 4]>
#' 4 1_1           4 <ensemble [2]> ENSEMBLE (MEDIAN): 2 MODELS Test  <tibble [52 x 4]>
#' ```
#'
#'
#' @export
ensemble_nested_average <- function(object,
                                    type = c("mean", "median"),
                                    keep_submodels = TRUE,
                                    model_ids = NULL,
                                    control = control_nested_fit()) {

    UseMethod("ensemble_nested_average", object)

}

#' @export
ensemble_nested_average.nested_mdl_time <- function(object,
                                                    type = c("mean", "median"),
                                                    keep_submodels = TRUE,
                                                    model_ids = NULL,
                                                    control = control_nested_fit()) {

    # Parallel or Sequential
    if ((control$cores > 1) && control$allow_par) {
        ret <- ensemble_nested_average_parallel(
            object         = object,
            type           = type,
            keep_submodels = keep_submodels,
            model_ids      = model_ids,
            control        = control
        )
    } else {
        ret <- ensemble_nested_average_sequential(
            object         = object,
            type           = type,
            keep_submodels = keep_submodels,
            model_ids      = model_ids,
            control        = control
        )
    }

}

# *** PARALLEL *** ----

ensemble_nested_average_parallel <- function(object,
                                             type = c("mean", "median"),
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

        # Filter out NULL models
        ensem <- ensem %>%
            dplyr::filter(!purrr::map_lgl(.model, is.null))

        # Filter model_ids
        if (!is.null(model_ids)) {
            ensem <- ensem %>%
                dplyr::filter(.model_id %in% model_ids)
        }

        ensem <- ensem %>% ensemble_average(type = type)

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
ensemble_nested_average_sequential <- function(object,
                                               type = c("mean", "median"),
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

                # Filter out NULL models
                ensem <- ensem %>%
                    dplyr::filter(!purrr::map_lgl(.model, is.null))

                print(ensem)

                # Filter model_ids
                if (!is.null(model_ids)) {
                    ensem <- ensem %>%
                        dplyr::filter(.model_id %in% model_ids)
                }

                ensem <- ensem %>% ensemble_average(type = type)

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
