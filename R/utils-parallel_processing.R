# USED TO SET UP THE PARALLEL BACKENDS IF NOT SET UP ALREADY
setup_parallel_processing <- function(control, is_par_setup, t1) {

    clusters_made <- FALSE
    cl            <- NULL

    if ((control$cores > 1) && control$allow_par && (!is_par_setup)){
        if (control$verbose) {
            message(
                stringr::str_glue(" No existing backend detected. It's more efficient to setup a Parallel Backend with `parallel_start()`...")
            )
            message(
                stringr::str_glue(" Starting parallel backend with {control$cores} clusters (cores)...")
            )
        }
        cl <- parallel::makeCluster(control$cores)
        doParallel::registerDoParallel(cl)
        parallel::clusterCall(cl, function(x) .libPaths(x), .libPaths())
        clusters_made <- TRUE

        if (control$verbose) {
            t <- Sys.time()
            message(stringr::str_glue(" Parallel Backend Setup | {round(t-t1, 3)} seconds"))
        }

    } else if (!is_par_setup) {
        # Run sequentially if parallel is not set up, cores == 1 or allow_par == FALSE
        if (control$verbose) message(stringr::str_glue("Running sequential backend. If parallel was intended, set `allow_par = TRUE` and `cores > 1`."))
        foreach::registerDoSEQ()
    } else {
        # Parallel was set up externally by user - Do nothing.
        if (control$verbose) message(stringr::str_glue("Using existing parallel backend with {foreach::getDoParWorkers()} clusters (cores)..."))
    }

    return(list(
        clusters_made = clusters_made,
        cl            = cl
    ))

}

# USED TO SHUT DOWN THE PARALLEL BACKENDS IF WE SET UP
finish_parallel_processing <- function(control, clusters_made, cl, t1) {

    t <- Sys.time()

    if (clusters_made) {
        # We set up parallel processing internally. We should close.
        doParallel::stopImplicitCluster()
        parallel::stopCluster(cl)
        foreach::registerDoSEQ()
        if (control$verbose) {
            message(stringr::str_glue(" Finishing parallel backend. Closing clusters. | {round(t-t1, 3)} seconds)"))
        }
    } else if ((control$cores > 1) && control$allow_par) {
        if (control$verbose) {
            message(stringr::str_glue(" Finishing parallel backend. Clusters are remaining open. | {round(t-t1, 3)} seconds"))
            message(" Close clusters by running: `parallel_stop()`.")
        }
    } else {
        if (control$verbose) {
            message(stringr::str_glue(" Finishing sequential backend. | {round(t-t1, 3)} seconds"))
        }
    }

}

# USED TO SELECT EITHER SEQ OR PAR FOREACH OPERATOR
get_operator <- function(allow_par = TRUE) {
    is_par <- foreach::getDoParWorkers() > 1

    cond <- allow_par && is_par
    if (cond) {
        res <- foreach::`%dopar%`
    } else {
        res <- foreach::`%do%`
    }
    return(res)
}
