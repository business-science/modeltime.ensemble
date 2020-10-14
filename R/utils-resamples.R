
# RESAMPLE UTILITIES -----

#' Unnests the Results of Modeltime Fit Resamples
#'
#' An internal function used by [modeltime_resample_accuracy()].
#'
#' @param object A Modeltime Table that has a column '.resample_results'
#'
#' @return
#' Tibble with columns for '.row_id', '.resample_id', '.model_id', '.model_desc', '.pred',
#' '.row', and actual value name from the data set
#'
#' @details
#'
#' Data Columns:
#' - `.row_id` - A unique identifier to compare observations. Used in `ensemble_model_spec()` to compare observations by `.model_id`.
#' - `.resample_id` - A unique identifier given to the resample iteration.
#' - `.model_id` and `.model_desc` - Modeltime Model ID and Description
#' - `.pred`, `.row`, and actual value column (name changes to name in dataset)
#'
#' @export
unnest_resamples <- function(object) {

    # Checks
    if (!inherits(object, "data.frame")) rlang::abort("object must be a data.frame")
    if (!".resample_results" %in% names(object)) rlang::abort("object must contain a column, '.resample_results'. Try using `modeltime_fit_resamples()` first. ")

    # Unnest
    object %>%
        dplyr::select(-.model) %>%
        tidyr::unnest(.resample_results) %>%
        dplyr::select(.model_id, .model_desc, .predictions) %>%

        # Add .resample_id
        dplyr::group_split(.model_id) %>%
        purrr::map( tibble::rowid_to_column, var = ".resample_id") %>%
        dplyr::bind_rows() %>%

        # Add .row_id - Needed to compare observations between models
        tidyr::unnest(.predictions) %>%
        dplyr::group_split(.model_id) %>%
        purrr::map( tibble::rowid_to_column, var = ".row_id") %>%
        dplyr::bind_rows()
}
