% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nested_ensemble_weighted.R
\name{ensemble_nested_weighted}
\alias{ensemble_nested_weighted}
\title{Nested Ensemble Weighted}
\usage{
ensemble_nested_weighted(
  object,
  loadings,
  scale_loadings = TRUE,
  metric = "rmse",
  keep_submodels = TRUE,
  model_ids = NULL,
  control = control_nested_fit()
)
}
\arguments{
\item{object}{A nested modeltime object (inherits class \code{nested_mdl_time})}

\item{loadings}{A vector of weights corresponding to the loadings}

\item{scale_loadings}{If TRUE, divides by the sum of the loadings
to proportionally weight the submodels.}

\item{metric}{The accuracy metric to rank models by the test accuracy table.
Loadings are then applied in the order from best to worst models.
Default: \code{"rmse"}.}

\item{keep_submodels}{Whether or not to keep the submodels in the
nested modeltime table results}

\item{model_ids}{A vector of id's (\code{.model_id}) identifying which submodels to
use in the ensemble.}

\item{control}{Controls various aspects of the ensembling process. See \code{modeltime::control_nested_fit()}.}
}
\value{
The nested modeltime table with an ensemble model added.
}
\description{
Creates an Ensemble Model using Weighted Averaging in the
Modeltime Nested Forecasting Workflow.
}
\details{
If we start with a nested modeltime table, we can add ensembles.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{nested_modeltime_tbl

# Nested Modeltime Table
Trained on: .splits | Model Errors: [0]
# A tibble: 2 x 5
  id    .actual_data       .future_data      .splits         .modeltime_tables
  <fct> <list>             <list>            <list>          <list>
1 1_1   <tibble [104 x 2]> <tibble [52 x 2]> <split [52|52]> <mdl_time_tbl [2 x 5]>
2 1_3   <tibble [104 x 2]> <tibble [52 x 2]> <split [52|52]> <mdl_time_tbl [2 x 5]>
}\if{html}{\out{</div>}}

An ensemble can be added to a Nested modeltime table.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{ensem <- nested_modeltime_tbl \%>\%
    ensemble_nested_weighted(
        loadings       = c(2,1),
        control        = control_nested_fit(allow_par = FALSE, verbose = TRUE)
    )
}\if{html}{\out{</div>}}

We can then verify the model has been added.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{ensem \%>\% extract_nested_modeltime_table()
}\if{html}{\out{</div>}}

This produces an ensemble .model_id 3, which is an ensemble of the first two models.

\if{html}{\out{<div class="sourceCode">}}\preformatted{# A tibble: 4 x 6
  id    .model_id .model         .model_desc                   .type .calibration_data
  <fct>     <dbl> <list>         <chr>                         <chr> <list>
1 1_3           1 <workflow>     PROPHET                       Test  <tibble [52 x 4]>
2 1_3           2 <workflow>     XGBOOST                       Test  <tibble [52 x 4]>
3 1_3           3 <ensemble [2]> ENSEMBLE (WEIGHTED): 2 MODELS Test  <tibble [52 x 4]>
}\if{html}{\out{</div>}}

We can verify the loadings have been applied correctly. Note that the loadings will be
applied based on the model with the lowest RMSE.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{ensem \%>\%
    extract_nested_modeltime_table(1) \%>\%
    slice(3) \%>\%
    pluck(".model", 1)
}\if{html}{\out{</div>}}

Note that the xgboost model gets the 66\% loading and prophet gets 33\% loading.
This is because xgboost has the lower RMSE in this case.

\if{html}{\out{<div class="sourceCode r">}}\preformatted{-- Modeltime Ensemble -------------------------------------------
    Ensemble of 2 Models (WEIGHTED)

# Modeltime Table
# A tibble: 2 x 6
  .model_id .model     .model_desc .type .calibration_data .loadings
      <int> <list>     <chr>       <chr> <list>                <dbl>
1         1 <workflow> PROPHET     Test  <tibble [52 x 4]>     0.333
2         2 <workflow> XGBOOST     Test  <tibble [52 x 4]>     0.667
}\if{html}{\out{</div>}}
}
