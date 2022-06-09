#  modeltime.ensemble 1.0.1

### Fixes

- Updates for `hardhat 1.0.0`

#  modeltime.ensemble 1.0.0

## NEW Nested Modeltime Ensembles

In `modeltime` 1.0.0, we introduced Nested Forecasting as a way to forecast many time series iteratively. In `modeltime.ensemble` 1.0.0, we introduce nested ensembles that can improve forecasting performance and be applied to many time series iteratively. We have added:

- `ensemble_nested_average()`: Apply average ensembles iteratively
- `ensemble_nested_weighted()`: Apply weighted ensembles iteratively


## New Vignette (Nested Ensembles)

- [Nested Ensembles](https://business-science.github.io/modeltime.ensemble/articles/nested-ensembles.html)

# modeltime.ensemble 0.4.2

### Compatibility with `modeltime` 0.7.0. 

- __Calibration__: Added "id" feature to enable accuracy and confidence intervals by time series ID. 

# modeltime.ensemble 0.4.1

- Improvements for parallel processing during refitting (available in `modeltime` 0.6.0). 
- Requires `modeltime` 0.6.0 and `parsnip` 0.1.6 to align with [xgboost upgrades](https://github.com/business-science/modeltime/issues/107). 


# modeltime.ensemble 0.4.0

__Recursive Ensembles__

- `recursive()` - The `recursive()` function is extended to recursive ensembles for both single time series and multiple time series models (panel data).
- ["Forecasting with Recursive Ensembles"](https://business-science.github.io/modeltime.ensemble/articles/recursive-ensembles.html) - A new forecasting vignette for using `recurive()` with ensembles.

__Fixes__

- `modeltime_forecast()` now returns `NA` when missing values are present in the sub-model predictions.  

# modeltime.ensemble 0.3.0

__Panel Data__

- Improvements made to `ensemble_average()`, `ensemble_weighted()` and `ensemble_model_spec()` to support _Panel Data_ (i.e. when data sets with multiple time series groups that have possibly overlapping time stamps). 

__Changes__

- `modeltime.ensemble` now depends on `modeltime.resample` for the `modeltime_fit_resamples()` functionality.
- `modeltime_fit_resamples()` moved to a new package `modeltime.resample`.
- `ensemble_weighted()`: Now removes models that have no weight (e.g. loading = 0). This speeds up refitting.  

# modeltime.ensemble 0.2.0

__Stacked Ensembles (Breaking Changes)__

The process for creating stacked ensembles is split into 2 steps:

- Step 1: Use `modeltime_fit_resamples()` to generate resampled predictions
- Step 2: Use `ensemble_model_spec()` to apply stacking using a `model_spec`

Note - `modeltime_refit(stacked_ensemble)` is still one step, which is the best way to handle refitting since multiple stacked models may have different submodel compositions. An additional argument, `resamples` can be provided to train stacked ensembles made with `ensemble_model_spec()`.

# modeltime.ensemble 0.1.0

* Initial release of `modeltime.ensemble`.
