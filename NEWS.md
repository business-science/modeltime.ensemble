# modeltime.ensemble 0.2.0

__Stacked Ensembles (Breaking Changes)__

The process for creating staacked ensembles is split into 2 steps:

- Step 1: Use `modeltime_fit_resamples()` to generate resampled predictions
- Step 2: Use `ensemble_model_spec()` to apply stacking using a `model_spec`

# modeltime.ensemble 0.1.0

* Initial release of `modeltime.ensemble`.
