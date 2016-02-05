function(trees = 6500, shrinkage = 0.002, ...) {
  options <- list(
    'gbm'
    , distribution        = 'bernoulli'
    , number_of_trees     = trees
    , shrinkage_factor    = shrinkage
    , depth               = 4
    , min_observations    = 6
    , train_fraction      = 1
    , bag_fraction        = 0.5
    , cv                  = TRUE
    , cv_folds            = 5
    , number_of_cores     = 10
    , perf_method         = 'cv'
    , prediction_type     = 'response'
  )

  mungebits2::list_merge(options, list(...))
}

