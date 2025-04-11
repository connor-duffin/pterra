#' Underlying function to run posterior predictions with terra.
#'
#' @param mod An rstanarm fitted model object.
#' @param dat Input data to fit the model, which then gets passed as 'newdata'
#'   into `rstanarm::posterior_predict`.
#' @param ... Extra arguments passed into `rstanarm::posterior_predict` (such as
#'   'draws').
#' @return A matrix of replicated values with dimensions `nrow(dat)` by `draws`
#'   (note this is the transposed output of `rstanarm::posterior_predict`).
.stan_posterior_predict <- function(mod, dat, ...) {
  yrep <- rstanarm::posterior_predict(
    object = mod,
    newdata = dat,
    ...
  )

  return(t(yrep))
}

#' Simulate from the posterior predictive distribution with terra.
#'
#' Utilizing a SpatRaster to hold the covariates, simulate the posterior
#' predictive distribution (PPD) of a model and return a SpatRaster whose layers
#' are draws from the PPD.
#' 
#' @param object A spatial object (e.g., `SpatRaster`) that serves as the
#'   covariate data for prediction. Passed to [terra::predict()].
#' @param model A fitted Stan model object, typically returned from a cmdstanr
#'   modeling workflow.
#' @param draws Integer specifying the number of posterior draws to use in
#'   prediction. Defaults to 1.
#' @param cores Integer specifying the number of CPU cores to use. Currently,
#'   only `cores = 1` is implemented.
#'
#' @return A `SpatRaster` object with posterior predictive values.
#' @export
terra_posterior_predict <- function(object, model, draws = 1, cores = 1) {
  # TODO: add in default arguments for terra::predict
  # TODO: add in tests from bayesian scripts
  if (cores > 1) {
    cl <- parallel::makeCluster(cores)
    use_cluster <- TRUE
  } else {
    use_cluster <- FALSE
  }

  if (use_cluster) {
    # TODO: fix parallelization
    ## draws_each <- floor(draws / cores)
    ## parallel::clusterExport(cl, c("object", "model"))
    ## result <- terra::rast(
    ##   parallel::parLapply(
    ##     cl,
    ##     1:cores,
    ##     terra::predict,
    ##     object = object,
    ##     model = model,
    ##     fun = .stan_posterior_predict,
    ##     draws = draws_each
    ##   )
    ## )
    ## parallel::stopCluster(cl)
  } else {
    result <- terra::predict(
      object,
      model,
      fun = .stan_posterior_predict,
      draws = draws
    )
  }

  return(result)
}

