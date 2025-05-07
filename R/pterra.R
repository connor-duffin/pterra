#' Simulate from the posterior predictive distribution with {terra}.
#'
#' Utilizing a SpatRaster to hold the covariates, simulate the posterior
#' predictive distribution (PPD) of a model and return a SpatRaster whose layers
#' are draws from the PPD.
#'
#' @param raster A `SpatRaster` that serves as the covariate data for
#'   prediction. Passed to [terra::predict()].
#' @param model A fitted Stan model object, typically returned from a cmdstanr
#'   modeling workflow.
#' @param draws Integer specifying the number of posterior draws to use in
#'   prediction. Defaults to 1.
#'
#' @return A `SpatRaster` object with posterior predictive values.
#' @export
terra_posterior_predict <- function(raster, model, draws = 1) {
  # TODO: add in default arguments for terra::predict
  # TODO: add in tests from bayesian scripts
  result <- terra::predict(
    raster,
    model,
    fun = .stan_posterior_predict,
    draws = draws,
  )

  return(result)
}

#' Parallel simulation from the posterior predictive distribution with {terra}.
#'
#' Reads in input raster data and writes to individual output rasters.
terra_posterior_predict_par <- function(file_raster,
                                        model,
                                        draws = 1,
                                        cores = 1) {
  draws_each <- .get_draws_each(draws, cores)
  stopifnot(length(draws_each), cores)

  # TODO: figure out a way to add default arguments
  parallel::mclapply(
    1:cores,
    function(i) {
      raster <- terra::rast(file_raster)
      file_out <- glue::glue("rast-{i}-prediction.tif")
      terra::predict(
        raster,
        model,
        fun = .stan_posterior_predict,
        draws = draws_each[i],
        filename = file_out,
        overwrite = TRUE
      )

      return(file_out)
    },
    mc.cores = cores
  )
}

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

#' Get draws for each core across the processes
.get_draws_each <- function(draws, cores) {
  draws_each <- rep(draws %/% cores, cores)
  extra <- draws %% cores
  if (extra > 0) {
    draws_each[1:extra] <- draws_each[1:extra] + 1
  }

  return(draws_each)
}
