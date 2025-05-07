#' Simulate from the posterior predictive distribution with {terra}.
#'
#' Utilizing a `SpatRaster` to hold the covariates, simulate the posterior
#' predictive distribution (PPD) of a model and return a `SpatRaster` whose
#' layers are draws from the PPD.
#'
#' @param raster A `SpatRaster` that serves as the covariate data for
#'   prediction. Passed to [terra::predict()].
#' @param model A fitted Stan model object, typically returned from a {cmdstanr}
#'   modeling workflow.
#' @param draws Integer specifying the number of posterior draws to use in
#'   prediction. Defaults to 1.
#' @param ... Additional arguments passed to [terra::predict()].
#'
#' @return A `SpatRaster` object with one layer per posterior predictive draw.
#'   Layer names are of the form `"sample_1"`, `"sample_2"`, etc.
#' @export
terra_posterior_predict <- function(raster, model, draws = 1, ...) {
  result <- terra::predict(
    raster,
    model,
    fun = .stan_posterior_predict,
    draws = draws,
    ...
  )
  names(result) <- paste0("sample_", seq(1, terra::nlyr(result)))

  return(result)
}

#' Parallel simulation from the posterior predictive distribution with {terra}.
#'
#' Reads in a raster file, performs posterior predictive simulations in
#' parallel, and writes the resulting combined raster to disk. This function is
#' useful for large-scale raster predictions where computation can be split
#' across multiple cores.
#'
#' Each core handles a subset of the posterior draws and writes results to a
#' temporary file, which are then combined and written to the specified output.
#'
#' @param file_raster Path to the input raster file (readable by
#'   [terra::rast()]). This file provides the covariate data for prediction.
#' @param file_out Path to the output raster file where posterior predictive
#'   draws will be written.
#' @param model A fitted Stan model object, typically returned from a {cmdstanr}
#'   modeling workflow.
#' @param draws Integer specifying the total number of posterior draws to
#'   simulate. These are split approximately evenly across cores.
#' @param cores Integer number of parallel workers to use.
#' @param ... Additional arguments passed to [terra::predict()] via
#'   [terra_posterior_predict()].
#'
#' @return A string giving the file path to the combined output raster.
#' @export
terra_posterior_predict_par <- function(file_raster,
                                        file_out,
                                        model,
                                        draws = 1,
                                        cores = 1,
                                        ...) {
  # Basic checks in the input data
  stopifnot(file.exists(file_raster))
  stopifnot(is.numeric(draws) && draws > 0)
  stopifnot(is.numeric(cores) && cores > 0)

  draws_each <- .get_draws_each(draws, cores)
  stopifnot(length(draws_each) == cores)

  # Write each raster to a tempfile, then stitch back together
  files_each <- tempfile(pattern = rep("stan-pred", cores), fileext = ".tif")

  # If the function dies then make sure to delete the files
  on.exit(unlink(files_each), add = TRUE)

  # Run the predictions in parallel - write each to a tempfile then combine
  parallel::mclapply(
    1:cores,
    \(i) {
      raster <- terra::rast(file_raster)
      terra_posterior_predict(
        raster,
        model,
        draws = draws_each[i],
        filename = files_each[i],
        overwrite = TRUE,
        ...
      )
    },
    mc.cores = cores
  )

  # Write raster out to disk after combining, with the sample counts
  r_out <- terra::rast(files_each)
  names(r_out) <- paste0("sample_", seq(1, terra::nlyr(r_out)))
  terra::writeRaster(r_out, file_out, overwrite = TRUE)

  return(file_out)
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
