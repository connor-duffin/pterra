test_that("terra_posterior_predict returns correct dimensions and type", {
  # Simulate dataset
  set.seed(27)
  data <- expand.grid(x = runif(10), y = runif(10))
  data$x2 <- data$x ^ 2
  data$y2 <- data$y ^ 2
  data$z <- (data$x - 0.5) ^ 2 + (data$y - 0.5) ^ 2 + 0.05 * rnorm(100)

  # Fit a Bayesian linear model using rstanarm: suppress warnings as messages
  # are generated by the stan model being small
  suppressWarnings(
    stan_fit <- rstanarm::stan_lm(
      z ~ x + y + x2 + y2,
      data = data,
      prior = rstanarm::R2(0.5),
      chains = 1,
      iter = 100, # You can reduce this for speed if needed
      seed = 12345,
      refresh = 0
    )
  )

  # Create raster covariates
  x <- terra::rast(ncol = 50, nrow = 50, xmin = 0, xmax = 1, ymin = 0, ymax = 1)
  coords <- terra::crds(x, df = TRUE)
  terra::values(x) <- coords$x

  y <- terra::deepcopy(x)
  terra::values(y) <- coords$y
  r <- c(x, y, x ^ 2, y ^ 2)
  names(r) <- c("x", "y", "x2", "y2")

  # Run posterior predictive simulation
  draws <- 10
  preds <- terra_posterior_predict(r, stan_fit, draws = draws)

  # Check against known values
  expect_s4_class(preds, "SpatRaster")
  expect_equal(terra::nlyr(preds), draws)
  expect_equal(dim(preds)[1:2], dim(r)[1:2])
  expect_equal(names(preds), paste0("sample_", 1:10))

  # Parallel version
  file_r <- file.path(tempdir(), "covs.tif")
  file_out <- file.path(tempdir(), "draws.tif")
  terra::writeRaster(r, file_r, overwrite = TRUE)
  terra_posterior_predict_par(file_r, file_out, stan_fit, draws = draws, cores = 2)

  # The same checks on the output raster
  preds_par <- terra::rast(file_out)
  expect_s4_class(preds_par, "SpatRaster")
  expect_equal(terra::nlyr(preds_par), draws)
  expect_equal(dim(preds_par)[1:2], dim(r)[1:2])
  expect_equal(names(preds_par), paste0("sample_", 1:10))
})

test_that(".get_draws_each splits draws correctly", {
  expect_equal(.get_draws_each(10, 2), c(5, 5))
  expect_equal(.get_draws_each(11, 3), c(4, 4, 3))
  expect_equal(.get_draws_each(0, 2), c(0, 0))
  expect_equal(.get_draws_each(120, 4), c(30, 30, 30, 30))
})

test_that(".stan_posterior_predict returns matrix of correct dimensions", {
  set.seed(27)
  data <- expand.grid(x = runif(5), y = runif(5))
  data$x2 <- data$x ^ 2
  data$y2 <- data$y ^ 2
  data$z <- (data$x - 0.5) ^ 2 + (data$y - 0.5) ^ 2 + 0.05 * rnorm(25)

  # Suppress warnings as messages are generated by the stan model being small
  suppressWarnings(
    stan_fit <- rstanarm::stan_lm(
      z ~ x + y + x2 + y2,
      data = data,
      prior = rstanarm::R2(0.5),
      chains = 1,
      iter = 100,
      seed = 123,
      refresh = 0
    )
  )

  pred_matrix <- .stan_posterior_predict(stan_fit, data, draws = 10)

  expect_true(is.matrix(pred_matrix))
  expect_equal(nrow(pred_matrix), nrow(data))
  expect_equal(ncol(pred_matrix), 10)
})
