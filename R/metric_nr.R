nr <- function(x) {
  checkmate::assert_class(x, "drift")
  checkmate::assert_complex(x[['ftirPowerMax']], null.ok = FALSE)

  power <- try(
    utils::read.table(
      text = gsub("i", "i\n", x[["ftirPower"]], fixed = TRUE),
      colClasses = "complex")[[1]],
    silent = TRUE
  )
  checkmate::assert_complex(power, null.ok = FALSE)

  MAX_REFL_RANGE = c(1700, 2300)  # [1/cm]
  wave_numbers <- as.numeric(rownames(x))
  checkmate::assert_true(is.in(MAX_REFL_RANGE, range(wave_numbers)))

  # Find out peak of planck curve:
  peak <- with(list(f <- function(x, x0)
    planck(x, x0 = x0),
    max_power <- x[["ftirPowerMax"]]), {
      power <- c(power, max_power)
      # Fit planck-curve to real data:
      m <- nls(
        y ~ beta * f(x, x0),
        data = data.frame(x = wave_numbers[Im(power)], y = Re(power)),
        start = c(beta = 14, x0 = wave_numbers[Im(max_power)]),
        lower = c(beta = 10, x0 = min(MAX_REFL_RANGE)),
        upper = c(beta = 20, x0 = max(MAX_REFL_RANGE)),
        algorithm = "port"
      )
      coef(m)
    })

  CO2 <- list(low = c(2268, 2277), high = c(2399, 2412))  # [1/cm]
  for (i in seq_len(ncol(x))) {
    max_refl <- max(stats::smooth.spline(x[setdiff(rownames(x), CO2), i],
                                  keep.data = FALSE)$y)
    cell <- as.double(x[, i])
    x[, i] <- planck(wave_numbers,
                     x0 = peak[["x0"]]) * (-cell / max_refl + 1)
  }
  x
}


is.in <- function(x, vec) {
  checkmate::assert_numeric(x,
                            finite = TRUE,
                            any.missing = FALSE,
                            len = 2)
  checkmate::assert_numeric(
    vec,
    finite = TRUE,
    any.missing = FALSE,
    len = 2,
    sorted = TRUE,
    unique = TRUE
  )
  all(!as.logical(findInterval(x, vec) - 1))
}
