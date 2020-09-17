#' @title
#'   Convert DRIFT-spectrum to normalized radiance units
#'
#' @family metric
#'
#' @description
#'  Convert raw DRIFT-spectrum recorded in transmittance mode (i.e. reflectance)
#'  to normalized radiance units expressed in units of power as it calculated
#'  by \code{\link{planck}}.
#'
#' @param x
#'   an object of \code{\link{S3-class}} \emph{drift}.
#'
#' @return
#'   an object of \code{\link{S3-class}} \emph{drift} exposed
#'   in \href{https://en.wikipedia.org/wiki/International_System_of_Units}{SI}-units
#'   of power: \eqn{W \cdot m^{-2} \cdot sr^{-1} \cdot \left ( {cm^{-1}} \right )^{-1}}{Wm^(-2)sr^(-1)(1/cm)^(-1)}.
#'
#' @details
#'   Only raw DRIFT-spectra recorded into \emph{3709} channels within
#'   \emph{349.1157 - 7501.1654} \eqn{cm^{-1}} infrared range could be processed.
#'
#' @references
#'  \emph{Infrared Analysis of Particulates by FT-IR Emission/Transmission
#'  Spectroscopy}. Peter R. Solomon et al., \emph{Applied Spectroscopy},
#'  Volume: \emph{40} issue: \emph{6}, page(s): \emph{746-759}. DOI: \href{https://doi.org/10.1366/0003702864508421}{10.1366/0003702864508421}.
#'
#' @export
#'
#' @examples
#' gx <- median(nr(coal_drift("GX")))
#' oc <- median(nr(coal_drift("OC")))
#'
#' vs <- gx - oc  # volatile substances
#' vs[["ftirValidFileName"]] <- "Volatile"
#' plot(slice(cbind(vs, oc, gx),,,3700))
#' abline(h = 0, col = "red")

nr <- function(x) {
  checkmate::assert_class(x, "drift")
  # Note! Only spectra within 349.1157 - 7501.1654 1/cm range recorded
  # in 3709 channels are allowed!
  wave_numbers <- as.double(rownames(x))
  checkmate::assert_true(length(wave_numbers) == 3709L)
  # Note! Only raw spectra, i.e. recorded in %Transmittance mode, could be processed
  checkmate::assert_string(x[["ftirIntensityMode"]], pattern = "%Transmittance")
  max_power = x[['ftirPowerMax']]
  checkmate::assert_complex(max_power, null.ok = FALSE)
  power <- try(
    utils::read.table(
      text = gsub("i", "i\n", x[["ftirPower"]], fixed = TRUE),
      colClasses = "complex")[[1]],
    silent = TRUE
  )
  checkmate::assert_complex(power, null.ok = FALSE)
  MAX_REFL_RANGE = c(1700, 2300)  # [1/cm]
  checkmate::assert_true(is.in(MAX_REFL_RANGE, range(wave_numbers)))  # always true?

  # Find out peak of Planck curve:
  peak <- with(
    list(f = function(x, x0) planck(x, x0 = x0)), {
      # Fit Planck curve to real data:
      power <- c(power, max_power)
      m <- stats::nls(
        y ~ beta * f(x, x0),
        data = data.frame(x = wave_numbers[Im(power)], y = Re(power)),
        start = c(beta = 14, x0 = wave_numbers[Im(max_power)]),
        lower = c(beta = 10, x0 = MAX_REFL_RANGE[1]),
        upper = c(beta = 20, x0 = MAX_REFL_RANGE[2]),
        algorithm = "port"
      )
      coef(m)
    })

  co2_channels <- with(
    list(
      co2 = c(2268:2277, 2399:2412)  # [1/cm]
    ), {
    spectrotest::channel(x, co2[findInterval(co2, range(wave_numbers)) == 1])
  })

  for (i in seq_len(ncol(x))) {
    cell <- as.double(x[, i])
    max_refl <- max(
      stats::smooth.spline(cell[-co2_channels], keep.data = FALSE)$y
    )
    x[, i] <-  spectrotest::planck(wave_numbers, x0 = peak[["x0"]]) *
      (-cell / max_refl + 1)
  }
  x[["ftirIntensityMode"]] <- "NormRadiance"
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
