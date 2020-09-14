#' @title
#'  Get the channel numbers of DRIFT-spectrum
#'
#' @family drift
#'
#' @description
#'  Get the channel numbers for appropriate wave numbers in the scale of the
#'  given object of \code{\link{S3-class}} \emph{drift}. The function is a
#'  simple backward approximation \emph{wavenumber - channel}.
#'
#' @param x
#'  An object of \code{\link{S3-class}} \emph{drift}.
#'
#' @param wn
#'  Numeric vector of wavenumbers for which channel numbers should be determined
#'  in the DRIFT-spectrum
#'
#' @return
#'  Numeric vector of channel numbers which are interpolated values of indicies.
#'
#' @export
#'
#' @examples
#'  s <- coal_drift()
#'
#'  # Get channels of CO2 absorption:
#'  co2 <- channel(s, c(2268:2277, 2399:2412))
#'  print(co2)

channel <- function(x, wn) {
  checkmate::assert_class(x, "drift")
  wave_numbers <- as.numeric(rownames(x))
  checkmate::assert_numeric(
    wn,
    lower = min(wave_numbers),
    upper = max(wave_numbers),
    finite = TRUE,
    any.missing = FALSE
  )
  round(stats::approxfun(wave_numbers, seq_along(wave_numbers))(wn))
}
