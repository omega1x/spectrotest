#' @title
#'   Calculate the spectral radiance of the Absolutely Black Body
#'
#' @family math
#'
#' @description
#'  Calculate the
#'  \href{https://en.wikipedia.org/wiki/Radiance}{spectral radiance} (power)
#'  emitted per unit projected area of the
#'  \href{https://en.wikipedia.org/wiki/Black_body}{Absolutely Black Body}
#'  (\dfn{ABB}) as a function of the given
#'  \href{https://en.wikipedia.org/wiki/Wavenumber}{wavenumbers}
#'  according to the
#'  \href{https://www.britannica.com/science/Plancks-radiation-law}{Planck's law}.
#'
#' @param x
#'   numeric vector of \href{https://en.wikipedia.org/wiki/Wavenumber}{wavenumbers}
#'   (1/cm) which the \href{https://en.wikipedia.org/wiki/Radiance}{spectral radiance}
#'   should be calculated at.
#' @param temperature
#'   \dfn{ABB} temperature in \href{https://en.wikipedia.org/wiki/Kelvin}{kelvins}
#'   given as a numeric vector of length 1, or alternatively.
#' @param x0
#'   abscissa of the radiance peak (in 1/cm) given as a numeric
#'   vector of length 1, or alternatively.
#' @param y0
#'   ordinate of the radiance peak
#'   (\href{https://en.wikipedia.org/wiki/International_System_of_Units}{SI}
#'   units of power:
#'   \eqn{W \cdot m^{-2} \cdot sr^{-1} \cdot \left ( {cm^{-1}} \right )^{-1}}{Wm^(-2)sr^(-1)(1/cm)^(-1)})
#'   given as a numeric vector of length 1.
#'
#' @return
#'   Calculated spectral radiance
#'   (\href{https://en.wikipedia.org/wiki/International_System_of_Units}{SI}
#'   units of power:
#'   \eqn{W \cdot m^{-2} \cdot sr^{-1} \cdot \left ( {cm^{-1}} \right )^{-1}}{Wm^(-2)sr^(-1)(1/cm)^(-1)})
#'   as a numeric vector.
#'
#' @details
#'   Only one of the next \dfn{ABB} radiance parameters should be provided:
#'   \itemize{
#'     \item \dfn{ABB} temperature (\code{temperature}) or
#'     \item abscissa of the radiance peak (\code{x0}) or
#'     \item ordinate of the radiance peak (\code{y0}).
#'   }
#'
#' @references
#'  \href{http://www.spectralcalc.com/blackbody/CalculatingBlackbodyRadianceV2.pdf}{Calculating Blackbody Radiance}
#'  / \href{http://www.spectralcalc.com/}{www.spectralcalc.com} -
#'  High-resolution spectral modelling: GATS, Inc.
#'
#' @export
#'
#' @examples
#'  # Typical middle infrared range exposed by FTIR-spectrometers:
#'  wave_numbers <- seq(from = 349.115696, by = 1.928816, length.out = 3709)
#'
#'  # radiance parameterized with absolute temperature in kelvins:
#'  planck(wave_numbers, temperature = 944.2387)
#'
#'  # radiance parameterized with peak position in 1/cm:
#'  planck(wave_numbers, x0 = wave_numbers[780])
#'
#'  # radiance parameterized with peak value in SI-units of radiance:
#'  planck(wave_numbers, y0 = 4.785513)

planck <- function(x,
                   temperature = y0^(1/3)*560.323665873058,
                   x0 = temperature*1.9610115857552,
                   y0 = (x0*1e-3)^3*.75377608961114){

  checkmate::assert_double(x, lower = .Machine$double.eps, finite = TRUE)
  checkmate::assert_number(temperature, lower = .Machine$double.eps)
  checkmate::assert_number(x0, lower = .Machine$double.eps)
  checkmate::assert_number(y0, lower = .Machine$double.eps)

  11.910428196*(x*1e-3)^3/(exp(1.43876731408165*x/temperature) - 1)
}
