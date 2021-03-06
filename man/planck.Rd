% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/math_planck.R
\name{planck}
\alias{planck}
\title{Calculate the spectral radiance of the Absolutely Black Body}
\usage{
planck(
  x,
  temperature = y0^(1/3) * 560.323665873058,
  x0 = temperature * 1.9610115857552,
  y0 = (x0 * 0.001)^3 * 0.75377608961114
)
}
\arguments{
\item{x}{numeric vector of \href{https://en.wikipedia.org/wiki/Wavenumber}{wavenumbers}
(1/cm) which the \href{https://en.wikipedia.org/wiki/Radiance}{spectral radiance}
should be calculated at.}

\item{temperature}{\dfn{ABB} temperature in \href{https://en.wikipedia.org/wiki/Kelvin}{kelvins}
given as a numeric vector of length 1, or alternatively.}

\item{x0}{abscissa of the radiance peak (in 1/cm) given as a numeric
vector of length 1, or alternatively.}

\item{y0}{ordinate of the radiance peak
(\href{https://en.wikipedia.org/wiki/International_System_of_Units}{SI}
units of power:
\eqn{W \cdot m^{-2} \cdot sr^{-1} \cdot \left ( {cm^{-1}} \right )^{-1}}{Wm^(-2)sr^(-1)(1/cm)^(-1)})
given as a numeric vector of length 1.}
}
\value{
Calculated spectral radiance
  (\href{https://en.wikipedia.org/wiki/International_System_of_Units}{SI}
  units of power:
  \eqn{W \cdot m^{-2} \cdot sr^{-1} \cdot \left ( {cm^{-1}} \right )^{-1}}{Wm^(-2)sr^(-1)(1/cm)^(-1)})
  as a numeric vector.
}
\description{
Calculate the
 \href{https://en.wikipedia.org/wiki/Radiance}{spectral radiance} (power)
 emitted per unit projected area of the
 \href{https://en.wikipedia.org/wiki/Black_body}{Absolutely Black Body}
 (\dfn{ABB}) as a function of the given
 \href{https://en.wikipedia.org/wiki/Wavenumber}{wavenumbers}
 according to the
 \href{https://www.britannica.com/science/Plancks-radiation-law}{Planck's law}.
}
\details{
Only one of the next \dfn{ABB} radiance parameters should be provided:
  \itemize{
    \item \dfn{ABB} temperature (\code{temperature}) or
    \item abscissa of the radiance peak (\code{x0}) or
    \item ordinate of the radiance peak (\code{y0}).
  }
}
\examples{
 # Typical middle infrared range exposed by FTIR-spectrometers:
 wave_numbers <- seq(from = 349.115696, by = 1.928816, length.out = 3709)

 # radiance parameterized with absolute temperature in kelvins:
 planck(wave_numbers, temperature = 944.2387)

 # radiance parameterized with peak position in 1/cm:
 planck(wave_numbers, x0 = wave_numbers[780])

 # radiance parameterized with peak value in SI-units of radiance:
 planck(wave_numbers, y0 = 4.785513)
}
\references{
\href{http://www.spectralcalc.com/blackbody/CalculatingBlackbodyRadianceV2.pdf}{Calculating Blackbody Radiance}
 / \href{http://www.spectralcalc.com/}{www.spectralcalc.com} -
 High-resolution spectral modelling: GATS, Inc.
}
\seealso{
Other math: 
\code{\link{pinv}()}
}
\concept{math}
