#' @title
#'  Differentiate or smooth DRIFT-spectrum
#'
#' @family drift
#'
#' @description
#'
#'  Differentiate or smooth DRIFT-spectrum using spline fit model. This is
#'  an \emph{S3}-method for class \emph{drift}.
#'
#' @param x
#'  an object of \code{\link{S3-class}} \emph{drift}.
#'
#' @param n
#'  integer order of derivative. First order derivative is the default value. Assign zero to perform smoothing which is the zero-order derivative.
#'
#' @param ...
#'  other named parameters to \code{\link{smooth.spline}} including but not
#'  limited to
#'  \describe{
#'    \item{spar}{smoothing parameter in (0,1] range. Default value 0.8 usualy is rather good value for DRIFT-spectra of hard coals}
#'    \item{df}{the desired equivalent number of degrees of freedom. Default value 12 usualy is rather good value for DRIFT-spectra of hard coals.}
#'  }
#'
#' @return
#'  An object of \code{\link{S3-class}} \emph{drift}.
#'
#' @export
#'
#' @examples
#'  s <- coal_drift()
#'
#'  # first derivative:
#'  ds <- diff(s)
#'  plot(ds)
#'
#'  # simple smoothing of signal:
#'  ss <- diff(s, 0)
#'  plot(ss)
#'
#'  # second derivative:
#'  d2s <- diff(s, n = 2)
#'  plot(d2s)
#'
#'  # sequential application of differentiation is not equivalent to higher
#'  # order differentiation in place:
#'  range(diff(diff(s)) - diff(s, 2))

diff.drift <- function(x, n = 1L, ...) {
  checkmate::assert_class(x, "drift")
  checkmate::assert_count(n)

  arg_list = list(...)
  checkmate::assert_number(arg_list$spar, lower = .Machine$double.eps, upper = 1, null.ok = TRUE)
  checkmate::assert_flag(arg_list$all.knots, null.ok = TRUE)
  checkmate::assert_count(arg_list$df, positive = TRUE, null.ok = TRUE)
  checkmate::assert_flag(arg_list$keep.data, null.ok = TRUE)

  DEF_SMOOTH_PARS <- list(
    spar = .8,
    all.knots = TRUE,
    df = 12,
    keep.data = FALSE
  )

  # add lost default parameters:
  arg_list <- c(
    arg_list,
    DEF_SMOOTH_PARS[setdiff(names(DEF_SMOOTH_PARS), names(arg_list))])

  # make wave numbers commensurable to intensity magnitudes:
  wave_numbers <- 1e-3 * as.numeric(row.names(x))

  # fit spline models:
  structure(
    apply(x,
        2,
        function(v)
          stats::predict(
            do.call(stats::smooth.spline, args = within(arg_list, {
              x <- wave_numbers
              y <- v
            })),
            wave_numbers, deriv = n)$y
    ),
    meta = {
      if (n > 0)
        attr(x, "meta")[["ftirIntensityMode"]] <-
          sprintf("D%02i of %s", n,
                  attr(x, "meta")[["ftirIntensityMode"]])
      attr(x, "meta")
    },
    dimnames = list(rownames(x), colnames(x)),
    class = "drift")
}
