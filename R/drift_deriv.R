#' @title
#'  Differentiate or smooth DRIFT-spectrum
#'
#' @family drift
#'
#' @description
#'
#'  Differentiate or smooth DRIFT-spectrum using spline fit model
#'
#' @param expr
#'  an object of \code{\link{S3-class}} \emph{drift}.
#'
#' @param ...
#'  other named parameters to \code{\link{smooth.spline}} and to
#'  \code{\link{predict.smooth.spline}}, including the most
#'  important ones:
#'  \describe{
#'    \item{deriv}{integer order of derivative. First order derivative is the default value. Assign zero to perform smoothing which is the zero-order derivative}
#'    \item{spar}{smoothing parameter in (0,1] range. Default value 0.8 usualy is rather good value for DRIFT-spectra of hard coals}
#'    \item{df}{the desired equivalent number of degrees of freedom. Default value 12 usualy is rather good value for DRIFT-spectra of hard coals}
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
#'  ds <- deriv(s)
#'  plot(ds)
#'
#'  # simple smoothing of signal:
#'  ss <- deriv(s, deriv = 0)
#'  plot(ss)
#'
#'  # second derivative:
#'  d2s <- deriv(s, deriv = 2)
#'  plot(d2s)
#'
#'  # sequential application of differentiation is not equivalent to higher
#'  # order differentiation in place:
#'  range(deriv(deriv(s)) - deriv(s, deriv = 2))
#'
deriv.drift <- function(expr, ...) {
  checkmate::assert_class(expr, "drift")
  arg_list = list(...)
  checkmate::assert_count(arg_list$deriv, null.ok = TRUE)
  checkmate::assert_number(arg_list$spar, lower = .Machine$double.eps,
                           upper = 1, null.ok = TRUE)
  checkmate::assert_flag(arg_list$all.knots, null.ok = TRUE)
  checkmate::assert_count(arg_list$df, positive = TRUE, null.ok = TRUE)
  checkmate::assert_flag(arg_list$keep.data, null.ok = TRUE)

  DEFAULT_PARAMETER <- list(
    deriv = 1L,  # in predict
    spar = .8,
    all.knots = TRUE,
    df = 12,
    keep.data = FALSE
  )

  # add lost default parameters:
  arg_list <- c(
    arg_list,
    DEFAULT_PARAMETER[setdiff(names(DEFAULT_PARAMETER), names(arg_list))])

  # make wave numbers commensurable to intensity magnitudes:
  wave_numbers <- 1e-3 * as.numeric(row.names(expr))

  # fit spline models:
  structure(
    apply(expr,
        2,
        function(v)
          stats::predict(
            do.call(stats::smooth.spline, args = within(arg_list, {
              x <- wave_numbers
              y <- v
              rm(deriv)
            })),
            wave_numbers, deriv = arg_list$deriv)$y
    ),
    meta = {
      if (arg_list$deriv > 0)
        attr(expr, "meta")[["ftirIntensityMode"]] <-
          sprintf("D%02i of %s", arg_list$deriv,
                  attr(expr, "meta")[["ftirIntensityMode"]])
      attr(expr, "meta")
    },
    dimnames = list(rownames(expr), colnames(expr)),
    class = "drift")
}
