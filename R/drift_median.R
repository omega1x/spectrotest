#' @title
#'   Aggregate measurements of DRIFT-spectrum using median
#'
#' @family drift
#'
#' @description
#'  Given cells of measurements, make up the aggregated measurement with
#'  \code{\link{median}}. Due to non-normality in channel measurements in cells
#'  applying \code{\link{median}} instead of \code{\link{mean}} as an
#'  aggregation function seems relevant in
#'  \href{https://en.wikipedia.org/wiki/Diffuse_reflectance_infrared_fourier_transform_spectroscopy}{DRIFTS}.
#'  This is a \emph{S3}-method for generic function \code{\link{median}}.
#'
#' @param x
#'  object of \code{\link{S3-class}} \emph{drift}
#'
#' @param na.rm
#'  not used parameter for consistency with generic method
#'
#' @param ...
#'  potentially further arguments for methods; not used in the this method.
#'
#' @return
#'  An object of \code{\link{S3-class}} \emph{drift} that has only one
#'  measurement cell - the result of cell aggregation by \code{\link{median}}.
#'
#' @importFrom stats median
#'
#' @export
#'
#' @examples
#'
#'  # Light unit tests:
#'  x <- median(cbind(coal_drift(), coal_drift()*.92))
#'  plot(x)
#'  stopifnot(ncol(x) == 1)

median.drift <- function(x, na.rm = FALSE, ...){
  checkmate::assert_class(x, "drift")
  structure(
    as.matrix(apply(unclass(x), 1, stats::median)),
    dimnames = list(rownames(x), "median"),
    meta = {
      meta <- attr(x, "meta")
      meta[["ftirCells"]] <- 1
      meta
    },
    class = "drift"
  )
}
