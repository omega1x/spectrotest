#' @title
#'  Coerce DRIFT-spectrum to a Data Frame
#'
#' @family drift
#'
#' @description
#'  A method for the \code{\link{as.data.frame}} generics specified for
#'  objects of \code{\link{S3-class}} \emph{drift}. All meta tags are bound
#'  to the right of wave number columns.
#'
#' @param x
#'  an object of \code{\link{S3-class}} \emph{drift}.
#'
#' @param row.names
#'  NULL or a character vector giving the valid row names for the data frame.
#'
#' @param optional
#'  logical. If TRUE "WN" prefix to wave numbers in column names is not added
#'
#' @param ...
#'  (not used)
#'
#' @return
#'  an object of \code{\link{S3-class}} \code{\link{data.frame}}
#'
#' @export
#'
#' @examples
#'  s <- coal_drift()
#'
#'  # default usage:
#'  dfs <- as.data.frame(s)
#'  stopifnot(is.data.frame(dfs))
#'
#'  # use own names for cells:
#'  dfsn <- as.data.frame(s, c("measurement_1", "measurement_2"))
#'  stopifnot(is.data.frame(dfsn))
#'

as.data.frame.drift <- function(x, row.names = NULL, optional = FALSE, ...){
  checkmate::assert_class(x, "drift")
  if (!is.null(row.names))
    checkmate::assert_names(row.names, "strict")
  checkmate::assert_flag(optional)

  meta <- attr(x, "meta")
  wave_numbers <- rownames(x)
  x <- as.data.frame(
    t(unclass(x)),
    row.names = if (!is.null(row.names)) row.names else colnames(x)
  )
  colnames(x) <- if (optional) wave_numbers else sprintf("WN%s", wave_numbers)
  cbind(x, as.data.frame.list(meta))
}
