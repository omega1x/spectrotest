#' @title
#'   Combine DRIFT-spectra by columns
#'
#' @family drift
#'
#' @description
#'  Take a sequence of \code{\link{S3-class}} \emph{drift}, vector, matrix or
#'  data-frame arguments and combine by columns. Those columns are thus considered
#'  measurement cells. This is a \emph{S3}-method for generic function
#'  \code{\link{cbind}}.
#'
#' @param ...
#'  first, object(s) of \code{\link{S3-class}} \emph{drift}, then vectors or
#'  matrices. These can be given as named arguments.
#'
#' @param deparse.level
#'   integer controlling the construction of column names:
#'   \describe{
#'     \item{0}{constructs no column names,}
#'     \item{1}{default, column names are combined as they are,}
#'     \item{2}{column names are enumerated.}
#'   }
#'
#' @return
#'  An object of \code{\link{S3-class}} \emph{drift}.
#'
#' @export
#'
#' @examples
#'  # Light unit tests:
#'  x <- coal_drift("X")
#'  oc <- coal_drift("OC")
#'  s <- cbind(x, 4, oc, 2*rnorm(nrow(x)))
#'  stopifnot(class(s) == "drift")
#'

cbind.drift <- function(..., deparse.level = 1){
  checkmate::assert_class(..1, "drift")
  checkmate::assert_choice(deparse.level, 0:2)
  obj <- list(...)
  meta <- attr(obj[[1]], "meta")
  wave_numbers <- rownames(obj[[1]])
  for (i in seq_along(obj))
    if (class(obj[[i]]) == "drift") attr(obj[[i]], "class") <- NULL
  obj <- do.call(cbind, obj)
  n <- ncol(obj)
  meta$ftirCells <-  n
  structure(
    obj,
    dimnames = list(
      wave_numbers,
      switch(
        deparse.level + 1,
        NULL,
        colnames(obj),
        sprintf("Cell_%02i", seq_len(n))
      )
    ),
    meta = meta,
    class = "drift"
  )
}
