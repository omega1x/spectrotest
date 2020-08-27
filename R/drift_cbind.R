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
  obj <- list(...)
  meta <- attr(obj[[1]], "meta")
  wave_numbers <- rownames(obj[[1]])
  for (i in seq_along(obj))
    if (class(obj[[i]]) == "drift") attr(obj[[i]], "class") <- NULL
  obj <- do.call(cbind, obj)
  n <- ncol(obj)
  meta$ftirCells <-  n
  attr(obj, "meta") <- meta
  colnames(obj) <- sprintf("Cell_%02i", seq_len(n))
  rownames(obj) <- wave_numbers
  class(obj) <- "drift"
  obj
}
