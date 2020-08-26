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
  obj <- lapply(
    obj,
    function(x) if (class(x) == "drift") unclass(x) else x
  )
  obj <- do.call(cbind, obj)
  n <- ncol(obj)
  meta$ftirCells <-  n
  attr(obj, "meta") <- meta
  colnames(obj) <- sprintf("Cell_%02i", seq_len(n))
  rownames(obj) <- wave_numbers
  class(obj) <- "drift"
  obj
}
