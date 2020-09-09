#' @export
Ops.drift <- function(e1, e2 = NULL){
  checkmate::assert_class(e1, "drift")
  checkmate::assert_multi_class(e2, c("integer", "numeric", "matrix", "drift"),
                                null.ok = TRUE)
  if (!is.null(e2))
    switch(
      class(e2),
      integer = checkmate::assert_number(e2, finite = TRUE),
      numeric = checkmate::assert_number(e2, finite = TRUE),
      drift = checkmate::assert_true(all(rownames(e1) == rownames(e2)))
    )
  NextMethod("Ops")
}
