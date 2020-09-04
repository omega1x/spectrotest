#' @export

# Indexing technique:
"[.drift" <- function(x, i, j, ...) {
  checkmate::assert_class(x, "drift")
  if (!missing(i))
    checkmate::assert_integerish(i,
                                 any.missing = FALSE,
                                 unique = TRUE,
                                 sorted = TRUE)
  if (!missing(j))
    checkmate::assert_integerish(j,
                                 any.missing = FALSE,
                                 unique = TRUE,
                                 sorted = TRUE)
  structure(unclass(x)[i, j, drop = FALSE],
            meta = attr(x, "meta"),
            class = "drift")
}


#' @export
"[[.drift" <- function(x, key) {
  checkmate::assert_class(x, "drift")
  checkmate::assert_string(key)
  attr(x, "meta")[[key]]
}


#' @export
"[[<-.drift" <- function(x, key, value) {
  checkmate::assert_class(x, "drift")
  checkmate::assert_names(key, "strict")
  checkmate::assert_scalar(value)
  if (is.character(value)) checkmate::assert_names(value, "strict")
  attr(x, "meta")[[key]] <- value
  return(x)
}
