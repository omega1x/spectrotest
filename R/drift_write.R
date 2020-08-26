#' @title
#'  Write DRIFT-spectrum object in Spectrotest format
#'
#' @family drift
#'
#' @description
#'  Write object of \code{\link{S3-class}} \emph{drift} in Spectrotest
#'  format to text file or connection. Spectrotest format is a
#'  \href{https://en.wikipedia.org/wiki/Comma-separated_values}{CSV}-compatible
#'  text format with special \href{https://en.wikipedia.org/wiki/Tag_(metadata)}{meta-tags},
#'  so it could be easily opened by any
#'  \href{https://en.wikipedia.org/wiki/Spreadsheet}{spreadsheet software}.
#'
#' @param x
#'  an object of \code{\link{S3-class}} \emph{drift}.
#'
#' @param file
#'  a \code{\link{connection}} object or a character string naming the file
#'  to write to.
#'
#' @return
#'  Spectrotest formatted string, invisibly.
#'
#' @details
#'  Argument \code{file} has the same meaning as in \code{\link{write}}.
#'
#' @export
#'
#' @examples
#'  s <- coal_drift()
#'
#'  # Write to text file:
#'  write.drift(s, "s.ir")
#'
#'  # Write to connection:
#'  con <- xzfile("s.xz", open = "w")
#'  write.drift(s, con)
#'  close(con)
#'  rm(con)
#'
#'  # Light unit tests:
#'  s0 <- read.drift("s.ir")
#'  stopifnot(all(s0 == s))
#'  unlink("s.ir")
#'
#'  con <- xzfile("s.xz", open = "r")
#'  s0 <- read.drift(con)
#'  close(con)
#'  rm(con)
#'  stopifnot(all(s0 == s))
#'  unlink("s.xz")
#'

write.drift <- function(x, file = "") {
  checkmate::assert_class(x, "drift")
  checkmate::assert(
    checkmate::check_string(file),
    checkmate::check_class(file, "connection"),
    combine = "or"
  )

  # prepare signal:
  utils::write.table(
    x,
    file = {
      signal_data = ""
      con <- textConnection("signal_data", "w", local = TRUE)
      on.exit(close(con))
      con
    },
    quote = FALSE,
    sep = "; ",
    eol = "\n",
    dec = ".",
    row.names = TRUE,
    col.names = FALSE
  )
  output_text <- paste0(c(
    "# Spectrotest, 2020",
    # prepare meta-data:
    sprintf("# @ %s %s", names(attr(x, "meta")), attr(x, "meta")),
    "",
    signal_data,
    ""
  ),
  collapse = "\n")
  cat(output_text, file = file)
  invisible(output_text)
}
