#' @title
#'  Read DRIFT-spectroscopy data
#'
#' @family drift
#'
#' @description
#'  Read \href{https://en.wikipedia.org/wiki/Diffuse_reflectance_infrared_fourier_transform_spectroscopy}{DRIFTS}
#'  spectrum from \emph{Spectrotest} text file or
#'  connection produced by \emph{FT-CONTROL} software for
#'  \emph{FTIR}-spectrometers.
#'
#' @param con
#'  a \code{\link{connection}} object or a character string naming the file
#'  to read.
#'
#' @return
#'  An object of \code{\link{S3-class}} \emph{drift}.
#'
#' @details
#'  Argument \code{con} has the same meaning as in \code{\link{readLines}}.
#'
#' @export
#'
#' @examples
#'  s0 <- coal_drift()
#'
#'  # Write to text file:
#'  write.drift(s0, "s0.ir")
#'
#'  # Read from text file:
#'  s1 <- read.drift("s0.ir")
#'
#'  unlink("s0.ir")
#'
#'  # Light unit test:
#'  stopifnot(all(s0 == s1))

read.drift <- function(con) {
  buffer <- readLines(con, skipNul = TRUE)  # readLines used as assertion
  drift_data <-
    as.matrix(
      utils::read.table(
        header = FALSE,
        sep = ";",
        dec = ".",
        row.names = 1,
        colClasses = "numeric",
        nrows = length(buffer),
        fill = FALSE,
        blank.lines.skip = TRUE,
        comment.char = "#",
        text = buffer
      )
    )
  colnames(drift_data) <- sprintf("Cell_%02i", seq_len(ncol(drift_data)))
  meta <-
    lapply(t(
      utils::read.table(
        header = FALSE,
        sep = " ",
        dec = ".",
        row.names = 3,
        colClasses = "character",
        comment.char = "",
        text = grep(utils::glob2rx("# @ *"), buffer, value = TRUE),
        stringsAsFactors = FALSE
      )[3]
    )[1,],
    function(x) {
      x <- utils::type.convert(x)
      if (is.factor(x)) as.character(x) else x
    })
  meta$ftirCells <- ncol(drift_data)
  structure(drift_data, meta = meta, class = "drift")
}
