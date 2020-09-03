#' @title
#'  Plot method for DRIFT-spectroscopy data
#'
#' @family drift
#'
#' @description
#'  A method for the \code{\link{plot}} generics specified for
#'  objects of \code{\link{S3-class}} \emph{drift}. Method plots
#'  \href{https://en.wikipedia.org/wiki/Diffuse_reflectance_infrared_fourier_transform_spectroscopy}{DRIFTS}
#'  spectra in coordinates \emph{wavenumbers}-\emph{intensity} for middle
#'  \href{https://en.wikipedia.org/wiki/Infrared}{infrared} range.
#'
#' @param x
#'  an object of \code{\link{S3-class}} \emph{drift}.
#'
#' @param y
#'  character vector of \code{\link{regex}}es that are searched against the list
#'  of DRIFT-spectroscopy measurment attributes embedded in the object to
#'  print them on plot.
#'
#' @param ...
#'  other parameters to \code{\link{plot.default}} and to \code{\link{lines}}.
#'
#' @return
#'  NULL, invisibly.
#'
#' @details
#' For raw measurements in DRIFT-spectroscopy the \emph{intensity} represents the
#' proportion of diffuse \href{https://en.wikipedia.org/wiki/Reflectance}{reflectance}
#' radiated by specimen. Note that due to optic geometry in DRIFT-spectroscopy
#' the raw intensity in FTIR-spectrometer may assign \href{https://en.wikipedia.org/wiki/Transmittance}{transmittance}
#' units which actually is \emph{reflectance}.
#'
#' @export
#'
#' @examples
#'  s <- coal_drift()
#'  plot(s)

plot.drift <- function(x, y = NULL, ...) {
  checkmate::assert_class(x, "drift")
  arg_list <- list(...)

  checkmate::assert_character(y, any.missing = FALSE, null.ok = TRUE)
  checkmate::assert_numeric(
    arg_list$xlim,
    any.missing = FALSE,
    len = 2,
    unique = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_numeric(
    arg_list$ylim,
    any.missing = FALSE,
    len = 2,
    unique = TRUE,
    null.ok = TRUE
  )
  checkmate::assert_string(arg_list$xlab, null.ok = TRUE)
  checkmate::assert_string(arg_list$ylab, null.ok = TRUE)

  wave_numbers <- as.numeric(rownames(x))

  # Make up canvas:
  do.call(graphics::plot.default,
          within(arg_list, {
            xlim <- if (!exists("xlim")) rev(range(wave_numbers)) else xlim
            ylim <- if (!exists("ylim")) range(x) else ylim
            xlab <- if (!exists("xlab")) "Wavenumbers, cm\u207b\ub9" else xlab
            ylab <-
              if (!exists("ylab")) attr(x, "meta")$ftirIntensityMode else ylab
            y <- x <- 0
            type <- "n"
          }))

  graphics::title("SPECTROTEST", adj = 1)

  # Print meta-data:
  if (is.null(y))
    y <- c("ftirValidFileName", "ftirDatetime", "ftirCells")

  query_keys <- unique(unlist(lapply(
    y, grep, x = names(attr(x, "meta")), value = TRUE
  )))

  if (length(query_keys) > 0) {
    write.dcf(attr(x, "meta")[query_keys],
              file = {
                meta_text <- c("DRIFT-spectroscopy measurement attributes",
                               paste(rep("\u2591", 30), collapse = ""))
                conn <- textConnection("meta_text", "a", local = TRUE)
                on.exit(close(conn))
                conn
              })

    graphics::mtext(
      text = paste(meta_text, collapse = "\n"),
      cex = 0.7,
      adj = 0.01,
      line = -0.5,
      padj = 1,
      bty = "c",
      bg = "grey"
    )
  }

  # Draw DRIFT-data:
  for (cell in seq_len(ncol(x)))
    do.call(
      if (nrow(x) > 1) graphics::lines else graphics::points,
      within(arg_list, {
      y <- unclass(x)[, cell]
      x <- wave_numbers
    }))
}
