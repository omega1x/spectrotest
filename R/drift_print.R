#' @export

print.drift <- function(x, ...) {
  checkmate::assert_class(x, "drift")
  screen_cast <- sprintf(
    paste0(
      c(
        "\u25cf SPECTROTEST\u00ae DRIFT Object \u2591 %s \u2591 FTIR: %s",
        "  %02i cell(s) %2.0f--%2.0f cm\u207B\u00B9 \u2591 %02i gost-tags        \u2591 %s ",
        "[%s]"
      ),
      collapse = "\n"
    ),
    as.POSIXct(attr(x, "meta")[["ftirDatetime"]],
               tz = " ", format = "%Y%m%d-%H%M%S"),
    attr(x, "meta")[["ftirModel"]],
    ncol(x),
    min(as.double(row.names(x))),
    max(as.double(row.names(x))),
    sum(grepl("^gost", names(attr(x, "meta")))),
    attr(x, "meta")[["ftirIntensityMode"]],
    attr(x, "meta")[["ftirValidFileName"]]
  )
  cat(screen_cast)
  invisible(screen_cast)
}
