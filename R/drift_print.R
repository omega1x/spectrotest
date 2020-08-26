#' @export

print.drift <- function(x, ...) {
  screen_cast <- sprintf(
    paste0(
      c(
        "\u2554\u2550\u2550 SPECTROTEST OBJECT \u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2557 ",
        "\u2588\u2588\u2588\u2588\u2588\u2588\u2557 \u2588\u2588\u2588\u2588\u2588\u2588\u2557 \u2588\u2588\u2557\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2557\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2588\u2557",
        "\u2588\u2588\u2554\u2550\u2550\u2588\u2588\u2557\u2588\u2588\u2554\u2550\u2550\u2588\u2588\u2557\u2588\u2588\u2551\u2588\u2588\u2554\u2550\u2550\u2550\u2550\u255d\u255a\u2550\u2550\u2588\u2588\u2554\u2550\u2550\u255d",
        "\u2588\u2588\u2551  \u2588\u2588\u2551\u2588\u2588\u2588\u2588\u2588\u2588\u2554\u255d\u2588\u2588\u2551\u2588\u2588\u2588\u2588\u2588\u2557     \u2588\u2588\u2551   ",
        "\u2588\u2588\u2551  \u2588\u2588\u2551\u2588\u2588\u2554\u2550\u2550\u2588\u2588\u2557\u2588\u2588\u2551\u2588\u2588\u2554\u2550\u2550\u255d     \u2588\u2588\u2551   ",
        "\u2588\u2588\u2588\u2588\u2588\u2588\u2554\u255d\u2588\u2588\u2551  \u2588\u2588\u2551\u2588\u2588\u2551\u2588\u2588\u2551        \u2588\u2588\u2551   ",
        "\u255a\u2550\u2550\u2550\u2550\u2550\u255d \u255a\u2550\u255d  \u255a\u2550\u255d\u255a\u2550\u255d\u255a\u2550\u255d        \u255a\u2550\u255d   ",
        " \u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u255d    ",
        " \u2588 SAMPLE NAME      | %s",
        " \u2588 MEASUREMENT DATE | %s",
        " \u2588 MEASURED CELLS   | %01i",
        " \u2588----------------------------------",
        " \u2588 WAVENUMBER RANGE | %03.1f - %03.1f",
        " \u2588 INTENSITY RANGE  | %03.1f - %03.1f",
        " \u2588 INTENSITY MODE   | %s",
        " \u2588 ---------------------------------",
        " \u2588 FTIR-MODEL       | %s",
        " \u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550"
      ),
      collapse = "\n"
    ),
    attr(x, "meta")[["ftirValidFileName"]],
    as.POSIXct(attr(x, "meta")[["ftirDatetime"]],
               tz = " ", format = "%Y%m%d-%H%M%S"),
    attr(x, "meta")[["ftirCells"]],

    min(as.double(row.names(x))),
    max(as.double(row.names(x))),
    min(x),
    max(x),
    attr(x, "meta")[["ftirIntensityMode"]],

    attr(x, "meta")[["ftirModel"]]
  )

  specimen_tags <-
    grep(utils::glob2rx("gost*"), names(attr(x, "meta")), value = TRUE)
  if (length(specimen_tags) > 0)
    screen_cast <- paste0(
      c(
        screen_cast,
        "",
        " \u2554\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550 MAIN SPECIMEN TAGS \u2550",
        sprintf(" \u2588  %s = %s", specimen_tags, attr(x, "meta")[specimen_tags]),
        " \u255a\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550\u2550"
      ),
      collapse = "\n"
    )

  cat(screen_cast)
  invisible(screen_cast)
}
