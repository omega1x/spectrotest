#' @title
#'  Slice DRIFT-spectrum by wavenumbers
#'
#' @family drift
#'
#' @description
#'
#'  Return a subset of a DRIFT-spectrum in the given range of wavenumbers.
#'
#' @param x
#'  an object of \code{\link{S3-class}} \emph{drift}.
#' @param k
#'  a wavenumber, [\eqn{cm^{-1}}{1/cm}]. If given only one channel is extracted from
#'  DRIFT-spectrum and further arguments are ignored.
#'
#' @param left
#'  a wavenumber, [\eqn{cm^{-1}}{1/cm}]. If given indicates the lowest wavenumber
#'  in wavenumber range.
#'
#' @param right
#'  a wavenumber, [\eqn{cm^{-1}}{1/cm}]. If given indicates the highest wavenumber
#'  in wavenumber range.
#'
#' @return
#'  An object of \code{\link{S3-class}} \emph{drift}.
#'
#' @export
#'
#' @examples
#'  s <- coal_drift()
#'
#'  # extract one channel at 2000 1/cm:
#'  s1 <- slice(s, 2000)
#'  plot(s1)
#'
#'  # make slice starting from 2000 1/cm:
#'  s2k <- slice(s, , 2000)
#'  plot(s2k)
#'
#'  # make slice up to 1000 1/cm:
#'  s1k <- slice(s, , , 1000)
#'  plot(s1k)
#'
#'  # subset in CO2 region:
#'  s2 <- slice(s, , 2399.44, 2411.02)
#'  plot(s2)
#'

slice <- function(x, k, left, right) {
  checkmate::assert_class(x, "drift")
  assertion <- expression(
    checkmate::assert_number(
      k,
      lower = floor(min(wave_numbers)),
      upper = ceiling(max(wave_numbers)),
      finite = TRUE
    ),
    checkmate::assert_number(
      left,
      lower = floor(min(wave_numbers)),
      upper = ceiling(max(wave_numbers)),
      finite = TRUE
    ),
    checkmate::assert_number(
      right,
      lower = floor(min(wave_numbers)),
      upper = ceiling(max(wave_numbers)),
      finite = TRUE
    ),
    checkmate::assert_number(
      right - left,
      lower = 0,
      finite = TRUE
    )
  )

  wave_numbers <- as.numeric(rownames(x))
  n <- length(wave_numbers)
  loc <- function(x) which.min(abs(wave_numbers - x))

  channels <- switch(
    names(which(
      c(
        empty = missing(k) && missing(left) && missing(right),
        point = !missing(k) && missing(left) && missing(right),
        point_warn = !missing(k) &&
          (!missing(left) || !missing(right)),
        left  = missing(k) && !missing(left) && missing(right),
        right = missing(k) && missing(left) && !missing(right),
        range = missing(k) && !missing(left) && !missing(right)
      )
    )),
    empty = {
      seq_len(length(wave_numbers))
    }  ,
    point = {
      eval(assertion[[1]])
      loc(k)
    },
    point_warn = {
      eval(assertion[[1]])
      warning(
        "Return subset for given k. Further range specification is ignored",
        immediate. = TRUE
      )
      loc(k)
    },
    left = {
      eval(assertion[[2]])
      seq(loc(left), n)
    },
    right = {
      eval(assertion[[3]])
      seq(1, loc(right))
    },
    range = {
      eval(assertion[2:4])
      seq(loc(left), loc(right))
    }
  )
  structure(
    unclass(x)[channels, , drop = FALSE],
    meta = attr(x, "meta"),
    class = "drift"
  )
}
