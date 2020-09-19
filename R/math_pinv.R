#' @title
#'   Calculate the Moore-Penrose generalized inverse of a matrix
#'
#' @family math
#'
#' @description
#'   Calculate the generalized inverse \code{y} of a matrix \code{x}, also
#'   known as \href{https://en.wikipedia.org/wiki/Generalized_inverse}{Moore-Penrose inverse},
#'   using the singular value decomposition \code{svd()}.
#'
#' @param x
#'   numeric matrix for which the \href{https://en.wikipedia.org/wiki/Generalized_inverse}{Moore-Penrose inverse} is required.
#'
#' @param tol
#'   value of tolerance used for assuming an eigenvalue is zero.
#'
#' @return
#'   \code{y} - matrix that is pseudoinverse of matrix \code{x}
#'
#' @references
#'   Ben-Israel, A., and Th. N. E. Greville (2003). \emph{Generalized Inverses - Theory and Applications}. Springer-Verlag, New York.
#'   ISBN \emph{978-0-387-21634-8}.
#' @export
#'
#' @examples
#' x <- matrix(c(7, 6, 4, 8, 10, 11, 12, 9, 3, 5, 1, 2), 3, 4)
#' b <- apply(x, 1, sum)  # 32 16 20 row sum
#' y <- pinv(x)
#' stopifnot(all.equal(drop(x %*% y %*% b), b))

pinv <- function(x, tol = .Machine$double.eps ^ (2 / 3)) {
  checkmate::assert_matrix(x, "numeric", any.missing = FALSE, min.rows = 1L,
                           min.cols = 1L)
  checkmate::assert_number(tol, lower = 0, upper = 1e-10, finite = TRUE)

  s <- svd(x)
  p <- s$d > max(tol * s$d[[1]], 0)
  if (all(p)) {
    s$v %*% (1 / s$d * t(s$u))
  } else if (any(p)) {
    s$v[, p, drop = FALSE] %*% (1 / s$d[[p]] * t(s$u[, p, drop = FALSE]))
  } else {
    matrix(0, nrow = ncol(x), ncol = nrow(x))
  }
}


