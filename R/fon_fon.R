fon <- function(x,
                order = 19,
                minwn = min(as.double(rownames(x))),
                maxwn = max(as.double(rownames(x))),
                threshold = 0.05,
                alpha = 0.495,
                eps = 1e-12,
                max_iter = 500) {
  # TODO: add checkmate assertions
  n_cores <- max(1, min(ncol(x), parallel::detectCores() - 1))
  cl <- parallel::makeCluster(n_cores)
  stream <-
    parallel::parCapply(cl, spectrotest::slice(x, , minwn, maxwn),
                        backcor, order, threshold, alpha, eps, max_iter)
  parallel::stopCluster(cl)
  structure(matrix(stream, nrow = order + 1, ncol = ncol(x)),
            xscale = c(Intercept = minwn + maxwn, X = -2)/(minwn - maxwn),
            class = "fon")
}


predict.fon <- function(x, newdata = seq(349.115696, 7501.165424, length.out = 3709)){
  # TODO: add checkmate assertions
  u <- attr(x, "xscale")
  v <- outer(u['Intercept'] + u['X']*newdata, seq.int(0, nrow(x) - 1), "^")  # Vandermonde matrix
  structure(
    v %*% x, # polynomial values,
    dimnames = list(round(newdata, 6), sprintf("Cell_%0i", seq_len(ncol(x)))),
    class = "drift"
    # TODO: Add meta attributes
  )
}


fitted.fon <- function(x) {
  # TODO: add checkmate assertions
  # TODO: construct algo as -/+1 to wn, then predict.fon with newdata = wn
 1
}

backcor <- function(y, order, threshold, alpha, eps, max_iter){
  n <- length(y)
  v <- outer(1 + 2*(seq_len(n) - n)/(n - 1), seq.int(0, order), "^")  # Vandermonde matrix
  inv <- spectrotest::pinv(t(v) %*% v) %*% t(v)  # Iverse of Vandermonde matrix
  fit_criteria <- function(vec1, vec2) sum((vec1 - vec2)^2)/sum(vec2^2)
  old_y_hat <- rep.int(1, n)
  y_hat <- v %*% inv %*% y
  epoch <- 1L # iteration counter
  while (eps < fit_criteria(y_hat, old_y_hat) && epoch < max_iter) {
    old_y_hat <- y_hat
    res <- y - y_hat
    eta <- res < threshold
    coef <- (inv %*% (y + 2 * res * ((alpha - .5) * eta - .5*!eta)))[, 1L]
    y_hat <- v %*% coef  # polynomial values
    epoch <- epoch + 1L
  }
  coef[[1]] <- coef[[1]] + min(y - y_hat)
  coef
}

require(spectrotest)
# TODO: fix as an example
s <- -log10(coal_drift()/100)  # TODO: add transformation as absorp function
f <- fon(slice(s,, 650, 4500), 4, 655, 4450)
plot(cbind(s, predict(f)), xlim = c(4500, 650), ylim = c(1, 2))
