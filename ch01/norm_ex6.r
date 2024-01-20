png(filename = "norm_ex6.png", width = 800, height = 800)

my_dnorm <- function(x, mu, sig2) {
  (2 * pi * sig2)^(-1 / 2) * exp(-(x - mu)^2 / (2 * sig2))
}

phi.seq <- c(-4, -2, 0, 2)
n.seq <- c(1, 5, 10, 20)
m <- length(phi.seq)
l <- length(n.seq)
n <- 30
x <- rnorm(n)
par(mfrow = c(2, 2))
for (phi in phi.seq) {
  plot(0, 0, xlim = c(-5, 7), ylim = c(0, 0.5), type = "n")
  for (k in 1:l) {
    nn <- n.seq[k]
    mu <- (phi + sum(x[1:nn])) / (nn + 1)
    sig2 <- (nn + 2) / (nn + 1)
    curve(my_dnorm(x, mu, sig2), col = k + 1, add = TRUE)
    title(paste("phi=", phi))
  }

  curve(dnorm(x), lwd = 2, lty = 2, col = 1, add = TRUE)
  legend("topright",
    c("True", n.seq),
    lty = c(2, rep(1, 4)),
    lwd = c(2, rep(1, 4)),
    col = 1:(l + 1)
  )
}

par(mfrow = c(1, 1))
