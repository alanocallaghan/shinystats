loglik <- function(slope, intercept, x, y, noise_sd) {
    sum(dnorm(y, mean = (slope * x) + intercept, sd = noise_sd, log=TRUE))
}
