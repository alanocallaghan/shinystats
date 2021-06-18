#' @describeIn apps A demonstration of LASSO regression.
#' @export
lassoApp <- function() {
    shiny::shinyApp(
        ui = shiny::fluidPage(
            shiny::titlePanel("LASSO regression"),
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    shiny::sliderInput(
                        inputId = "obs",
                        label = "Number of observations:",
                        min = 10,
                        max = 1000,
                        value = 15
                    ),
                    shiny::sliderInput(
                        inputId = "slope",
                        label = "Slope:",
                        min = -10,
                        max = 10,
                        step = 0.1,
                        value = 1
                    ),
                    shiny::sliderInput(
                        inputId = "intercept",
                        label = "Intercept:",
                        min = -5,
                        max = 5,
                        step = 0.1,
                        value = 1
                    ),
                    shiny::sliderInput(
                        inputId = "noise",
                        label = "Noise:",
                        min = 0.1,
                        max = 5,
                        step = 0.1,
                        value = 2
                    ),
                    shiny::sliderInput(
                        inputId = "l1",
                        label = "Max L1 norm:",
                        min = 0.1,
                        max = 10,
                        step = 0.1,
                        value = 1
                    )
                ),
                shiny::mainPanel(
                    shiny::plotOutput(outputId = "distPlot")
                )
            )
        ),
        server = function(input, output) {
            output$distPlot <- shiny::renderPlot({
                set.seed(42)
                noise_sd <- input$noise
                nobs <- input$obs
                slope <- input$slope
                intercept <- input$intercept
                maxlim <- max(abs(slope), abs(intercept)) * 2
                maxlim <- max(maxlim, 5)
                lims <- c(-maxlim, maxlim)
                l1 <- input$l1
                x <- rnorm(nobs, mean = 0, sd = 1)
                noise <- rnorm(nobs, mean = 0, sd = noise_sd)
                y <- (slope * x) + (intercept) + noise

                loglik <- function(slope, intercept) {
                    sum(dnorm(y, mean = (slope * x) + intercept, sd = noise_sd, log=TRUE))
                }
                n <- 200

                s <- seq(-maxlim, maxlim, length.out = n)

                ll <- matrix(ncol = n, nrow = n)
                coef <- mask <- norm_mat <- matrix(ncol = n, nrow = n)
                for (i in seq_along(s)) {
                    coef[, ] <- s
                }
                for (i in seq_along(s)) {
                    for (j in seq_along(s)) {
                        norm_mat[i, j] <- abs(s[[i]]) + abs(s[[j]])
                        ll[i, j] <- loglik(s[i], s[j])
                    }
                }
                mask <- norm_mat <= l1
                image(s, s, ll,
                    xlab = "slope", ylab = "intercept",
                    col = viridis(40, option = "A", direction=1)
                )
                abline(v = 0, lty = "dashed")
                abline(h = 0, lty = "dashed")
                # points(slope, intercept, pch=19)
                fit <- lm(y ~ x)
                # pll <- ll + abs(s) * 2
                # points(coef(fit)[[2]], coef(fit)[[1]], pch = 19)
                ind <- arrayInd(which.max(ll), dim(ll))
                points(coef[ind[[1]]], coef[ind[[2]]], pch = 19, cex = 2, col = "firebrick")
                contour(s, s, norm_mat, add = TRUE, levels = l1, drawlabels = FALSE)
                if (l1 > 0) {
                    pll <- ll * as.numeric(mask)
                    pll[pll == 0] <- NA
                    ind <- arrayInd(which.max(pll), dim(pll))
                    points(coef[ind[[1]]], coef[ind[[2]]], pch = 19, cex = 2, col = "dodgerblue")
                }
            })
        }

    )
}
