#' @describeIn apps A demonstration of ridge regression.
#' @export
ridgeApp <- function() {
    shiny::shinyApp(
        ui = shiny::fluidPage(
            shiny::titlePanel("Ridge regression"),
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    shiny::sliderInput(
                        inputId = "obs",
                        label = "Number of observations:",
                        min = 5,
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
                        inputId = "l2",
                        label = "Max L2 norm:",
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

                l2 <- input$l2
                x <- rnorm(nobs, mean = 0, sd = 1)
                noise <- rnorm(nobs, mean = 0, sd = noise_sd)
                y <- (slope * x) + (intercept) + noise

                n <- 200
                s <- seq(-maxlim, maxlim, length.out = n)

                ll <- matrix(ncol = n, nrow = n)
                coef <- mask <- norm_mat <- matrix(ncol = n, nrow = n)
                for (i in seq_along(s)) {
                    coef[, ] <- s
                }
                for (i in seq_along(s)) {
                    for (j in seq_along(s)) {
                        norm_mat[i, j] <- sqrt(s[[i]]^2 + s[[j]]^2)
                        ll[i, j] <- loglik(s[i], s[j], x, y, noise_sd)
                    }
                }
                mask <- norm_mat <= l2
                ind_mle <- arrayInd(which.max(ll), dim(ll))
                pll <- ll * as.numeric(mask)
                pll[pll == 0] <- NA
                ind_ple <- arrayInd(which.max(pll), dim(pll))

                par(mfrow = 1:2)

                plot(x, y, pch = 19)
                abline(
                    a = coef[ind_ple[[2]]],
                    b = coef[ind_ple[[1]]], col = "dodgerblue"
                )
                abline(
                    a = coef[ind_mle[[2]]],
                    b = coef[ind_mle[[1]]], col = "firebrick"
                )

                image(s, s, ll,
                    xlab = "slope", ylab = "intercept",
                    col = viridis(40, option = "A", direction = 1),
                    xlim = lims, ylim = lims
                )
                abline(v = 0, lty = "dashed")
                abline(h = 0, lty = "dashed")
                points(
                    coef[ind_mle[[1]]], coef[ind_mle[[2]]],
                    pch = 19, cex = 2, col = "firebrick"
                )
                contour(
                    s, s, norm_mat,
                    add = TRUE, levels = l2, drawlabels = FALSE
                )
                points(
                    coef[ind_ple[[1]]], coef[ind_ple[[2]]],
                    pch = 19, cex = 2, col = "dodgerblue"
                )
            })
        }
    )
}
