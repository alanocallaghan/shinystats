#' @describeIn apps A demonstration of ridge regression.
#' @export
MLEApp <- function() {
    shiny::shinyApp(
        ui = shiny::fluidPage(
            shiny::titlePanel("Maximum likelihood estimation"),
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
                        inputId = "noise",
                        label = "Noise:",
                        min = 0.1,
                        max = 5,
                        step = 0.1,
                        value = 2
                    ),
                    shiny::sliderInput(
                        inputId = "slope",
                        label = "True slope:",
                        min = -10,
                        max = 10,
                        step = 0.1,
                        value = 1
                    ),
                    shiny::sliderInput(
                        inputId = "intercept",
                        label = "True intercept:",
                        min = -5,
                        max = 5,
                        step = 0.1,
                        value = 1
                    ),
                    shiny::sliderInput(
                        inputId = "estslope",
                        label = "Estimated slope:",
                        min = -10,
                        max = 10,
                        step = 0.1,
                        value = 1
                    ),
                    shiny::sliderInput(
                        inputId = "estintercept",
                        label = "Estimated intercept:",
                        min = -5,
                        max = 5,
                        step = 0.1,
                        value = 1
                    ),
                    shiny::checkboxInput("residuals", "Show residuals")
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
                coef <- matrix(ncol = n, nrow = n)
                for (i in seq_along(s)) {
                    coef[, ] <- s
                }
                for (i in seq_along(s)) {
                    for (j in seq_along(s)) {
                        ll[i, j] <- loglik(s[i], s[j], x, y, noise_sd)
                    }
                }

                estslo <- input$estslope
                estint <- input$estintercept

                par(mfrow = 1:2)
                plot(x, y, pch = 19, main = "Data and model")
                abline(
                    a = estint,
                    b = estslo, col = "firebrick"
                )
                if (input$residuals) {
                    yhat <- x
                    for (i in seq_along(x)) {
                        yhat[[i]] <- (x[[i]] * estslo) + estint
                        lines(
                            x = rep(x[[i]], each = 2), y = c(yhat[[i]], y[[i]]),
                            lty = "dashed"
                        )
                    }
                }
                image(s, s, ll,
                    xlab = "slope", ylab = "intercept",
                    main = "Likelihood",
                    col = viridis(40, option = "A", direction = 1),
                    xlim = lims, ylim = lims
                )
                abline(v = 0, lty = "dashed")
                abline(h = 0, lty = "dashed")
                points(
                    estslo, estint,
                    pch = 19, cex = 2, col = "firebrick"
                )
            })
        }
    )
}
