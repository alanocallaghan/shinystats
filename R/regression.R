#' @describeIn apps A demonstration of linear regression principles.
#' @export
regressionApp <- function() {
    shiny::shinyApp(
        ui = shiny::fluidPage(
            shiny::titlePanel("Linear regression"),
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    shiny::sliderInput(
                        inputId = "obs",
                        label = "Number of observations:",
                        min = 10,
                        max = 1000,
                        value = 50
                    ),
                    shiny::sliderInput(
                        inputId = "slope",
                        label = "Slope:",
                        min = -10,
                        max = 10,
                        step = 0.1,
                        value = 0
                    ),
                    shiny::sliderInput(
                        inputId = "intercept",
                        label = "Intercept:",
                        min = -10,
                        max = 10,
                        value = 0
                    ),
                    shiny::sliderInput(
                        inputId = "noise",
                        label = "Noise:",
                        min = 0,
                        max = 10,
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
                n <- input$obs
                noise_sd <- input$noise
                intercept <- input$intercept
                slope <- input$slope

                x <- rnorm(n)
                noise <- rnorm(n, sd = noise_sd)
                y <- intercept + (slope * x) + noise
                fit <- lm(y ~ x)

                nsamples <- 200
                sx <- seq(min(x) * 1.1, max(x) * 1.1, length.out = nsamples)
                sy <- seq(min(y) * 1.1, max(y) * 1.1, length.out = nsamples)
                fx <- cbind(rep(1, nsamples), sx) %*% c(intercept, slope)

                dens <- matrix(NA, nrow = nsamples, ncol = nsamples,
                    dimnames = list(sx, sy)
                )
                for (i in seq_along(sx)) {
                    for (j in seq_along(sy)) {
                        dens[i, j] <- dnorm(
                            sy[[j]],
                            mean = fx[[i]],
                            sd = noise_sd
                        )
                    }
                }
                mdf <- reshape2::melt(dens)

                xlim <- xlim(range(x) * 1.1)
                ylim <- ylim(range(y) * 1.1)

                g1 <- ggplot(mdf) +
                    aes(x = Var1, y = Var2, fill = value) +
                    geom_tile() +
                    scale_colour_viridis(
                        name = "Probability",
                        aesthetics = "fill"
                    ) +
                    xlim + ylim +
                    labs(x = "x", y = "y") +
                    theme_bw()

                text <- paste0(
                    "Estimates: ", paste(
                        format(coef(fit), digits = 3), collapse = "; "
                    ), "\n",
                    "p-value:", format(anova(fit)$P[[1]], digits = 3)
                )

                g2 <- ggplot() +
                    aes(x, y) +
                    labs(title = "Linear regression", subtitle = text) +
                    geom_point() +
                    geom_smooth(method = "lm", formula = "y~x") +
                    xlim + ylim +
                    theme_bw()
                cowplot::plot_grid(g1, g2)
            })
        }
    )
}
