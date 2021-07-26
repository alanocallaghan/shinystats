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

                text <- paste0(
                    "Estimates: ", paste(
                        format(coef(fit), digits = 3), collapse="; "
                    ), "\n",
                    "p-value:", format(anova(fit)$P[[1]], digits = 3)
                )
                ggplot() +
                    aes(x, y) +
                    labs(title = "Linear regression", subtitle = text) +
                    geom_point() +
                    geom_smooth(method = "lm", formula = "y~x")
            })
        }
    )
}
