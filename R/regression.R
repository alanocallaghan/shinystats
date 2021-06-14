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

                n <- input$obs
                x <- rnorm(n)
                noise <- rnorm(n, sd = input$noise)
                y <- (input$slope * x) + input$intercept + noise
                fit <- lm(y ~ x)
                ggplot() +
                    aes(x, y) +
                    geom_point() +
                    geom_smooth(method = "lm", formula = "y~x")
            })
        }
    )
}
