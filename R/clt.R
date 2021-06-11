cltApp <- function() {
    shiny::shinyApp(
        ui = shiny::fluidPage(
            shiny::titlePanel("Central limit theorem example"),
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    shiny::sliderInput(
                        inputId = "obs",
                        label = "Number of observations per sample:",
                        min = 1,
                        max = 500,
                        value = 5
                    ),
                    shiny::sliderInput(
                        inputId = "samples",
                        label = "Number of samples:",
                        min = 1,
                        max = 500,
                        value = 50
                    ),
                    shiny::selectInput(
                        "distribution",
                        "Type of distribution:",
                        choices = c(
                            "normal" = "rnorm",
                            "binomial" = "rbinom",
                            "exponential" = "rexp",
                            "t-distribution" = "rt",
                            "cauchy" = "rcauchy",
                            "negative binomial" = "rnbinom",
                            "gamma" = "rgamma"
                        )
                    )
                ),
                # Main panel for displaying outputs ----
                shiny::mainPanel(
                    shiny::plotOutput(outputId = "distPlot")
                )
            )
        ),
        server = function(input, output) {

            output$distPlot <- shiny::renderPlot({
                if (input$distribution %in% c("rbinom", "rnbinom")) {
                    args <- list(input$obs, 100, 0.5)
                } else if (input$distribution == "rexp") {
                    args <- list(input$obs)
                } else {
                    args <- list(input$obs, 1, 1)
                }
                data <- replicate(
                    input$samples,
                    mean(do.call(input$distribution, args))
                )
                hist(
                    data,
                    breaks = "FD",
                    freq = FALSE,
                    xlab = "Sample mean",
                    main = "Histogram of sample means")
                }
            )
        }
    )
}
