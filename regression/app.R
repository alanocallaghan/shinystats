library("shiny")

ui <- fluidPage(

    titlePanel("Linear regression"),
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "obs",
                label = "Number of observations:",
                min = 10,
                max = 1000,
                value = 50
            ),
            sliderInput(
                inputId = "slope",
                label = "Slope:",
                min = -10,
                max = 10,
                value = 0
            ),
            sliderInput(
                inputId = "intercept",
                label = "Intercept:",
                min = -10,
                max = 10,
                value = 0
            ),
            sliderInput(
                inputId = "noise",
                label = "Noise:",
                min = 0,
                max = 10,
                value = 1
            )
            # ,
            # selectInput(
            #     "distribution",
            #     "Type of distribution:",
            #     choices = c(
            #         "normal" = "rnorm",
            #         "binomial" = "rbinom",
            #         "exponential" = "rexp",
            #         "t-distribution" = "rt",
            #         "cauchy" = "rcauchy",
            #         "negative binomial" = "rnbinom",
            #         "gamma" = "rgamma"
            #     )
            # )
        ),

        # Main panel for displaying outputs ----
        mainPanel(
            plotOutput(outputId = "distPlot")
        )
    )
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        # if (input$distribution %in% c("rbinom", "rnbinom")) {
        #     args <- list(input$obs, 100, 0.5)
        # } else if (input$distribution == "rexp") {
        #     args <- list(input$obs)
        # } else {
        #     args <- list(input$obs, 1, 1)
        # }

        n <- input$obs
        x <- rnorm(n)
        noise <- rnorm(n, sd = input$noise)
        y <- (input$slope * x) + input$intercept + noise
        fit <- lm(y ~ x)
        # plot(
        #     x,
        #     y
        # )
        # abline(fit)
        library("ggplot2")
        ggplot() +
            aes(x, y) +
            geom_point() +
            geom_smooth(method = "lm", formula = "y~x")
    })
}

shinyApp(ui = ui, server = server)
