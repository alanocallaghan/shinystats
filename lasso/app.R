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
                value = 15
            ),
            sliderInput(
                inputId = "slope",
                label = "Slope:",
                min = -10,
                max = 10,
                step = 0.1,
                value = 1
            ),
            sliderInput(
                inputId = "intercept",
                label = "Intercept:",
                min = -5,
                max = 5,
                step = 0.1,
                value = 1
            ),
            sliderInput(
                inputId = "noise",
                label = "Noise:",
                min = 0.1,
                max = 5,
                step = 0.1,
                value = 2
            ),
            sliderInput(
                inputId = "l1",
                label = "L1 penalty:",
                min = 0.1,
                max = 5,
                step = 0.1,
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
            plotOutput(outputId = "distPlot", width = "1000px", height = "800px")
        )
    )
)

server <- function(input, output) {

    output$distPlot <- renderPlot({
        library("viridis")
        set.seed(42)
        noise_sd <- input$noise
        nobs <- input$obs
        slope <- input$slope
        intercept <- input$intercept
        l1 <- input$l1
        x <- rnorm(nobs, mean = 0, sd = 1)
        noise <- rnorm(nobs, mean = 0, sd = noise_sd)
        y <- (slope * x) + (intercept) + noise

        lik <- function(slope, intercept) {
            sum(dnorm(y, mean = (slope * x) + intercept, sd = noise_sd, log=TRUE))
        }
        n <- 200
        s <- seq(-3, 3, length.out = n)
        ll <- matrix(ncol = n, nrow = n)
        coef <- matrix(ncol = n, nrow = n)
        mask <- matrix(ncol = n, nrow = n)
        for (i in seq_along(s)) {
            coef[, ] <- s
        }
        for (i in seq_along(s)) {
            for (j in seq_along(s)) {
                mask[i, j] <- abs(s[[i]]) + abs(s[[j]]) < l1
                ll[i, j] <- lik(s[i], s[j])
            }
        }
        image(s, s, ll,
            xlab = "slope", ylab = "intercept",
            col = viridis(50, option = "A", direction=1)
        )
        abline(v = 0, lty = "dashed")
        abline(h = 0, lty = "dashed")
        # points(slope, intercept, pch=19)
        fit <- lm(y ~ x)
        # pll <- ll + abs(s) * 2
        # points(coef(fit)[[2]], coef(fit)[[1]], pch = 19)
        ind <- arrayInd(which.max(ll), dim(ll))
        points(coef[ind[[1]]], coef[ind[[2]]], pch = 19)
        if (l1 > 0) {
            pll <- ll * as.numeric(mask)
            pll[pll == 0] <- NA
            ind <- arrayInd(which.max(pll), dim(pll))
            points(coef[ind[[1]]], coef[ind[[2]]], pch = 8, cex = 3)
        }

        # l1 <- 1
        lines(c(0, l1), c(l1, 0))
        lines(c(0, -l1), c(-l1, 0))
        lines(c(-l1, 0), c(0, l1))
        lines(c(0, l1), c(-l1, 0))
    })
}

shinyApp(ui = ui, server = server)
