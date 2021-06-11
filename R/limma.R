#' @describeIn apps A demonstration of moderated t-tests.
#' @export
limmaApp <- function() {
    obj <- readRDS("/home/alan/Documents/github/carpentries/high-dimensional-stats-r/data/FlowSorted_Blood_EPIC.rds")  
    # obj <- obj[1:5000, obj$bmi_clas %in% c("Normal", "Obese")]
    # obj <- obj[1:2000, ]
    shiny::shinyApp(
        ui = shiny::fluidPage(

            shiny::titlePanel("Standard t-test vs moderated t-test"),
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    shiny::sliderInput(
                        inputId = "obs",
                        label = "Number of observations per group:",
                        min = 4,
                        max = 37,
                        value = 10
                    ),
                    shiny::sliderInput(
                        inputId = "features",
                        label = "Number of features:",
                        min = 10,
                        max = 10000,
                        value = 1000
                    ),
                    # sliderInput(
                    #     inputId = "intercept",
                    #     label = "Intercept:",
                    #     min = -10,
                    #     max = 10,
                    #     value = 0
                    # ),
                    # sliderInput(
                    #     inputId = "noise",
                    #     label = "Noise:",
                    #     min = 0,
                    #     max = 10,
                    #     value = 1
                    # )
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
                shiny::mainPanel(
                    shiny::plotOutput(outputId = "distPlot")
                )
            )
        ),
        server = function(input, output) {

            output$distPlot <- shiny::renderPlot({
                library("shiny")
                library("limma")
                library("minfi")
                library("ggplot2")

                inds <- list()
                # for (x in unique(obj$bmi_clas)) {
                #     inds[[x]] <- which(obj$bmi_clas == x)[1:n]
                # }
                # ind <- c(inds[[1]], inds[[2]])
                n <- input$obs
                f <- input$features

                ind <- 1:min(n, ncol(obj))
                ind_f <- 1:f

                sub <- obj[ind_f, ind]


                X <- t(getM(sub))
                # y <- factor(sub$bmi_clas,
                #     levels = c("Normal", "Obese"),
                #     ordered = TRUE
                # )
                y <- sub$Age
                lms <- lapply(1:ncol(X), function(i) {
                    lm(X[, i] ~ as.numeric(y))
                })
                cc1 <- sapply(1:ncol(X), function(i) {
                    coef(lms[[i]])[[1]]
                })
                cc2 <- sapply(1:ncol(X), function(i) {
                    coef(lms[[i]])[[2]]
                })

                design <- model.matrix(~y)
                colnames(design) <- c("intercept", "age")
                fit <- lmFit(t(X), design = design)
                fit <- eBayes(fit)
                tt1 <- topTable(fit, coef = 1, number = nrow(fit))
                tt2 <- topTable(fit, coef = 2, number = nrow(fit))

                ps1 <- sapply(1:ncol(X), function(i) summary(lms[[i]])$coef[1, "Pr(>|t|)"])
                ps2 <- sapply(1:ncol(X), function(i) summary(lms[[i]])$coef[2, "Pr(>|t|)"])

                par(mfrow = c(1:2))
                plot(
                    x = ps1,
                    y = tt1[colnames(X), "P.Value"],
                    main = "Intercept",
                    xlab = "p-value from standard t-test",
                    ylab = "p-value from moderated t-test",
                    pch = 19,
                    cex = 0.5,
                    log = "xy"
                )
                abline(0:1, lty = "dashed", col = "firebrick")
                plot(
                    x = ps2,
                    y = tt2[colnames(X), "P.Value"],
                    main = "Covariate",
                    xlab = "p-value from standard t-test",
                    ylab = "p-value from moderated t-test",
                    pch = 19,
                    cex = 0.5,
                    log = "xy"
                )
                abline(0:1, lty = "dashed", col = "firebrick")
                


                # ggplot() +
                #     aes(x, y) +
                #     geom_point() +
                #     geom_smooth(method = "lm", formula = "y~x")
            })
        }
    )
}
