#' @describeIn apps A demonstration of moderated t-tests.
#' @export
limmaApp <- function() {
    shiny::shinyApp(
        ui = shiny::fluidPage(

            shiny::titlePanel("Standard t-test vs moderated t-test"),
            shiny::sidebarLayout(
                shiny::sidebarPanel(
                    shiny::sliderInput(
                        inputId = "obs",
                        label = "Number of samples:",
                        min = 3,
                        max = 37,
                        value = 10
                    ),
                    shiny::sliderInput(
                        inputId = "features",
                        label = "Number of features:",
                        min = 10,
                        max = 5000,
                        value = 1000
                    ),
                ),
                shiny::mainPanel(
                    shiny::plotOutput(outputId = "distPlot")
                )
            )
        ),
        server = function(input, output) {

            output$distPlot <- shiny::renderPlot({

                inds <- list()
                obj <- shinystats::methylation

                n <- input$obs
                f <- input$features

                ind <- 1:min(n, ncol(obj))
                ind_f <- 1:f

                sub <- obj[ind_f, ind]


                X <- t(getM(sub))

                y <- sub$Age
                lms <- lapply(1:ncol(X), function(i) {
                    lm(X[, i] ~ as.numeric(y))
                })
                names(lms) <- colnames(X)
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

                # df <- data.frame(
                #     sapply(lms, function(x) summary(x)$sigma)[rownames(fit)],
                #     fit$s2.post
                # )

                ps1 <- sapply(1:ncol(X), function(i) summary(lms[[i]])$coef[1, "Pr(>|t|)"])
                ps2 <- sapply(1:ncol(X), function(i) summary(lms[[i]])$coef[2, "Pr(>|t|)"])
                lims1 <- c(min(ps1, tt1[colnames(X), "P.Value"]), 1)
                lims2 <- c(min(ps2, tt2[colnames(X), "P.Value"]), 1)

                par(mfrow = c(1:2))
                plot(
                    x = ps1,
                    y = tt1[colnames(X), "P.Value"],
                    main = "Intercept",
                    xlab = "p-value from standard t-test",
                    ylab = "p-value from moderated t-test",
                    xlim = lims1,
                    ylim = lims1,
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
                    xlim = lims2,
                    ylim = lims2,
                    pch = 19,
                    cex = 0.5,
                    log = "xy"
                )
                abline(0:1, lty = "dashed", col = "firebrick")
            })
        }
    )
}
