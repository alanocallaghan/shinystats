#' @name Apps
#' @title Shiny apps to demonstrate statistical principles.
#' @examples
#' \dontrun{
#'  regressionApp()
#'  limmaApp()
#'  ridgeApp()
#'  lassoApp()
#'  elasticApp()
#' }
#' @rdname apps
NULL

#' Human methylation data on immunomagnetic sorted peripheral adult blood cells
#'
#' A random subset of 10,000 features from the Bioconductor package
#' "FlowSorted.Blood.EPIC" from Bioconductor 3.13.
#'
#' The FlowSorted.Blood.EPIC object is based in samples assayed by Brock
#' Christensen and colleagues; for details see Salas et al. 2018.
#' \url{https://www.ncbi.nlm.nih.gov/geo/query/acc.cgi?acc=GSE110554}.
#'
#' @format A \linkS4class{GenomicRatioSet} with 10,000 features and 37 samples
#' along with associated metadata.
#' @source
#' \url{https://bioconductor.org/packages/3.12/data/experiment/html/FlowSorted.Blood.EPIC.html}
"methylation"

#' @importFrom graphics abline contour hist image par points
#' @importFrom stats coef dnorm lm model.matrix rnorm anova
#' @importFrom limma lmFit topTable eBayes
#' @importFrom minfi getM
#' @importFrom SummarizedExperiment assay
#' @importFrom ggplot2 ggplot aes
#'  geom_point geom_smooth geom_tile
#'  labs lims xlab ylab xlim ylim
#'  theme_bw
#' @importFrom viridis viridis scale_colour_viridis
NULL
