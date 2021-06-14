#' @name Apps
#' @title Shiny apps to demonstrate statistical principles.
#' 
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
#' @format A \linkS4class{GenomicRatioSet} with 10,000 features and 37 samples along with associated metadata.
#' @source \url{https://bioconductor.org/packages/release/data/experiment/html/FlowSorted.Blood.EPIC.html}
"methylation"

#' @importFrom graphics abline contour hist image par points
#' @importFrom stats coef dnorm lm model.matrix rnorm
#' @importFrom limma lmFit topTable eBayes
#' @importFrom minfi getM
#' @importFrom ggplot2 aes geom_point geom_smooth ggplot
#' @importFrom viridis viridis
NULL
