suppressPackageStartupMessages({
    library("FlowSorted.Blood.EPIC")
    library("ExperimentHub")
    library("here")
})

hub <- ExperimentHub()
query(hub, "FlowSorted.Blood.EPIC")  

FlowSorted.Blood.EPIC <- hub[["EH1136"]]  

methylation <- preprocessQuantile(FlowSorted.Blood.EPIC)
y <- as.numeric(factor(methylation$smoker)) - 1

cc <- complete.cases(y)
methylation <- methylation[, cc]
set.seed(42)
methylation <- methylation[sample(nrow(methylation), 5000), ]
usethis::use_data(methylation, overwrite=TRUE)
