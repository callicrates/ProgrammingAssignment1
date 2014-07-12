## Data Science Specialization: R Programming
## Assignment 1
## Part 3: Correlate Sensor Values


corr <- function(directory, threshold = 0) {
    ## Correlate sensor values at sensors with enough valid data
    #
    # Args:
    #
    #   directory: character vector of length 1 with a path to the
    #              data files
    #
    #   threshold: Only process files with at least this many complete
    #              cases

    # Oh dear.  This is Part 2 in one line.
    countCompleteRows <- function(frame) {
        sum( ! apply(frame, 1, function(row) any(is.na(row))) )
    }

    computeCorrelation <- function(frame) {
        cor(frame[["sulfate"]], frame[["nitrate"]], use="pairwise.complete.obs")
    }

    computeCorrelationForFile <- function(filename) {
#        message("Loading data from file ", filename)
        data <- read.csv(filename)
        completeCases <- countCompleteRows(data)
#        message("File contains ", completeCases, " complete cases")
        if (countCompleteRows(data) > threshold) {
            return(computeCorrelation(data))
        } else {
            return(NA)
        }
    }


    allFiles <- list.files(path=directory, full.names=TRUE)
    allCorrelationValues <- lapply(allFiles, computeCorrelationForFile)

    allCorrelationValues <- allCorrelationValues[!is.na(allCorrelationValues)]
    unlist(allCorrelationValues)
}
