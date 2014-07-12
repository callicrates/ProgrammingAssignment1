## R Programming
## Coursera Data Science Specialization
## Programming Assignment 2: I Can't Breathe!

library(data.table)

pollutantmean <- function(directory, pollutant, id = 1:332) {
    # Compute the mean for one pollutant over some set of sensors.
    #
    # Args:
    #   directory: Character vector: directory containing the data files
    #   pollutant: Character vector: which pollutant to compute (currently "sulfate" or "nitrate")
    #   id: Integer vector:  which sensors to use to compute the mean
    #
    # Returns:
    #   Sample mean of the desired pollutant at the specified sensors

    loadSingleFile <- function(fileId) {
        filename <- sprintf("%s/%03d.csv", directory, as.integer(fileId))
#        message(sprintf("Reading data table from %s", filename))
        read.csv(filename, header=TRUE)
    }

    framesFromFiles <- lapply(id, loadSingleFile)
    allData <- rbindlist(framesFromFiles)

    myColumn <- allData[[pollutant]]
    cleanedColumn <- myColumn[!is.na(myColumn)]
    mean(cleanedColumn)
}
