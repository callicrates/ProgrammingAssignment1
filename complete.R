## R Programming
## Coursera Data Science Specialization
## Programming Assignment 2: I Can't Breathe!

complete <- function(directory, id = 1:332) {
    # Find the number of complete cases in the requested data files.
    #
    # Args:
    #   directory: Character vector: directory containing the data files
    #   id: Integer vector:  which sensors to use in the count
    #
    # Returns:
    #   Data frame where column 1 is the ID and column 2 is the
    #   number of complete cases for that ID

    loadSingleFile <- function(fileId) {
        filename <- sprintf("%s/%03d.csv", directory, as.integer(fileId))
#        message(sprintf("Reading data table from %s", filename))
        read.csv(filename, header=TRUE)
    }

    # I know I could also use complete.cases here but I want to learn
    # more about how to do things in the language before I start using
    # the library for everything.
    countCompleteCases <- function(frame) {
        rowContainsNA <- apply(frame, 1, function(row) any(is.na(row)))
        sum(!rowContainsNA)
    }

    framesFromFiles <- lapply(id, loadSingleFile)
    completeCounts <- lapply(framesFromFiles, countCompleteCases)

    data.frame(id=I(id), nobs=I(completeCounts))
}
