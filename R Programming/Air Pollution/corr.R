##
## Takes a directory of data files and a threshold for complete cases and
## calculates the correlation between sulfate and nitrate for monitor
## locations where the number of completely observed cases (on all variables)
## is greater than the threshold. The function should return a vector of
## correlations for the monitors that meet the threshold requirement.
## If no monitors meet the threshold requirement, then the function should
## return a numeric vector of length 0.
##

corr <- function(directory, threshold = 0) {

  # check for folder trailing slash
  if (directory[length(directory)] != "/") {
    directory <- paste(directory, "/", sep = "")
  }

  # get all monitor files
  polutionFiles <- list.files(directory, full.names = TRUE)

  # vector that holds the sulfate/nitrate corelatin for each monitor with
  # complete cases above the threshold
  monitorsCorelation <- numeric()

  for (file in polutionFiles) {

    # get monitor data
    polutionData <- getComplete(file)

    # check threshold
    if (nrow(polutionData) > threshold) {

      # add to corelation vector
      monitorsCorelation <- c(
        monitorsCorelation,
        round(
          cor(polutionData$sulfate, polutionData$nitrate),
          4
        )
      )

    }
  }

  monitorsCorelation
}
