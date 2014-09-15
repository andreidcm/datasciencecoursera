##
## Calculates the mean of a pollutant (sulfate or nitrate) across a specified
## list of monitors. The function 'pollutantmean' takes three arguments:
##   'directory', 'pollutant', and 'id'.
##
## Given a vector monitor ID numbers, 'pollutantmean' reads that monitors'
## particulate matter data from the directory specified in the 'directory'
## argument and returns the mean of the pollutant across all of the monitors,
## ignoring any missing values coded as NA
##

pollutantmean <- function(directory, pollutant, id = 1:332) {

  # check for folder trailing slash
  if (directory[length(directory)] != "/") {
    directory <- paste(directory, "/", sep = "")
  }

  # construct data files path -  format file name as 00x.csv
  polutionFiles <- paste(
    directory,
    formatC(id, width = 3, flag = "0"),
    ".csv",
    sep = ""
  )

  # create empty data frame
  polutionData <- data.frame(
    Date = as.Date(character()),
    sulfate = integer(),
    nitrate = integer(),
    id = integer()
  )

  # pass through the files and add the data to the main data frame
  for (file in polutionFiles) {
    polutionData <- rbind(
      read.csv(file, colClasses = c("Date", "numeric", "numeric", "integer")),
      polutionData
    )
  }

  # return rounded mean with NA removed
  round(
    mean(polutionData[[pollutant]], na.rm = TRUE),
    3
  )
}


