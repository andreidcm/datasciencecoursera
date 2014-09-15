##
## Read a directory full of files and reports the number of completely observed
## cases in each data file. The function should return a data frame where the
## first column is the name of the file and the second column is the number of
## complete cases
##

# returns rows that dont have any NA
getComplete <- function(file){

  # read data
  polutionData <- read.csv(file, colClasses = c("Date", "numeric", "numeric", "integer"))

  # return filtered out NA
  filteredData <- na.omit(polutionData)
}

#
complete <- function(directory, id = 1:332) {

  # check for folder trailing slash
  if (directory[length(directory)] != "/") {
    directory <- paste(directory, "/", sep = "")
  }

  # empty data frame for file stats
  statData <- data.frame(
    id = integer(),
    nobs = integer()
  )

  # pass through the files and add file stats
  for (currentId in id) {

    # construct file path
    file <- paste(
      directory,
      formatC(currentId, width = 3, flag = "0"),
      ".csv",
      sep = ""
    )

    # add row with file stats
    statData <- rbind(
      statData,
      data.frame(id = currentId, nobs = nrow(getComplete(file)))
    )
  }

  statData
}



