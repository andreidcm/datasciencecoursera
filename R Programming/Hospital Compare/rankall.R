

rankall <- function(outcome, num = "best") {

  # map the outcome filters to the data columns
  filters <- c(
    'heart attack' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
    'heart failure' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
    'pneumonia' = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  )

  ##
  ## Check valid input
  ##
  if (!outcome %in% names(filters)) {
    stop('invalid outcome')
  }

  # conveniance
  filter_column <- filters[outcome]

  # load data
  hospital_data <- read.csv('hospital-data/outcome-of-care-measures.csv', colClasses = 'character')

  ##
  ## Sanitize data
  ##
  # make sure all states are uppercase
  hospital_data$State <- toupper(hospital_data$State)

  # keep only the columns we're interested in
  hospital_data <- hospital_data[,names(hospital_data) %in% c('Hospital.Name', 'State', filter_column)]

  # logical vector with values denoting whether the filter column is numeric
  filter_is_numeric <- apply(hospital_data, 1, function(row){
    suppressWarnings(!is.na(as.numeric(row[filter_column])))
  })

  # filter out NA
  hospital_data <- hospital_data[filter_is_numeric,]

  # turn filter column numeric
  hospital_data[, filter_column] <- as.numeric(hospital_data[, filter_column])

  ##
  ## Construct rank data frame
  ##
  hospital_rank <- data.frame(
    hospital = character(),
    state = character(),
    stringsAsFactors = F
  )

  # loop over the hospital data for each estate
  for (state_data in split(hospital_data, hospital_data$State)) {

    # order by filter columns and hospital name
    order_index <- order(
      state_data[, filter_column],
      state_data[, 'Hospital.Name']
    )
    state_data <- state_data[order_index,]

    # defaults
    position <- 1
    hospital_name = ''

    # determine position of the hospital
    if (is.numeric(num)) {
      position <- num
    } else {
      if (num == 'worst') {
        position <- nrow(state_data);
      }
    }

    if (position > nrow(state_data)) {
      hospital_name = NA
    } else {
      hospital_name = state_data[position, 'Hospital.Name']
    }

    # add new state rank
    hospital_rank <- rbind(hospital_rank, data.frame(
      hospital = hospital_name,
      state = state_data[1, 'State']
    ))
  }

  hospital_rank
}
