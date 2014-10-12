# Write a function called best that take two arguments: the 2-character
# abbreviated name of a state and an outcome name. The function reads the
# outcome-of-care-measures.csv file and returns a character vector with the
# name of the hospital that has the best (i.e. lowest) 30-day mortality for the
# specified outcome in that state. The hospital name is the name provided in
# the Hospital.Name variable.
#
# The outcomes can be one of 'heart attack', 'heart failure', or 'pneumonia'.
#
# Hospitals that do not have data on a particular outcome should be excluded
# from the set of hospitals when deciding the rankings.
#
# Handling ties. If there is a tie for the best hospital for a given outcome,
# then the hospital names should be sorted in alphabetical order and the first
# hospital in that set should be chosen (i.e. if hospitals 'b', 'c', and 'f'
# are tied for best, then hospital 'b' should be returned)

best <- function(state, outcome) {

  # map the outcome filters to the data columns
  filters <- c(
    'heart attack' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack',
    'heart failure' = 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure',
    'pneumonia' = 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'
  )

  ##
  ##  Check valid input
  ##
  if (!outcome %in% names(filters)) {
    stop('invalid outcome')
  }

  # conveniance
  filter_column <- filters[outcome]

  # load data
  hospital_data <- read.csv('hospital-data/outcome-of-care-measures.csv', colClasses = 'character')

  # make sure all states are uppercase
  hospital_data$State <- toupper(hospital_data$State)

  ## check valid states
  state <- toupper(state);
  if (!state %in% unique(hospital_data$State)) {
    stop('invalid state')
  }

  ##
  ##  Filter data
  ##
  # keep only the columns we're interested in
  hospital_data <- hospital_data[,names(hospital_data) %in% c('Hospital.Name', 'State', filter_column)]
  # filter hospitals by state
  hospital_data_by_state <- hospital_data[hospital_data$State == state,]

  # logical vector with values denoting whether the filter column is numeric
  filter_is_numeric <- apply(hospital_data_by_state, 1, function(row){
    suppressWarnings(!is.na(as.numeric(row[filter_column])))
  })
  # filter out NA
  hospital_data_by_state <- hospital_data_by_state[filter_is_numeric,]

  # turn filter column numeric
  hospital_data_by_state[, filter_column] <- as.numeric(hospital_data_by_state[, filter_column])

  ##
  ##  Order by filter columns and hospital name
  ##
  order_index <- order(
    hospital_data_by_state[, filter_column],
    hospital_data_by_state[, 'Hospital.Name']
  )

  hospital_data_by_state <- hospital_data_by_state[order_index,]
  hospital_data_by_state[1, 'Hospital.Name']
}
