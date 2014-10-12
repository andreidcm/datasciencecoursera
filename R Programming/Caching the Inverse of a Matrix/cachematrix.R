##
## Function takes a primitive matrix and returns a list of methods
## that get/set the matrix and it's inverse.
##
makeCacheMatrix <- function(plainMatrix = matrix()) {

  #
  inverse <- NULL

  # getter for the matrix primitive
  get <- function() {
    plainMatrix
  }

  # setter for the matrix primitive and reset the inverse if the matrix changes
  set <- function(newMatrix) {
    # set new value
    plainMatrix <<- newMatrix

    if (!is.null(inverse)) {
      message("Invalidating cache")
    }
    # reset inverse
    inverse <<- NULL
  }

  # getter and setter for the inverse
  getInverse <- function() {
    inverse
  }
  setInverse <- function(newInverse) {
    inverse <<- newInverse
  }

  # return matrix object list thingy
  list(
    get = get,
    set = set,
    getInverse = getInverse,
    setInverse = setInverse
  )
}

##
## Function takes a special matrix, constructed via makeCacheMatrix,
## and returns it's inverse from cache if it's available, if not constructs it
## and stores it for later use
##
cacheSolve <- function(specialMatrix) {

  # get the inverse of our spacial matrix
  inverse <- specialMatrix$getInverse();

  # check to see if its computed
  if (!is.null(inverse)) {
    message("Getting inverse matrix from cache")
    return(inverse)
  }

  # the inverse has not been cached so we computed and store it for further use
  inverse <- solve(specialMatrix$get())
  specialMatrix$setInverse(inverse)

  #
  inverse
}
