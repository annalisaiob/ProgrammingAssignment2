## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # initialise empty variable for inverse of matrix
  i <- NULL
  # "matrix setter", to change the matrix after the object has been initialised, to not have to call the makeCacheM function again
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # "matrix getter", to get the content of the matrix 
  get <- function() x
  # "inverse setter", to set the value of the inverse matrix
  setin <- function(solve) i <<- solve
  # "inverse getter", to get the inverse matrix
  getin <- function() i
  list(set = set, get = get,
       setin = setin,
       getin = getin)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # retrieve inverse from object
  i <- x$getin()
  # check if i is null, and if it's not return the cached data
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  # get the initial matrix
  data <- x$get()
  # calculate its inverse
  i <- solve(data, ...)
  # set the inverse as i
  x$setin(i)
  i
}
