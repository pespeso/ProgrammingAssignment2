## Two functions for Week 3 Assignment 2, to execute and cache the inverse of a matrix
## Example to test it:
#   x <- matrix(runif(25), 5, 5)
#   test <- makeCacheMatrix(x)
#   cacheSolve(test)
#   cacheSolve(test)
## The second cacheSolve execution is to show the "getting cached data" message

## makeCacheMatrix function includes set/get methods to create and to obtain the special matrix object.
makeCacheMatrix <- function(x = matrix()) {
  # Initiatize the variables.
  m <- NULL
  
  # Method to set the matrix
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  # Method to get the matrix value
  get <- function() x
  
  # Develop the set method, assigned to the solve function
  setinv <- function(solve) m <<- solve
  
  # Develop the getinv method, assigned to the value of m (the inverse of x)
  getinv <- function() m
  
  # Return list of available methods
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Compute the inverse of the matrix created by makeCacheMatrix function.
## If the inverse has already been calculated, then cacheSolve return this already saved value.
## If not, it will calculate and store this value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  
  # Check if the inverse has already been calculated. If so, it returns the value.
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # If the inverse has not been calculated, get the special matrix value
  data <- x$get()
  
  # calculate the inverse through the solve method
  m <- solve(data) %*% data
  
  # and it uses the set method to assign the inverse of the matrix
  x$setinv(m)
  
  # finally, it returns this inverse of the matrix
  m
}
