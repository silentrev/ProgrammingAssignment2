## The first function, makeChacheMatrix, creates a list 
## contaning the matrix and handles to the methods defined 
## by the function. The input to the function needs to be 
## a squared matrix

makeCacheMatrix <- function(m = matrix()) {

  ## Set inverse property to NULL
  i <- NULL
  
  ## Function to set the matrix
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  ## Function to get the matrix
  get <- function() {
    ## Return the matrix
    m
  }
  
  ## Method to set the inverse of the matrix
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  ## Method to get the inverse of the matrix
  getInverse <- function() {
    ## Return the inverse property
    i
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}



## cacheSolve calculates the inverse of the matrix created 
## by the makeCacheMatrix function.  If the inverse has 
## already been calculated, the cached value is returned.

cacheSolve <- function(x, ...) {

  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix 
  data <- x$get()
  
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
}
