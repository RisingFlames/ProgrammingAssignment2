## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix object that can be used to cache its inverse.
#1. Set the value of the matrix
#2. Get the value of the matrix
#3. Set the value of the inverse of the matrix
#4. Get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  #Set m to NULL to have clean value
  m <- NULL
  #Create the set variable for the function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  #Create the get variable for the function
  get <- function() x
  #create the setmatrix variable for the inverse
  setmatrix <- function(solve) m <<- solve
  #Create the getmatrix variable for inverse
  getmatrix <- function() m
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix()
  #If the inverse of the matrix has already been computed,
  # don't recalculate but rather just return the already cached version
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  #Calculate the inverse of the matrix since this is the first
  # time it has been asked for and return it
  data <- x$get()
  m <- solve(data, ...)
  x$setmatrix(m)
  m
}
