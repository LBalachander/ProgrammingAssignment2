## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function is to initiate the matrix creation and its functions

makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) invM <<- inverse
  
  getInverse <- function() invM
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)  
  
  
}


## Write a short comment describing this function
## this function is to create inverse and caching it 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Return the inverse matrix of x
  invM <- x$getInverse()
  
  if (!is.null(invM)) {
    message("Providing cached data")
    return(invM)
  }
  
  mat <- x$get()
  
  invM <- solve(mat, ...)
  
  x$setInverse(invM)
  
  invM
  
}
