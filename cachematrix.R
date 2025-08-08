## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the inverse as NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL  # Reset the inverse when the matrix is changed
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the inverse
    getInverse <- function() inv
    
    # Return a list of the above functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}



## Write a short comment describing this function


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) inv <<- inverse
  
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}




