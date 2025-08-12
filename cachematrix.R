# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  # Initialize the cached inverse as NULL
    
    # Function to set the matrix
    set <- function(y) {
        x <<- y    # Set the matrix in parent environment
        inv <<- NULL  # Clear the cached inverse
    }
    
    # Function to get the matrix
    get <- function() x
    
    # Function to set the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    # Function to get the inverse
    getInverse <- function() inv
    
    # Return list of functions
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

# Function to compute or retrieve cached inverse
cacheSolve <- function(x, ...) {
    # Try to get cached inverse
    inv <- x$getInverse()
    
    # If inverse is cached (not NULL), return it with message
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # If not cached, compute inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Cache the computed inverse
    x$setInverse(inv)
    
    # Return the inverse
    inv
}

# Example usage:
# Create a matrix
m <- matrix(c(1,2,3,4), nrow=2, ncol=2)

# Create special matrix object
special_matrix <- makeCacheMatrix(m)

# First time: computes inverse
cacheSolve(special_matrix)

# Second time: retrieves from cache
cacheSolve(special_matrix)
