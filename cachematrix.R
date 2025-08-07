## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix<-function(x=matrix()){
  inv<- NULL 
  set <-function(y){
    x<<- y
    inv<<-NULL
  }
  get <-function()x
  setinverse <-function(inverse) inv <<-inverse
  getinverse<-function() inv
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Write a short comment describing this function


 
cacheinverse<- function(x, ...){
  inv<- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  matrix_to_inv_data<-x$get()
  inv<-solve(matrix_to_inv_data, ...)
  x$setinverse(inv)
  inv
}
my_Matrix<-makeCacheMatrix(matrix(1:4,2,2))
my_Matrix$get()
my_Matrix <- makeCacheMatrix(matrix(c(1,2,3,4), 2, 2))
my_Matrix <- makeCacheMatrix(matrix(c(4,3,2,1), 2, 2))  # Using an invertible matrix
result <- cacheinverse(my_Matrix)  # This will compute and store the inverse
print(result)  # This should show the inverse matrix
cacheinverse(my_Matrix)
 
result2 <- cacheinverse(my_Matrix)  # This should retrieve from cache and show the message



