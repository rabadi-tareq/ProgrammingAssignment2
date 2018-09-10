## I am writing two functions to compute the inverse of a supplied matrix and cache its value
## the routine checks if the matrix is not null and invertible.
## I am also supplying two invertible matrices for the purpose of testing.


A <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-1), nrow=3, byrow=TRUE)

B <- matrix( c(5, 1, 0,
               3,-1, 2,
               4, 0,-2), nrow=3, byrow=TRUE)


## Creates the structure for maintaining the original matrix and its cache

makeCacheMatrix <- function(x = matrix()) {

  invertedMatrix <- NULL
  
  if(is.null(x))
    stop(... = "The supplied matrix can't be NULL")
  
  if(det(x) == 0)
    stop(...="The supplied matrix is not invertable")
  
  set <- function(newMatrixValue){
    
    x <<- newMatrixValue
    intervedMatrix <<- NULL
    
  }
  
  get <- function() x
  
  setInverse <- function(matrixInverse) invertedMatrix <<- matrixInverse
  
  getInverse <- function() invertedMatrix
  
  list(get = get,set=set,setInverse=setInverse,getInverse=getInverse)
}


## Returns the inverse of the matrix if it's been already computed, or computes if it's not

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  matrix <- x$get()
  
  inverse <- solve(matrix)
  x$setInverse(inverse)
  
  inverse
  
}
