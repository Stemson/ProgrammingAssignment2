## makeCacheMatrix and cacheSolve are complementary fucntions that enable the user to compute and cache the inverse of 
## a matrix. makeCacheMatrix creates a special 'matrix' that's inverse matrix may be solved and cached via the cacheSolve function,
## assuming the intial matrix is invertible. 

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matirx()) {
  
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}

## cacheSolve returns the matrix that is the inverse of 'x'.
## Where x is a special 'matrix' returned by calling the makeCacheMatrix function on an invertible matrix.
## If the inverse has not been cached, then it will instead be computed via the solve() function.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
  
}