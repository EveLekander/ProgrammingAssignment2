## Put comments here that give an overall description of what your
## functions do

##Caching the inverse of matrix
## Matrix inversions are usually costly computations and there are benefits from
##caching the inverse of matrix rather than repeated computations.

## Function creates a special "matrix" object that can cache its inverse.
## Using set function, the input matrix and its inverse can be defined from parent environment directly.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL              ## inverse result of x 
  set <- function(y) {
    x <<- y                ## input argument to the x from parent enivornment, if y not defined
    inv <<- NULL           ## assigning value NULL to inv
  }
  get <- function() x      ## get the matrix x
  setInverse <- function(inverse) inv <<- inverse ##set the inverse matrix, inv, from parent environment
  
  getInverse <- function () inv ##get inverse for matrix inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Function computes the inverse of the special "matrix" returned by makeCacheMatric above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.
## function checks if the inverse is existing in the results of makecachematrix.
## If results exist, output will be retrieved from cahced data,
## If not, function calculates inverse of input matrix makeCacheMatrix with solve function and cahces result using setInverse of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()        ## Retrieve inverse of matrix
  if(!is.null(inv)) {          ## if inv not NULL, inv already calculated
    message("getting cahced data")
    return(inv)                ## give inv value
  }
  data <- x$get()              ## otherwise inv is NULL, then data is x
  inv <- solve(data, ...)      ## solve inverse matrix and assing results to inv
  x$setInverse(inv)            ## cache value inv using setInverse in makeCacheMatrix
  inv                          ## return value of inv (inverse of x)
}
