## A pair of functions that cache a matrix's inverse

## Approach based on the assignment example: Caching the mean of a vector

## makeCacheMatrix creates a list object that contains four functions
##   two that get/set a matrix and two that get/set its inverse
## the inverse is calculated via the cacheSolve function 
##  unless it already exists in the list cache, in which case it returns the cache

## Assuming x is an invertible matrix. 


## Return a list object that can keep track of a matrix and its inverse

makeCacheMatrix <- function(matrix_data = matrix()) {
  inverse <- NULL
  set <- function(new_matrix) {
    matrix_data <<- new_matrix
    inverse <<- NULL
  }
  get <- function() matrix_data
  setinverse <- function(new_inverse) inverse <<- new_inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Return the cached inverse or calculate it

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  else {
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    return(inv)
  }
  
}
