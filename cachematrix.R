## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## The input argurement is a invertible matrix.
## 

## set function generates a new "matrix" object
## get function output the current matrix
## setinv function is used to set the inverse of the matrix
## getinv funciotn is used to output the inverse

makeCacheMatrix <- function(x = numeric()){
  inv <- matrix()
  set <- function(y){
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!anyNA(inv)){
    message("getting cached data")
    return(inv)    
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  inv <- x$setinv(inverse)
  inv
}
