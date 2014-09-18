## Put comments here that give an overall description of what your
## functions do

#Matrix inversion is usually a costly computation and there may 
#be some benefit to caching the inverse of a matrix rather than 
#computing it repeatedly. We write a pair of functions that cache 
#the inverse of a matrix.


## Write a short comment describing this function

#This function creates a special "matrix" object that can 
#cache its inverse.

makeCacheMatrix <- function(x = numeric()) {
  g <- NULL
  set <- function(y) {
    x <<- y
    g <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) g <<- solve
  getsolve <- function() g
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

#This function computes the inverse of the special "matrix"
#returned by makeCacheMatrix above. If the inverse has already
#been calculated (and the matrix has not changed), then cacheSolve 
#should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  g <- x$getsolve()
  if(!is.null(g)) {
    message("getting cached data")
    return(g)
  }
  data <- x$get()
  g <- solve(data, ...)
  x$setsolve(g)
  g
}
