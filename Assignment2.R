##### In this assignment we must create two functions that accomplish seperate tasks
##### Firstly, we need a function named makeCacheMatrix which creates a special "matrix" object that can cache its inverse. 
##### And secondly, we to create a function called cacheSolve which computes the inverse of the special "matrix" returned by makeCacheMatrix. 
##### If the inverse has already been calculated (and the matrix has not changed), then the cachesolve shall retrieve the inverse from 
######the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#####this function shall return a matrix that will be the inverse of our matrix

cacheSolve <- function(x = matrix(), ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  matrix <- x$get()
  inv <- solve(x = matrix(), ...)
  x$setInverse(inv)
  inv
}
