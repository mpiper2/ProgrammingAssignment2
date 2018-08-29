## This function generates and stores the inverse of the input matrix in a cache

## the makeCacheMatrix function produces a "special matrix" that is the inverse 
## of the input and stores it in the cache
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  } 
  get <- function() x
  setinv <- function(inverse) inv<<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}



## cacheSolve is used to take the inverse of x (returns a matrix)


cacheSolve <- function(x,...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- inverse(data, ...)
  x$setinverse(inv)
  inv
}