## This pair of functions collect information during the creation of a matrix
## that can retrieved by using the second function. 

## Takes a invertable matrix and caches its inverse for later use.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(myMatrix)  {
      x <<- myMatrix 
      inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse, 
         getinverse = getinverse)
}


## Produce the inverse of the "matrix" created from the makeCacheMatrix() either by
## retrieval from the cache, or computing it.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message ("getting cached data")
    return (inv)
  }
  data <- x$get() 
  inv <- solve(data, ...)
  x$setinverse
  inv
}
