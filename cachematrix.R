## Allows for caching the inverse of the matrix, and calling it from the cache 
## instead of computing it every time

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    ##sets the cached inverse value to m
    setinverse <- function(inverse) m <<- inverse
    ##returns the cached value when called
    getinverse <- function() m
    #list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}

cacheSolve <- function(x, ...) {
  ##get the inverse from the cached value created with the makeCacheMatrix function, 
  ##if the cached value is not null.
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##calculate if inverse matrix is not already cached
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
