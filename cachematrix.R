# Cache. Store the value of this function as cache_<matrix name>

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  get <- function() x
  setinv <- function(solve) m <<- solve(x)
  getinv <- function() m
  list(get = get, setinv = setinv, getinv = getinv)
}

# Compute. Give cache_<matrix name> as the input to this function.

cacheSolve <- function(x) {
  invm <- x$getinv()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  newm <- x$get()
  invm <- solve(newm)
  x$setinv(invm)
  invm # Return the inverse of the given matrix.
}