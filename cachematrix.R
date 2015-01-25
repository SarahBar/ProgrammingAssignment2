## The following R script allows you to store an invertible matrix and find its inverse when needed.
## The inverse of the matrix is then cached for later use.


## The makeCacheMatrix function sets/gets the matrix and sets/gets a cache for storing its inverse.

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
      m <<- NULL
  }
  get <- function() x
  set_inverse <- function(inverse) m <<- inverse
  get_inverse <- function() m
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## The cacheSolve function checks if the inverse has already been cached.
## If yes, cacheSolve retrieves the inverse.
## If no, cacheSolve calculates the inverse and sends it to the cache.

cacheSolve <- function(x, ...) {

  m <- x$get_inverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$set_inverse(m)
  m

}


