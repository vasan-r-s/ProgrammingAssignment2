## Creating two functions to reduce repeated calculations of inverse of matrix.
## The first function cache's the existing inverse of a matrix (if it exists).The second calculates the inverse of the matrix

## makeCacheMatrix is a function that puts an inverse of a matrix (calculated via Solve function) in to a cache

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
      set <- function(y) {
          x <<- y
          m <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) m<<- inverse
      getinverse <- function() m
      list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)

}


## cacheSolve function calculates the inverse of a matrix using the Solve function in R

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
