## The makeCacheMatrix function creates a matrix x (defined by the user).
## The place holder variable i stores the inverse of the matrix, which is
## determined by the setinverse function defined within makeCacheMatrix. The
## getinverse function retrieves this value and the output of makeCacheMatrix
## is a list of the outputs of these objects.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cacheSolve function takes the matrix (x) and uses the getinverse function
## defined in makeCacheMatrix to determine and return the variable i (inverse).
## However if i is a null value (is not stored in the cache), then it will be
## calculated in cacheSolve using the solve function and that answer will be
## saved to the cache for future use.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}

