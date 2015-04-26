# Description of the functions in makeCacheMatrix
#   which creates a matrix
# The set function allows you to change the value
# The get function returns the value
# The setinverse function stores the value
# The getinverse function returns the value

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# Description of the cacheSolve function
#   which returns the inverse of a matrix
# Calls the function getinverse 
# Checks to see if the value is already in cache
# If the value is in the cache return the value
# If the value is not in cache call the solve function
# and store the value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
