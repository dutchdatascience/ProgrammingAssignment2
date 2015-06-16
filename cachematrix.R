# Assignment: Caching the Inverse of a Matrix

# Matrix inversion is usually a costly computation and there may be
# some benefit to caching the inverse of a matrix rather than compute
# it repeatedly (there are also alternatives to matrix inversion that
# we will not discuss here). Your assignment is to write a pair of
# functions that cache the inverse of a matrix.


# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# - get the value of the vector
# - set the value of the inverse
# - get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  i           <- NULL
  get         <- function() x
  setinverse  <- function(inverse) i <<- inverse
  getinverse  <- function() i
  list( get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


# The following function calculates the inverse of the special "matrix"
# created with the above function. However, it first checks to see if the
# inverse has already been calculated. If so, it gets the inverse from
# the cache and skips the computation. Otherwise, it calculates the inverse
# of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  i    <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i    <- solve(data)
  x$setinverse(i)
  i
}

# > # Examples run:
# > x <- matrix( c(2, 3, 2, 2), byrow = TRUE, 2, 2)
# > i <- makeCacheMatrix(x)
# > i$get()
#       [,1] [,2]
# [1,]    2    3
# [2,]    2    2
# > 
# > # first run no cache
# > cacheSolve(i)
#     [,1] [,2]
# [1,] -1  1.5
# [2,]  1 -1.0
# > 
# > # second run from cache
# > cacheSolve(i)
# getting cached data
#     [,1] [,2]
# [1,] -1  1.5
# [2,]  1 -1.0
# > 
