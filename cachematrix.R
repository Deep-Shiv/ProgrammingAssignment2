## This is Deepthi Shivram trial solution for caching the inverse of a matrix
## Solution for course#2 Week#3

## Function creates a special "matrix" object that can cache its inverse. following the same steps as the example provided by teachers

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(i) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by mackeCacheMatrix above
## if the inverse has already beeen calculated, and matrix has not changed, it displays message "from cache" and retrieves the inverse from the cache


cacheSolve <- function(x, ...)
{
  m <- x$getInverse()
  if(!is.null(m))
  { message ("cached data print")
    return(m)
  }
  m <- solve(x$get())
  x$setInverse(m)
  m
}

