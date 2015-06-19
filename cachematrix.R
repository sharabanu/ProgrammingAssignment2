
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y)
  {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned
##  by makeCacheMatrix above. If the inverse has already been calculated 
##  (and the matrix has not changed), then the cachesolve should retrieve 
##   the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
## Sample Output
## > x<-matrix(1:4,2)
##> x
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> b = makeCacheMatrix(x)
##> b$get()
##[,1] [,2]
##[1,]    1    3
##[2,]    2    4
##> cacheSolve(b)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
##> 
