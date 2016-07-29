##Matrix inversion is usually a costly computation and 
##there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly 
##Following 2 functions that cache the inverse of a matrix.



## makeCacheMatrix return a list with 4 functions:
## get() get the matrix 
## set() assign the matrix
## getinv()  get inverse matrix from cache
## setinv() set inverse matrix to cache
 

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL;
  set <- function(y)
  {
    x <<- y;
    i <<- NULL;
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  
  list(set = set, get = get, setinv = setinv, getinv = getinv)
  
}



##cachesolve computes the computes the inverse of the special "matrix" 
##If the inverse has already been calculated, retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if ( !is.null(i))
    {
      message("getting cached data")
      return(i)
    }
    d <- x$get()
    i <- solve(d, ...)
    x$setinv(i)
    i
}


##Testing
#t1 <- matrix( c(1,2,2,1), 2, 2)
#ct1 <- makeCacheMatrix(t1);
#cacheSolve(ct1)
#cacheSolve(ct1)

