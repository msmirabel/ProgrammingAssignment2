
## This function creates a special "matrix" object that can cache its inverse. It contains a function
## to set or get the matrix or set or get inverse of a matrix.  

makeCacheMatrix <- function(mx = matrix()){
  i <- NULL
  set <- function(mx2)
  {
    mx <<- mx2
    i <<- NULL
  }
  get <- function() mx
  
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## this function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(mx, ...) {
  i <- mx$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- mx$get()
  i <- solve(data, ...)
  mx$setinverse(i)
  i 
}
