## These functions calculate and caches the inverse of a matrix.  
## This will save time on a costly opperation if it needs to be done more than once.
## The assumption is that the matrix is invertable.

## This function make a cache of it's inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL   
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse 
  getinverse <- function() i
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)

}


## This function takes the matrix and calculate the inverse or retrieve the inverse 
## if the inverse was already calculated and the matrix did not chage since then.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
        message("getting cached data")
        return(i)
  }
  data <-x$get()
  i <- solve(data)
  x$setinverse(i)
}
