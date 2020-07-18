## Calculating inverse of a matrix and storing it as cache. If the same matrix is inputted, then the
##cached result will be the output.
## To generate the special matrix list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() 
  {x}
  
  setinverse <- function(inverse) 
  {inv <<- inverse}
  
  getinverse <- function()
    {inv}
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The output i.e. inverse of matrix inputted

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  
  x$setinverse(inv)
  
  inv
}
