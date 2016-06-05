## Given x is a matrix, run x through makeCacheMatrix().
## Now, if you desire x's inverse, run x through cacheSolve().
## The first time through cacheSolve, cacheSolve will solve for the inverse and
## store the inverse within makeCacheMatrix's resulting list.
## Every subsequent time you run x through cacheSolve(), the cacheSolve fxn will
## pull the cached inverse from makeCacheMatrix()'s list and will skip the 
## calculation altogether.


## Given x is a matrix, run x through makeCacheMatrix().
## makeCacheMatrix() will create a list designed to communicate with cacheSolve().
## set() sets the value of our matrix.
## get() if no inverse is already cached, get() communicates x to cacheSolve().
## get.inv() communicates x's inverse to cacheSolve if calc already performed.
## set.inv() caches x's inv once cacheSolve calcs the inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    mat <<- NULL
  }
  get <- function() x
  set.inv <- function(inverse) inv <<- inverse
  get.inv <- function() inv
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}

## If cacheSolve has already calc'd x's inverse, it pulls it from makeCacheMatrix()'s
## cache via get.inv().
## If cacheSolve() has not yet calc'd x's inverse, it calcs the inverse and
## stores the resulting matrix in makeCacheMatrix() via set.inv().
cacheSolve <- function(x, ...) {
  inv <- x$get.inv()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set.inv(inv)
  inv
}
