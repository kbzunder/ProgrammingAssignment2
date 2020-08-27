rm(list = ls())
#making list which stores the inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setsolve <- function(solve) m<<-solve 
  getsolve <- function() m
  list(set=set, get = get, setsolve = setsolve, getsolve=getsolve)
}
## making a function, wnich takes argument, returned by makeCacheMatrix and 
## and retrieves the previousely inversed matrix from cache

cacheSolve <- function(x, ...) {
  m <- x$getsolve() # inversed matrix, if already done
  if (!is.null(m)){ # checking if there exists inversed matrix in cache
    print("getting cached data")
    return(m)
  }
  data <- x$get() # if not previously done, 
  m <- solve(data,...) #inversing the matrix
  x$setsolve(m)   # assigning inversed matrix to m
  m            #returning m
}

m1 <- matrix(c(1/2,-1/4,-1,3/4), nrow = 2, ncol =2)
a <- makeCacheMatrix(m1)           
cacheSolve(a)
cacheSolve(a)
