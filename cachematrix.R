## These functions work in pair to solve the inverse of a matrix,
## and to store that inverse in order to reduce computing load on a system

## If an inverse is already stored and then called, it is retrieved from a cache, 
## however, if an inverse is called and not present,
## then that inverse is subsequently calculated and stored


## ~~makeCacheMatrix~~
## Takes an invertable matrix and stores it in cache. 
## Provides functionality to externally set and retrieve the inverse of said matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv<- function(inverse) inv <<- inverse
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv = getinv)
  
}


## ~~cacheSolve~~
## Return a matrix that is the inverse of 'x'
## If the inverse for x exists, function takes that data
## However, if the inverse is not stored in the corresponding makeCacheMatrix, function creates and stores the matrix itself

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached inverse data")
    return(inv)
    
  }
  data <- x$get()
  inv <-solve(data, ...)
  x$setinv(inv)
  inv
}