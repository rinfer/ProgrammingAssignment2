## Within this assignement two functions are to be programmed.
## The first creates a matrix x which stores its inverse in the cache.
## The seconde calculates the inverse of the matrix x using 
## cached values.


## With the function makeCacheMatrix a list of other functions is created. 
## These functions buildI  a matrix and store the matrix' inverse
## in the cache.
## The "function list" consists of a setMatrix (sets value of matrix), 
## getMatrix (gets its value), cacheInv (sets the inverse value into cache)
## and getInv (gets inverse value from cache) function. 

makeCacheMatrix <- function(x = matrix()) {  #x = numeric/matrix?
  cache <- NULL
  setMatrix <- function(newVal) {
    x <<- newVal
    cache <<- NULL
  }
  getMatrix <- function() {
    x
  }
  
  cacheInv <- function(inverse) {
    cache <<- inverse
  }
  getInv <- function() {
    cache
  }
  list(setMatrix = setMatrix, getMatrix = getMatrix, 
       cacheInv = cacheInv, getInv = getInv)
}

## The function cacheSolve calculates the inverse of the matrix created by
## makeCacheMatrix. Inverse values calculated and stored in the 
## cache are recycled from there.

cacheSolve <- function(x, ...) {
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getMatrix()
  inv <- solve(data, ...)
  x$cacheInv(inv)
  inv
}

## In order to test the functions you could create p.ex. one of the
## two matrices
a <- makeCacheMatrix(matrix(c(1,2,12,13), nrow = 2, ncol = 2))
b <- makeCacheMatrix(matrix(c(1, 2, 12, 13, 22, 23, 32, 33, 42), nrow = 3, ncol = 3))

## Then try this:
summary(a)
a$getMatrix()
cacheSolve(a) # to run twice