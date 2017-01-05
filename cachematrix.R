## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInv <-function(inv) i <<- inv
  getInv <- function() i
  list(set = set, get = get, setInv=setInv, getInv=getInv)
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## For this assignment, assume that the matrix supplied is always invertible.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)){
    message("getting cached data")
    return (i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInv(i)
  i
}

# to test, 
# x = matrix(c(2,4,3,1,5,7,9,8,6), nrow=3, ncol=3)
# x looks like below,
#      [,1] [,2] [,3]
# [1,]    2    1    9
# [2,]    4    5    8
# [3,]    3    7    6
# m = makeCacheMatrix(x)
# cacheSolve(m)
# Result should be 
# [,1]       [,2]        [,3]
# [1,] -0.4  0.8769231 -0.56923077
# [2,]  0.0 -0.2307692  0.30769231
# [3,]  0.2 -0.1692308  0.09230769
