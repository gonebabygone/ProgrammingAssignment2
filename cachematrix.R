## Coursera R-Programming
## https://class.coursera.org/rprog-016
##
## Programming Assignment 2 - Lexical Scoping
##
## Matrix inversion is a costly computation.  The functions in this file
## allow you to compute the inverse of a matrix and cache the result
## so that subsequent queries are fast.
## 
## These functions use a CacheMatrix object.  A CacheMatrix object is
## simply a wrapper around a matrix that has some memory allocated for
## caching its inverse.
##
## Suppose you have a matrix M.  The way to use these functions is:
##   1. Create a CacheMatrix wrapping M by calling makeCacheMatrix(M)
##   2. Any time you want to access the inverse of M, access it by
##      calling cacheSolve on the CacheMatrix object.  The first time
##      cacheSolve is called, it will compute the inverse of M and
##      save it.  Subsequent calls will simply return the cached value,
##      it will not repeat the computation.
##   3. If you change M, update the CacheMatrix object with the
##      'set' method.  This will invalidate the cache so the inverse
##      will be recomputed the next time cacheSolve is called.
## 
## You should not access the members of the CacheMatrix object except
## for calling the set method when you want to change the wrapped matrix.
## You should only access the cached inverse via the cacheSolve function.
## 
## EXAMPLE:
##
## > M <- rnorm(9)
## > dim(M) <- c(3,3)
## > M
##            [,1]       [,2]       [,3]
## [1,] -0.7776530 -1.6002826 -1.3689541
## [2,] -0.2796738  0.8117453  0.1077206
## [3,]  2.1652982  2.0903130  1.4860779
## > solve(M)
##            [,1]       [,2]       [,3]
## [1,]  0.6982701 -0.3440284  0.6681741
## [2,]  0.4617888  1.2871162  0.3320947
## [3,] -1.6669692 -1.3091855 -0.7677781
## > cm <- makeCacheMatrix(M)
## > cacheSolve(cm)
##            [,1]       [,2]       [,3]
## [1,]  0.6982701 -0.3440284  0.6681741
## [2,]  0.4617888  1.2871162  0.3320947
## [3,] -1.6669692 -1.3091855 -0.7677781
## > cacheSolve(cm)
## getting cached data
##            [,1]       [,2]       [,3]
## [1,]  0.6982701 -0.3440284  0.6681741
## [2,]  0.4617888  1.2871162  0.3320947
## [3,] -1.6669692 -1.3091855 -0.7677781
##

## makeCacheMatrix
##
## Creates a CacheMatrix object.  
##
## Parameters:
##   x (optional) - The matrix whose inverse you want to compute.
##                  If not provided, defaults to an empty matrix
##
## Returns:
##   CacheMatrix object

makeCacheMatrix <- function(x = matrix()) {
  # This code was copied almost directly from
  # the makeVector example.  The only things changed
  # were the variable names.
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve
##
## Returns the inverse of a cached matrix.
## The first time this is called on a CacheMatrix, it will
## compute the inverse and store it in the CacheMatrix.
## Subsequent calls will simply return the cached value.
##
## This function assumes that the given matrix is square
## and invertible, otherwise it will throw an error.
##
## BEWARE WHEN PASSING EXTRA ARGUMENTS!
##
## Following the example in the assignment instructions,
## this function takes extra arguments and passes them to
## the call to solve. However, the cache does not take these
## extra arguments into consideration.  For example, if you
## call this function twice on the same CacheMatrix, but
## with different extra arguments, the first result will be
## cached, and the second call will simply return the result
## from the first call, even though that was computed with
## different arguments to solve!
##
## Parameters:
##   x - A CacheMatrix object.
##   Extra parameters will be passed as arguments to solve.
##
## Returns:
##   A matrix which is the inverse of the matrix
##   wrapped by the CacheMatrix object.

cacheSolve <- function(x, ...) {

  # If the cache exists, return it
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }

  # Otherwise, compute the inverse ...
  data <- x$get()
  inv <- solve(data, ...)
  # ... and save it in the cache
  x$setinv(inv)
  inv
}
