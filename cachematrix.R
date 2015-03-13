## The functions below are part of R Programming Assignment 2
##
## The overall goal is to leverage R Scoping to cache the inverse
## of a Matrix to save computation time instead of repeating
## this multiple times. We will be leveraging the <<- operator
## in order to assign a value to an object. Two functions below
## is the required code for the class.


## This function will create a special matrix, which is a list
## containing a function to set/get the value of the matrix
## AND set/get the value of it's inverse. This list will be 
## returned.
##
## I use the 'solve' to calculate the inverse of the input matrix.
## It is assumed the input matrix is valid to calculate the inverse
## of (aka, it needs to be equal rows and columns).
makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinv <- function(solve) m <<- solve
       getinv <- function() m
       list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function will calculate the inverse of the Matrix by
## first checking to see if the inverse was already calculated
## and stored.  If so, it will simply return the stored value.
## Otherwise, it will calculate the inverse of the stored
## matrix and store it prior to return the inverse value.
##
## The input into this function is the returned list from
## the function makeCacheMatrix above.
cacheSolve <- function(x = matrix(), ...) {
       ## If stored already, then simply return the inverse
       m <- x$getinv()
       if(!is.null(m)) {
              message("getting cached data")
              return(m)
       }
       
       ## Otherwise, retreive the matrix, and calculate the
       ## inverse, store it and then return the inverse.
       data <- x$get()
       m <- solve(data, ...)
       x$setinv(m)
       return(m)
}