## These 2 functions demonstrate how to nest functions in R, to create
## an object, an (assumed square) invertible matrix to cache/store 
## its inverse.This should avoid repetitious computation.

## The makeCacheMatrix function returns a list containing the function 
## to set the value of the matrix, get the value of the matrix, 
## set the value of the inverse, get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set<- function(y) {
                x<<- y
                s<<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

## Input to the cacheSolve function is from the function list 
## which has been created,defined and returned by makeCacheMatrix 
## function above.

## The cacheSolve function gets and returns the cached inverse of 
## the matrix x.
## Otherwise, if the matrix has been changed,it recomputes and 
## returns a matrix that is the inverse of x

cacheSolve <- function(x, ...) {
        s <- x$getsolve()
        if(!is.null(s)) { 
                message("getting cached inverse")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        return(s)
}
