## Put comments here that give an overall description of what your
## functions do

## 1. makeCacheMatrix Function creates the matrix object that chaches 
## the input matrix and its inverse data:

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(solve) m <<- solve
        getmatrix <- function() m
        list(set=set, get=get,
             setmatrix=setmatrix,
             getmatrix=getmatrix)
}

## 2. cacheSolve Function calls function stored in matrix and returned by makeCacheMatrix. 
## If the inverse calculation is already performed, then cacheSolve obtaines the inverse data from the cache. 
## If the input has been changed, the Function calculates the new inverse 
## and updates the inverse in the cache

cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setmatrix(m)
        m
}