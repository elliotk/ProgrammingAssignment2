## Author.....: Elliot Kleiman
## Date.......: Sat Apr 26 19:26:39 EDT 2014
## File.......: cachematrix.R
## Description: Caching the inverse of a matrix.
##              Functions `makeCacheMatrix' and `cacheSolve'
##              are used to demonstrate variable caching in R.
## Usage......: source(cachematrix.R)
##              

## 1. makeCacheMatrix()
## Function returns a list of getter and setter functions to:
##
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
## usage: mat1 <- makeCacheMatrix()
##        mat1$get()
##        mat1$set(matrix(c(2, 3, 1, 2), 2))
##        mat1$getinverse()
##        mat1$setinverse(solve(mat1$get()))
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y 
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    ## Return list
    list(set=set, get=get,
         setinverse=setinverse,
         getinverse=getinverse)
}

## 2. cacheSolve()
## Function to return a matrix that is the inverse of 'x'
##
## If the inverse was previously computed, it will retrieve
## the inverse from the cache. Otherwise, it will compute
## the inverse and set the value in the cache using the
## function `setinverse'.
## usage:  cacheSolve(mat1)
cacheSolve <- function(x, ...) {
    ## Fetch inverse
    i <- x$getinverse()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    ## Fetch data
    data <- x$get()
    
    ## Is matrix square?
    if (nrow(data) != ncol(data)) {
        ## Dimension message xstring
        dim.msg <- paste("(", nrow(data), " X ",
                         ncol(data), ") ",
                         "must be square", sep="")
        message(dim.msg)
    } else {
        i <- solve(data, ...)
        x$setinverse(i)
    }
    ## Return inverse
    i
}
