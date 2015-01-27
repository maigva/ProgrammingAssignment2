## A pair of functions that cache the inverse of a matrix.

## source("cachematrix.R")

## Create a matrix x.
## x = rbind(c(1, -1/2), c(-1/2, 1))

##      [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0

## Create the special matrix object that can cache its inverse.
## cx <- makeCacheMatrix(x)

## Return the special matrix.
## cx$get()

##      [,1] [,2]
## [1,]  1.0 -0.5
## [2,] -0.5  1.0

## Calculate and return the inverse matrix.
## cacheSolve(cx)

##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333

## Return the inverse matrix from the cache.
## cacheSolve(cx)

## The inverse has already been calculated.
## Return the inverse matrix from the cache.

##           [,1]      [,2]
## [1,] 1.3333333 0.6666667
## [2,] 0.6666667 1.3333333

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    # The cached inverse matrix.
    inv <- NULL
    # Setter for the matrix.
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # Getter for the matrix.
    get <- function() x
    # Setter for the inverse matrix.
    setinv <- function(inverse) inv <<- inverse
    # Getter for the inverse matrix.
    getinv <- function() inv
    # Returns the matrix with the new defined functions.
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    # The inverse matrix is already been calculated.
    if (!is.null(inv)) {
        message("The inverse has already been calculated")
        message("Return the inverse matrix from the cache")
        return(inv)
    }
    # The inverse matrix has not been calculated yet.
    data <- x$get()
    inv <- solve(data, ...)
    # Cache the inverse matrix.
    x$setinv(inv)
    ## Return a matrix that is the inverse of 'x'.
    inv
}
