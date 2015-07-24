## This code is able to cache the inverse of a matrix

## This function below creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y # caches the inputted matrix 
        m <<- NULL # m is set to null if cacheSolve is used (matrix has changed or cache hasn't been stored)
    }
    get <- function() x
    setmatrix <- function(solve) m <<- solve
    getmatrix <- function() m
    list(set = set, get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## This function below computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (with no changes in matrix), then the casheSolve should retrieve the inverse from the cache. 

cacheSolve <- function(x=matrix(), ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix() # m is set to value of inverse of matrix, if it has been calculated
    if(!is.null(m)) { # checks if inverse has already been created
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m #returns inverse of matrix
}
