## makeCacheMatrix provides the functions to work with a matrix and works in combination with
## cacheSolve to provide the inverse of a matrix

makeCacheMatrix <- function(mat = matrix()){
    inverse <- NULL
    set <- function(y){
        mat <<- y
        inverse <<- NULL
  }
    get <- function() mat
    setInverse <- function(inverted) inverse <<- inverted
    getInverse <- function() inverse
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## First checks to see if the inverse of the matrix has been solved. If it has not, it uses
## the solve() function to get the inverse of the matrix

cacheSolve <- function(mat, ...){
    inverse <- mat$getInverse()
    if (!is.null(inverse)){
        message("Getting Cached Data")
        return(inverse)
    }
    data <- mat$get()
    inverse <-solve(data)
    mat$setInverse(inverse)
    inverse
}






