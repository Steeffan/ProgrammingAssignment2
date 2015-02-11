## The global idea is to calculates the inverse of a Matrix and to cache the result to spare further calculation
## Another idea is, by caching a Matrix and its inverse Matrix in a single object, calling and naming objects is easier
## as a

## makeCacheMatrix creates a list with 4 methods (functions)

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        getMat <- function() x
        setMat <- function(mat) {
                x <<- mat
                i <<- NULL
        }
        getInv <- function() i
        setInv <- function(inv) i <<- inv
        list(getMat = getMat, setMat = setMat, getInv = getInv, setInv = setInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        mat <- x$getMat()
        inv <- solve(mat)
        x$setInv(inv)
        inv
}
