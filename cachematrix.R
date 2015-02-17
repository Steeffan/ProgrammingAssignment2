## The global idea is to compute the inverse of a matrix and to cache it to avoid redondant calculation as long
## as the matrix does not change.
## For this purpose we use the R scoping rules to preserve the state of objects (here a Matrix and its inverse)
## into another object (here a list of functions), what we will call cached.


####################################
####      makeCacheMatrix       ####
####################################
## This function constructs 4 functions (getMat, setMat, getInv and setInv) and returns them in a list
## getMat and getInv return respectively the value of the matrix mat and the inverse matrix inv
## setMat sets the value of the matrix mat to a new value and initializes the inverse matrix inv to NULL
## setInv sets the value of the inverse matrix inv
## When makeCacheMatrix is invoked, it creates these 4 functions in a specific environment (child of the global environment)
## where the variables mat (matrix) and inv (inverse matrix) are stored (or cached) (lexical scoping rule)

makeCacheMatrix <- function(mat = matrix()) {
        
        ## inverse matrix initialized
        inv <- NULL
        
        ## constructs the 4 functions
        getMat <- function() mat

        setMat <- function(newMat) {
                mat <<- newMat
                inv <<- NULL
                message("matrix set to new value and inverse matrix initialized")
        }
        
        getInv <- function() inv
        
        setInv <- function(invMat) inv <<- invMat

        ## return a list with the 4 functions
        list(getMat = getMat, setMat = setMat, getInv = getInv, setInv = setInv)
}


####################################
####        cacheSolve          ####
####################################
## This function calculates (if necessary) and returns the inverse of a matrix
## It looks for the inverse matrix in the specific environment where the elements of x were created
## If calculated, the inverse matrix is returned
## If not calculated, calculates the inverse matrix, caches it and returns it
## The input x is the list of 4 functions returned by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        
        ## get the inverse matrix from the cache
        inv <- x$getInv()
        
        ## if calculated, return it
        if (!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        
        ## if not calculated, get the matrix, solve it, and cache the inverse matrix        
        mat <- x$getMat()
        inv <- solve(mat, ...)
        x$setInv(inv)
        
        ## return the inverse matrix
        message("inverse matrix calculated and cached")
        inv
}
