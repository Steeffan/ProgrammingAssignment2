## The global idea is to create two functions to cache a matrix and its inverse matrix, and to build a set 
## of functions (set, get and solve) to manipulate these two matrix.


## makeCacheMatrix constructs 4 functions and returns them in a list
## getMat and getInv return respectively the value of the matrix x and the inverse matrix i
## setMat set the value of the matrix x and initialize the inverse matrix i to NULL
## setInv set the value of the inverse matrix i

## When makeCacheMatrix is invoked, it creates these 4 functions in a specific environment (child of the global environment)
## where the input variable x (matrix) and the local variable i (inverse matrix) are stored (lexical scoping rule)

makeCacheMatrix <- function(x = matrix()) {
        
        ## inverse matrix initialized
        i <- NULL
        
        ## constructs the 4 functions
        getMat <- function() x 

        setMat <- function(mat) {
                x <<- mat
                i <<- NULL
                message("matrix set to new value and inverse matrix initialized")
        }
        
        getInv <- function() i
        
        setInv <- function(inv) i <<- inv

        ## return a list with the 4 functions
        list(getMat = getMat, setMat = setMat, getInv = getInv, setInv = setInv)
}


## cacheSolve calculates (if necessary) and returns the inverse of a matrix
## This function looks for the inverse matrix in the specific environment where the elements of x were created
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
        inv <- solve(mat,...)
        x$setInv(inv)
        
        ## return the inverse matrix
        message("inverse matrix calculated and cached")
        inv
}
