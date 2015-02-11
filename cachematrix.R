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
        
        ## return the matrix (function)
        getMat <- function() x 
        
        ## set a new value in the matrix and initialize the inverse matrix (function)
        setMat <- function(mat) {
                x <<- mat
                i <<- NULL
                message("matrix set to new value and inverse matrix initialized")
        }
        
        ## return the inverse matrix (function)
        getInv <- function() i
        
        ## set the value of the inverse matrix (function)
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
        
        ## get the inverse matrix in the cache
        inv <- x$getInv()
        
        ## if calculated, return it
        if (!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        
        ## if not calculated, solve it, cache it and return it
        
        ## get the matrix
        mat <- x$getMat()
        
        ## solve the matrix
        inv <- solve(mat)
        
        ## cache the inverse matrix
        x$setInv(inv)
        
        message("inverse matrix calculated and cached")
        
        ## return the inverse matrix
        inv
}
