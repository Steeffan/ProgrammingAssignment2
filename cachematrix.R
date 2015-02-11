## The global idea is to create a dedicated environment using the lexical scopus rules to cache ("hide") a matrix
## and its calculated inverse matrix


## makeCacheMatrix returns a list of 4 functions (getMat, setMat, getInv, setInv)
## when the function is called, due to the lexical scopus rules, the variables x (matrix) and i (inverse) are stored
## in the environment where the functions are created. This environment is a child of the global environment and it is referenced in
## the list returned when makeCacheMatrix is invoked

makeCacheMatrix <- function(x = matrix()) {

        ## inverse matrix initialized
        i <- NULL
        
        ## return the matrix
        getMat <- function() x 
        
        ## set a new value in the matrix and initialize the inverse matrix
        setMat <- function(mat) {
                x <<- mat
                i <<- NULL
                message("matrix set to new value and inverse matrix initialized")
        }
        
        ## return the inverse matrix
        getInv <- function() i
        
        ## set the value of the inverse matrix
        setInv <- function(inv) i <<- inv
        
        ## return a list with the 4 functions
        list(getMat = getMat, setMat = setMat, getInv = getInv, setInv = setInv)
}


## This function looks for the inverse matrix of x in the specific environment and, if not cached, 
## calculates the inverse matrix and save it to the environment

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Look for the inverse matrix in the cache
        inv <- x$getInv()
        
        ## if cached, return it
        if (!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }
        
        ## if not cached, solve it, cache it and return it
        mat <- x$getMat()
        ## solve the matrix
        inv <- solve(mat)
        ## cache the inverse matrix
        x$setInv(inv)
        message("inverse matrix calculated and cached")
        inv
}
