## Put comments here that give an overall description of what your
## functions do

## creates a special object to store a matrix value,
## as well as its inverse,
## and provides getters/setters for the matrix value and inverse value
makeCacheMatrix <- function(X = matrix()) {
    i <- NULL
    
    # define setter for matrix value
    set <- function(matrixValue) {
        X <<- matrixValue
        i <<- NULL # overwrite inverse
        X
    }
    
    # define getter for matrix
    get <- function() X
    
    # define setter for inverse value
    setInverse <- function(inverse){
            i <<- inverse
            i
    }
    
    # define getter for inverse vaue
    getInverse <- function() i
    
    list(set=set, 
         get=get, 
         setInverse=setInverse, 
         getInverse=getInverse)
}


## checks if the inverse of a matrix is cached, or else
## calcucaltes and stores a new inverse
## accepts CacheMatrix as input
cacheSolve <- function(X, ...) {
    if(is.null(X)){
        stop("input matrix cannot be null")
    }
    if(typeof(X) != "list" || 
                (is.null(X$getInverse) || typeof(X$getInverse) != "closure") || 
                (is.null(X$setInverse) || typeof(X$setInverse) != "closure") ||
                (is.null(X$get) || typeof(X$get) != "closure")){
        stop("input must be a CacheMatrix list - see makeCacheMatrix()")
    }
    
    i <- X$getInverse()
    if(!is.null(i)) {
        message("using cached inverse.")
        return(i)
    }
    
    X$setInverse(solve(X$get(), ...))
}
