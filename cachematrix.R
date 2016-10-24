## These two functions utilize the lexical scoping properties of R to
## build a sete of 4 functions housed within object makeCacheMatrix
## which can then be used by downstream R code cacheSolve

## the makeCacheMatrix functions stores the square invertible matrix
## passed to it as the variable x. makeCacheMatrix builds a set of 4 functions
## and returns those functions as a list in the parent environment
## which are accessible by the cacheSolve function

makeCacheMatrix <- function(x = matrix()) { ## Intialize x as a matrix
        n <- NULL                       ## Initialize m
        set <- function(y) {            ## 1. Assign the input argument to
                x <<- y                 ## the x object in the parent environment
                n <<- NULL              ## Assign the value of NULL to the n object
        }                               ## in the parent environment
        get <- function() x             ## Get the value of the input argument
        setinverse <- function(inverse) n <<- inverse ## Set the value of n
        getinverse <- function() n         ## Get the value of n
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        ## set the names to be easily accessed using $ operator
}

## cacheSolve checks the object of type makeCacheMatrix passed to it
## for the inverted matrix named as variable x. If the matrix has been
## inverted and stored the cacheSolve returns the cached data, otherwise
## cacheSolve accesses the object makeCacheMatrix and gets the matrix and
## then inverts while passing the inverted matrix to the function
## setinverse housed in the makeCacheMatrix environment

cacheSolve <- function(x, ...) {
        n <- x$getinverse()
        if(!is.null(n)) {
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setinverse(n)
}
