## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## setting the enviroment of the matrix and the inverse, with also setting parameters where no matrix x.

makeCacheMatrix <- function(x = matrix()) {
        g <- NULL
        set <- function(y){
                x <<- y
                g <<- NULL
                }
        get <- function()x
        setinverse <- function(inverse) g <<- inverse
        getinverse <<- function()g
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## the results from the above makeCachematrix are the call for this one to inverse the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        g <- x$getinverse()
        if(!is.null(g)){
                message ("getting cached")
                return(g)
                }
        dat <- x$get()
        g <- solve(dat, ...)
        x$setinverse(g)
        g
}
