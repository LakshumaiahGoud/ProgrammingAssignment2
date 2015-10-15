## Put comments here that give an overall description of what your
## functions do
##cachematrix file contains two functions 
##1.makeCacheMatrix 
##2.cacheSolve
##used to return inverse of matrix from cached variable
##this function gives info how to cache data and return form cached

##set matrix : it initialize matrix and assigned
##get matrix : returns matrix
##setinverse : assign inversed matrix
##getinverse : returns inversed matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


##it set and get the matrix and inversed using solve, it will return the invers matrix and assign to the variable <<- 
##it will return matrix from cached variable 

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
