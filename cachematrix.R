## These 2 functions are used to create a special object that stores a matrix
## and cache its inverse. It avoids to recompute the inversion in case of 
## multiple usage.

#
# USAGE EXAMPLE
#
# m: the matrix in input
# m <- matrix(2:5, nrow = 2, ncol = 2)
#
# a: function used to cache m
# a <- makeCacheMatrix(m)
#
# calculate or retrieve the inverse of m
#
# cacheSolve(a)
#      [,1] [,2]
# [1,] -2.5    2
# [2,]  1.5   -1
#
# if we relaunch the previous function, we see that the inverse of the matrix is
# retrieved from the cache
#
# cacheSolve(a)
# getting cached data
# [,1] [,2]
# [1,] -2.5    2
# [2,]  1.5   -1



## Create a "special' matrix which is a list of functions 
## to set and get the value of the matrix, and to set and get 
## the value of the INVERSE of the matrix

makeCacheMatrix <- function(x = matrix()) {
    # i: inverse of the square matrix 
    # initialization of i
    i <- NULL
    
    # set: replaces the matrix stored in the main function
    set <- function(y) {
        x <<- y
        # in this case, we also have to recompute i
        i <<- NULL
    }
    
    # get: simply returns the matrix stored in the main function
    get <- function() x
    
    # setinverse: stores the value of i into the main function
    setinverse <- function(solve) i <<- solve
    
    # getinverse: returns the value of i stored in the main function
    getinverse <- function() i
    
    # we store these 4 functions into the main one (aka makeCacheMatrix)
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x'
## If the inverse had already been stored in the cache, the function returns the
## value witout recalculating it.
## It there's no matrix stored in the cache, the function calculates the inverse
## of the matrix, stores it and finally returns it

cacheSolve <- function(x, ...) {
    # we check if the inverse of the matrix is already stored
    i <- x$getinverse()
    if(!is.null(i)) {
        # if the inverse of the matrix is already stored, 
        # we return it right away
        message("getting cached data")
        return(i)
    }
    # else, we retrieve the matrix stored in makeCacheMatrix
    data <- x$get()
    # we calculate its inverse and store the result into i
    i <- solve(data, ...)
    # we store this inverse in the object generated with makeCacheMatrix
    x$setinverse(i)
    i
}