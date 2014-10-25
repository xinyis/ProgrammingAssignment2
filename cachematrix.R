##Programming Assignment 2: Lexical Scoping
#Creator: xinyi.sun
#Created at: 2014.10.25

#makeCacheMatrix function
#creates a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) inv <<- inverse
    get_inverse <- function() inv
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}


#cacheSolve function
#computes the inverse of the special matrix returned by makeCacheMatrix 
#If the inverse has already been calculated (and the matrix has not changed)
#then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$get_inverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$set_inverse(inverse)
    inverse
}
