## The functions makeCacheMatrix and cacheSolve compute the inverse of a "matrix"
## unless the inverse has already been calculated (and the matrix has not changed).
## Then the inverse is taken from the cache.


## makeCacheMatrix: This function creates a special "matrix", which is actually a list containing functions to
## 1) set the values of the matrix
## 2) get the values of the matrix
## 3) set the values of the inverse
## 4) set the values of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve: The function calculates the inverse of a special "matrix" created with the
## function above. It first checks if the inverse has already been calculated. If so, it gets
## the inverse from the cache. Otherwise, it calculates the inverse of the data and sets the 
## values of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}