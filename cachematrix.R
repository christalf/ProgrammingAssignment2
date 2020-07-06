## Chaching the Inverse of a Matrix.

## The two functions defined below are used to create a special
## object that stores an invertible matrix and caches its
## inverse matrix.


## This first function, `makeCacheMatrix`, creates a special
## "invertible matrix", which is really a list containing a
## function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function, `cacheSolve`, calculates the inverse
## of the special "matrix" returned by the above funcion.
## However, if the inverse has already been calculated (and the
## matrix has not changed), then the function retrieves the
## inverse from the cache and skips the computation.
## Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache.
## Returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
