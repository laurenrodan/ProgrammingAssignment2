## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix assumes that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
    inv_x <- NULL
    set <- function(y) {
        x <<- y
        inv_x <<- NULL
    }
    get <- function() x
    setinverse<- function(inverse) inv_x <<-inverse
    getinverse <- function() inv_x
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve finds the inverse of the special matrix created by makeCacheMatrix.
## cacheSolve will get the inverse from the cache if the matrix has been calculated but not changed. 

cacheSolve <- function(x, ...) {
    inv_x <- x$getinverse()
    if (!is.null(inv_x)) {
        message("getting cached data")
        return(inv_x)
    } else {
        inv_x <- solve(x$get())
        x$setinverse(inv_x)
        ## Return a matrix that is the inverse of 'x'
        return(inv_x)
    }
}

        
}
