## Caching the Inverse of a Matrix

## create a special "matrix" (which is really a list) to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(i) inv <<- i
        getinverse <- function() inv
        
        list(set = set, get = get, 
             setinverse = setinverse, getinverse = getinverse)
}


## retrieve the inverse from the cache or create new inverse
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting chached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
