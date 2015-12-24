## This function caches the inverse of a square invertible matrix.

## The first function, makeVector creates a special "matrix", 
## which is really a list containing a function to
##      1. set the value of the square invertible matrix
##      2. get the value of the square invertible matrix
##      3. set the value of the inverse of the matrix
##      4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()){
        m <- NULL
        set <- function(y){
                x <<- y 
                m <<- NULL
        }
        
        get <- function() {
                x
        }
        
        setinverse <- function(inverse) {
                m <<- inverse
        }
        getinverse <- function() {
                m
        }
        
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The second function, CacheSolve calculates the inverse of the 
## special matrix created with the above function. However, it 
## first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
