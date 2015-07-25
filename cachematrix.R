## Function makeCacheMatrix:
##   Def: This function creates a special "matrix" where to cache a matrix and its inverse
##   Parameters: x must be a square matrix
##   Result: list of 4 functions:
##    - set: initialize  the original matrix and its cached inverse
##    - get: get original matrix
##    - setinverse: cache a matrix
##    - getinverse: get
makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Function cacheSolve:
##   Def: This function computes the inverse of the special "matrix" 
##        returned by makeCacheMatrix above. If the inverse has already been calculated 
##        (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
##   Parameters: x is a special "matrix" created by the function makeCacheMatrix
##   Result: Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
    
    #Tests if inverse has already been caculated
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    
    inverse <- solve(data, ...)  #Calculates the matrix inverse
    x$setinverse(inverse)  #Stores the result in the cache object
    inverse

}

