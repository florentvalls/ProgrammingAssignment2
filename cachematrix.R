## This function Creates a special "matrix" that can cache its 
## inverse. we first set the matrix value and make sure the variable
## s is set to "nothing" in its environment.
## Then we get the default special "matrix" and we assign the
## operator solve (again in its environment) to obtain its inverse.
## We retrieve the matrix.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x        
        setinv <- function(solve) s <<- solve
        getinv <- function() s
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## This function retrieve the inverse of the function makecacheMatrix.
## We pass a condition that implies that if the matrix have already
## been given and have not changed in any way, that its inverse is 
## retrieved from the cache, if not it calculates its inverse.

cacheSolve <- function(x, ...) {
        s <- x$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        mat <- x$get()
        s <- solve(mat, ...)
        x$setinv(s)
        s
}