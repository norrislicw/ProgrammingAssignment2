makeCacheMatrix <- function(x = matrix()) {
        ## This function creates a special "matrix" object
        ## that can cache its inverse.
        
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        
        ## Calculate the inverse based on the defined function
        ## which is the function solve() based on cacheSolve()
        setinverse <- function(inverse) m <<- inverse
        
        ## Return the inverse, if cacheSolve() is called (else, NULL will be returned)
        getinverse <- function() m
        
        ## Return the string (info) of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
        ## This function computes the inverse of the special "matrix"
        ## returned by the function makeCacheMatrix().
        ## If the inverse has already been calculated (and the matrix has not changed),
        ## then this function should retrieve the inverse from the cache.
        
        m <- x$getinverse()

        ## If m is not NULL, then return the value (inverse of matrix) calculated before
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Else, calculate the inverse of the matrix using the function solve()
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}