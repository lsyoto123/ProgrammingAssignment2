## MakeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##             If the inverse has already been calculated(and the matrix has not changed), then the cachesolve should retrieve
##             the inverse from the cache.

## MakeCacheMatrix: This function creates a special "matrix" object that an cche its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y)                         ## to make the set function
        {x <<- y
         m <<- NULL}
        get <- function() x
        setinverse <- function(solve) m <<- solve  ## to calculate inverse
        getinverse <- function() m                 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated(and the matrix has not changed),
## then the cacheslve should rtrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()                        
        if(!is.null(m))                            ## to check whether the inverse has already been calculated or not
         {message("getting cached data")
         return(m)}
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m                                          ## Return a matrix that is the inverse of 'x'
        }
        
