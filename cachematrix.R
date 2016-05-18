## Week 3 programming assignment - Coursera course "R programming" JP Bogers 2016 

## The first function (makeCacheMatrix) creates a special matrix

## This matrix (besides holding the values) also holds the functions to display the matrix (get),
## to set the matrix in the cache (setinvers) and to get the inverse from the cache (getinvers).

## The cache is stored in the "supervariable" m. The scope of m is in the function makeCacheMatrix,
## the value of this "supervariable" is thus available/visible while in makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL                            ## Initialize cache and set to NULL
        set <- function(y) {                     ## Store the original matrix
                x <<- y
                cache <<- NULL
        }
        get <- function() x                      ## Get the original matrix
        setinvers <- function(solve) cache <<- solve    ## Store the calculated inverse in the cache
        getinvers <- function() cache                   ## Get the cache back
        list(set = set, get = get,
             setinvers = setinvers,
             getinvers = getinvers)
}


## The second function cacheSolve will first check if the inverse is available (by asking getinvers) and 
## potentially calculates the inverse of the matrix. 
## If the cache is NOT empty, the value is returned from makeCacheMatrix to cacheSolve and displayed
## If the cache is empty, the inverse is calculated in cacheSolve and handed back to makeCacheMatrix 
## before being displayed


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinvers()    ## Is the inverse available? (does x$getinvers() return NOT NULL)
        if(!is.null(m)) {     ## The inverse is available!
                message("getting cached data")
                return(m)
        }
        data <- x$get()       ## The cache is empty, get the original matrix
        m <- solve(data, ...) ## Calculate the inverse
        x$setinvers(m)        ## Store the inverse in the cache
        m                     ## Display the inverse
}
