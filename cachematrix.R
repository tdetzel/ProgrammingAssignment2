## The two functions below can speed up code requiring matrix inversions by moving
## computations to cache memory, where they can be retrieved instead of recalculated.   

## Function makeCacheMatrix makes a special matrix that can cache its inverse.
## The input must be a square matrix for the function to work. 
## The function returns a list that does the following:
    ## 1 -- set the value of the matrix
    ## 2 -- get the value of the matrix
    ## 3 -- set the value of the inverse
    ## 4 -- get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
   m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Function cacheSolve takes a list created
## by makeCacheMatrix and returns inverse of the original
## matrix passed to makeCacheMatrix.

## If the inverse is already calculated (and the matrix is 
## not changed), it is pulled from cache memory; otherwise 
## the inverse is computed using the R solve() function. 

cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
