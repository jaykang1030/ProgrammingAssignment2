## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly


# makeCacheMatrix, creates a special "matrix", which is really a 
# list containing a function to:
# 1 set the value of the matrix
# 2 get the value of the matrix
# 3 set the value of the inverse of the matrix using the solve() function
# 4 get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve, calcuates inverse of a matrix
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
}

## sample run
x = matrix(rnorm(9), ncol = 3, nrow = 3) # randomly generate 3 by 3 matrix
m = makeCacheMatrix(x) ## create "cached" matrix object
m$get() ## print "cached" matrix object 
cacheSolve(m)  ## calculates inverse of "cached' matrix object
m$getinverse() ## print inverse result
