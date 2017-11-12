## R Assignment by demima999

## Function to create a list that store the matrix and cache its inverse
## The fuction will 1) set the value of the matrix; 2) get the value of the matrix;
## 3) set the value of the inverse; 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function to calculate the inverse of the special matrix created in the makeCacheMatrix function
## It will first examine if the inverse has been made before; if yes, just return the cache data
## If no, it will make the calculation and store the value in cache data
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
        
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}