## These functions cache the inverse of a matrix and avoid computing it avoidly

## This function creates a special "Matrix", which is really a list containing a function to

## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                                       # store the value of inverse matrix
        set <- function(y) {                            # set the value of the matrix
                x <<- y
                i <<- NULL
        }
        get <- function() x                             # get the value of the matrix
        setinverse <- function(inverse) i <<- inverse   # set the value of the inverse
        getinverse <- function() i                      # get the value of the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the inverse 
## in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        matrix <- x$get()
        i <- solve(x)
        x$setinverse(i)
        i
}
