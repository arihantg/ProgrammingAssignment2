##Below are two functions that are used to create a special object that stores a numeric matrix and cache's its Inverse

## The first function, makecachematrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
              m <- NULL
              set <- function(y) {
                x <<- y
                m <<- NULL
              }
              get <- function() x
              setInverse <- function(y = matrix()) m <<- y
              getInverse <- function() m
              list(set = set, get = get,
                   setInverse = setInverse,
                   getInverse = getInverse)


}


## The following function calculates the Inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the Inverse has already been calculated. If so, it gets the Inverse from the cache and skips the computation. Otherwise, it calculates the Inverse of the data and sets the value of the Inverse in the cache via the setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
