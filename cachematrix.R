## x - a square invertible matrix
##returns a list containing functions to the following
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse
## 4. get the inverse
## list is used as the input to cacheSolve
## creates a special matrix object that cache it's inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                # use `<<-` to assign a value to an object in an environment
                # different from the current environment.
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## computes the inverse of the “matrix” returned by makeCacheMatrix().  
##if the inverse has already been calculated and the matrix has not changed, 
##it’ll retrieves the inverse from the cache directly.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        # if the inverse has already been calculated
        if(!is.null(inv)) {
                # get it from the cache and skips the computation
                message("getting cached data")
                return(inv)
        }
        # otherwise, calculates the inverse 
        matdata <- x$get()
        inv <- solve(matdata, ...)
        # sets the value of the inverse in the cache via the setinv function
        x$setinv(inv)
        return(inv)
}
