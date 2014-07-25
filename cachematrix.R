# Whit Walters
# Coursera R Programming - rprog-005
# July 25, 2014
# makeCacheMatrix: return a list of functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse
# 4. Get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    # inv will store the cached inverse matrix
    inv <- NULL

    # 1 Set the value of matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # 2 Get value of matrix
    get <- function() x

    # 3 Set value for the inverse
    setinv <- function(inverse) inv <<- inverse
    # 4 Get value for the inverse
    getinv <- function() inv

    # Return the list of functions for modifying matrix
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated then return the inverse.
cacheSolve <- function(x, ...) {
    inv <- x$getinv()

    # If the inverse is already calculated, then return it
    if (!is.null(inv)) {
        message("getting the cached data")
        return(inv)
    }

    # If the inverse is not yet calculated, then calculate it
    data <- x$get()
    inv <- solve(data, ...)

    # Cache the inverse
    x$setinv(inv)

    # Return the inverse
    inv
}
