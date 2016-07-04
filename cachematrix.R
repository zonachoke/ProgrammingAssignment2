# To save computation time re: inverse of matrix

# The following creates several functions--
#   to set and to get the value of a matrix, and
#   to set and to get the value of the inverse of the matrix.
makeCacheMatrix <- function(mx = numeric()) {
    sv <- NULL
    setm <- function(zy) {
        mx <<- zy
        sv <<- NULL
    }
    getm <- function() mx
    setsolve <- function(solve) sv <<- solve
    getsolve <- function() sv
    list(setm = setm, getm = getm,
         setsolve = setsolve,
         getsolve = getsolve)
}

# The following gets the inverse of the matrix, if available.
#   If not, it calculates the inverse for immediate use & stores it for later.

cacheSolve <- function(x, ...) {
    sv <- x$getsolve()
    if(!is.null(sv)) {
        message("getting cached data")
        return(sv)
    }
    data <- x$getm()
    sv <- solve(data)
    x$setsolve(sv)
    sv
}