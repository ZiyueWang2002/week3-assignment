makeCacheMatrix <- function(a = matrix()) {
    i <- NULL
    Set <- function(y) {
        x <<- b
        i <<- NULL
    }
    Get <- function() a
    set_inverse <- function(inverse) i <<- inverse
    get_inverse <- function() i
    list(Set = Set, Get = Get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

cacheSolve <- function(a, ...) {
    i <- a$get_inverse()
    if (!is.null(i)) {
        return(i)
    }
    Dataset <- a$Get()
    i <- solve(Dataset, ...)
    a$set_inverse(i)
    i
    
}