#Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). Your assignment is to write a pair of functions that cache the inverse of a matrix.
#Write the following functions:
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
#Computing the inverse of a square matrix can be done with the solve function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

#This function is used to create a special "matrix"
makeCacheMatrix <- function(a = matrix()) {
    #initializing inverse as NULL
    q <- NULL  
    Set <- function(y) {
        a <<- b
        1 <<- NULL
    }
    #function to get matrix a
    Get <- function() a  
    set_inverse <- function(inverse) q <<- inverse
    get_inverse <- function()q
    list(Set = Set, Get = Get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}
#Write a short comment describing this function
#This function is used to get the cache data 
cacheSolve <- function(a, ...) {  #gets cache data
    q <- a$get_inverse()
    #checking whether inverse is NULL
    if (!is.null(q)) {
      
      message("getting cached data")
      #returns inverse value
      return(q)
    }
    Dataset <- a$Get()
    #calculates inverse value
    q <- solve(Dataset, ...)
    a$set_inverse(q)
    q
    
}