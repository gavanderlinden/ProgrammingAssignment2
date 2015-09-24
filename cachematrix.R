## General note:
## I personally think the cacheSolve function is superfluous
## Having all code in one object is a cleaner solution
## However I try to stick as close to the assignment as possible
## To make grading easier

## creates an object that implements an interface for a matrix
## object caches an inversed matrix as long as matrix does not change 
makeCacheMatrix <- function(matrix_ = matrix()) {
    inversed_ <- NULL
    
    ## delete cached inversed matrix if matrix_ is changed
    set <- function(x) {
        if (!identical(matrix_, x)) {
            matrix_ <<- x
            inversed_ <<- NULL
        }
    }

    get <- function() matrix_
    set_inversed <- function(x) {
        message("caching inversed matrix")
        inversed_ <<- x
    }
    get_inversed <- function() inversed_
    list(
        set = set,
        get = get,
        set_inversed = set_inversed,
        get_inversed = get_inversed
    )
}

# function returns a matrix that is the inverse of a CacheMatrix
# first create CacheMatrix with makeCacheMatrix function
cacheSolve <- function(x, ...) {
    
    ## if get_inversed is null, calculate inversed matrix
    ## and store to cache
    inversed_matrix <- x$get_inversed()
    if (is.null(inversed_matrix)) {
        inversed_matrix <- solve(x$get())
        x$set_inversed(inversed_matrix)
    }
    inversed_matrix
}