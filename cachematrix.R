## Put comments here that give an overall description of what your
## functions do

###OVERALL DESCRIPTION

# The purpose of this assignment is to find a way to set a kind of cache
# in my objects, so to obtain (complicated to evaluated) values
# in no time, rather than evaluating these value every time.
# Of course this is a more consuming memory solution.

###

## Write a short comment describing this function

# This is like a C++ class.
# But we are in R and we do this with a function that
# take a matrix and give a list in which are defined
# functions set and get (as in C++) to set and obtain
# data of that matrix.
# Initially the inverse of the matrix is not calculated,
# so it is set to NULL value
# With setInv you invoke solve (builtin function for get the inverse of a matrix)

makeCacheMatrix <- function(x = matrix()) {
    invCache <- NULL
    set <- function(y) {
        x <<- y
        invCache <<- NULL
    }
    get <- function() x
    setInv <- function(solve) invCache <<- solve
    getInv <- function() invCache
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## Write a short comment describing this function
# After have written a makeCacheMatrix object,
# can be used to make calculations in place of a normal Matrix object.
# In this object the inverse of the matrix is stored.
# Whenever I invoke the getInv "method" (please, let me use this term)
# if the inverse is not yet stored, it evaluates the inverse of the matrix and
# stores the result in invCache, value that will be the output if I invoke
# the same method again. This is memory consuming for RAM (I use 1 additional object, invCache)
# but saves times for calculation.

cacheSolve <- function(x, ...) {
    invCache <- x$getInv()
    if(!is.null(invCache)) 
    {
        message("getting cached data")
        return(invCache)
    }
    else
    {
        data <- x$get()#prendo i dati della matricex$get()
        invCache <- solve(data, ...)
    }
    x$setInv(invCache)
    invCache
}
