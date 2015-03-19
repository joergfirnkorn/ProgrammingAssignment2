# OVERALL DESCRIPTION:
# Matrix inversion is an example of a rather complex computation that can be done much faster
# when the inverse of a matrix is saved in a cache and re-used - and not calculated repeatedly


# FUNCTION 1: "makeCacheMatrix" creates a list containing a function to:
# set the value of the matrix
# get the value of the matrix
# set the value of the inverted matrix
# get the value of the inverted matrix

# (1) "inv" initializes the stored inverse value to NULL; (2) "set"-function sets the value of the
# matrix; (3) "get" gets the matrix value; (4) "setInvmatrix" sets the inverse; (5) "getInvmatrix"
# gets the inverse; and (6) "list" returns a list including all functions above ("special matrix")


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
       
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setInvmatrix <- function(inverse) inv <<- inverse
        getInvmatrix <- function() inv
        list(set = set,
             get = get,
             setInvmatrix = setInvmatrix,
             getInvmatrix = getInvmatrix)
}

# FUNCTION 2: "cacheSolve" returns the inverse of the "special matrix" calculated above (makeCacheMatrix)
# whereby the way the inverse is returned dependes on a condition:
# If the inverse of the matrix has been calculated before it is already stored ("inv" is filled and
# thus not NULL in that scenario). In contrast, if the inverse of the matrix has not been calculated
# before it gets calculated and stored in the cache-variable.

cacheSolve <- function(x, ...) {                        
        inv <- x$getInvmatrix() 
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrix <- x$get()
        inv <- solve(matrix, ...)
        x$setInvmatrix(inv)
        return(inv)
}



# TEST
# making test matrix
# x <- rbind(c(3, -7), c(4, -8))
# x
#     [,1] [,2]
# [1,]    3   -7
# [2,]    4   -8
# m <- makeCacheMatrix(x)
# m
# $set
# (...)
# <environment: 0x0000000008a5f0a0>

# m$get()
#      [,1] [,2]
# [1,]    3   -7
# [2,]    4   -8

# First run: no cache - calculated
# cacheSolve(m)
#      [,1] [,2]
# [1,]   -2 1.75
# [2,]   -1 0.75

# Second run: from cache - not calculated
# cacheSolve(m)
# getting cached data
#      [,1] [,2]
# [1,]   -2 1.75
# [2,]   -1 0.75

# re-inverting re-creates original matrix
# m2 <- cacheSolve(m)
# getting cached data
# m2
#      [,1] [,2]
# [1,]   -2 1.75
# [2,]   -1 0.75

# m_re_inv <- makeCacheMatrix(m2)
# m_re_inv
# $set
# function (y) 
# {
#    x <<- y
#    inv <<- NULL
# }
# <environment: 0x0000000008a6c3d8>
#
# $get
# (...)
# <environment: 0x0000000008a6c3d8>

# m_re_inv$get()
#      [,1] [,2]
# [1,]   -2 1.75
# [2,]   -1 0.75

# cacheSolve(m_re_inv)
#      [,1] [,2]
# [1,]    3   -7   <- 2x inverted = original matrix :-)
# [2,]    4   -8  