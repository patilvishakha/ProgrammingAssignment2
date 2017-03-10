## Caching the Inverse of a Matrix: Matrix inversion is usually a costly computation
## and there may be some benefit to caching the inverse of a matrix rather than computing 
## it repeatedly, for example in cases where we compute the inverse inside a loop. The 
## following two function cache the inverse of a matrix.

## The first function, makeCacheMatrix, creates a vector, which is actually a list containing
## a function to set the value of the matrix, get the value of the matrix, set the value of the
## inverse, and get the value of the inverse respectively.

makeCacheMatrix <- function(x = matrix()) {
	m_inverse <- NULL
        set <- function(y){
                x <<- y
                m_inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) m_inverse <<- inv
        getInverse <- function() m_inverse
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function calculates the inverse of the matrix. But before calculating the inverse
## it checks if the inverse has already been computed and cached. If yes, it fetches the value of
## the inverse from the cache and skips the computation. otherwise, it computes and returns the
## inverse.

cacheSolve <- function(x, ...) {
        m_inverse <- x$getInverse()
        if(!is.null(m_inverse)){
                message("getting cached result.")
                return(m_inverse)
        }
        data <- x$get()
        m_inverse <- solve(data, ...)
        x$setInverse(m_inverse)
        m_inverse
}
