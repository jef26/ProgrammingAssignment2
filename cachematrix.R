## Put comments here that give an overall description of what your
## functions do

## creates a special matrix object that caches its inverse

makeCacheMatrix <- function(x = matrix(), tol = sqrt(.Machine$double.eps)) {
        dnx <- dimnames(x)
        if(is.null(dnx)) dnx <- vector("list", 2)
        s <- svd(x)
        nz <- s$d > tol * s$d[1]
        structure(
                if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else x,
                dimnames = dnx[2:1])
        }
get <- function() x
setinv <- function(nz) m <<- nz
getinv <- function() m
list(set = set, get = get,
     setinv = setinv,
     getinv = getinv)
}


## computes the inverse of the special matrix.  
## If inverse has already been calculated matrix is not changed. 

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- inv(data, ...)
        x$setinv(m)
        m
        ## Return a matrix that is the inverse of 'x'
        solve(x)
}
