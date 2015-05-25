library("corpcor")

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(X = matrix()) {
        solinverse <- NULL
        set <- function(Y){
                X <<- Y
                solinverse <<- NULL
        }
        get <- function() X
        setsolinverse <- function(Inverse) solinverse <<- Inverse
        getsolinverse <- function() solinverse
        list(set=set,get=get,setsolinverse=setsolinverse,getsolinverse=getsolinverse)
}

# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache

cacheSolve <- function(X, ...) {
        solinverse <- X$getsolinverse()
        if(!is.null(solinverse)){
                message("MATRIX INVERSE LOADED FROM CACHE AS IT IS AVAILABLE")
                return(solinverse)
        }
        message("MATRIX INVERSE COMPUTED AS IT IS NOT AVAILABLE IN CACHE")
        data <- X$get()
        solinverse <- pseudoinverse(data, ...)
        X$setsolinverse(solinverse)
        solinverse
}
