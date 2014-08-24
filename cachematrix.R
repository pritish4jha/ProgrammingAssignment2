## functions to cache the inverse of a matrix


## special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

	## Initialize the inverse
    i <- NULL

    ## set matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## get the matrix
    get <- function() {
    	## Return matrix
    	m
    }

    ## set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## get the inverse of the matrix
    getInverse <- function() {
        ## Return inverse
        i
    }

    ## Return list
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Next task is to compute the inverse of the special matrix returned by function "makeCacheMatrix"
## We now create our second function "cachesolve"
##If the inverse has already been calculated (and the matrix has not changed), 
##then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {

    ## Return inverse of matrix
    m <- x$getInverse()

    ## Return if inverse already there
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## Get matrix from object
    data <- x$get()

    ## Calculate inverse
    m <- solve(data) %*% data

    ## Set inverse
    x$setInverse(m)

    ## Return
    m
}
