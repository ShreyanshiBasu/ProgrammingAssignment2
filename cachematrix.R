## COMPUTING THE INVERSE OF A MATRIX IN AN EFFICIENT WAY.
## THIS FUNCTION IS USED TO SET UP THE MATRIX AND CALL THE RESPECTIVE FUNCTIONS. 

makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}



## IT CHECKS WHETHER THE INVERSE OF THE MATRIX IS THERE OR NOT. IF YES THEN IT SEARCHES THE CACHE AND DISPLAYS THE OUTPUT. ELSE COMPUTES THE INVERSE USING THE SOLVE()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)
    inv
}
