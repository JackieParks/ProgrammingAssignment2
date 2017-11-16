## Using these functions together will compute the inverse of a square, 
## invertible matrix or 
## retrieve it from cache if it has already been cached.

## The first function builds a set of functions and 
##returns the functions within a list to the parent environment.
##Results in an object that contains four functions: 
##set(), get(), setinverse(), and getinverse(). 
##It also includes the two data objects, x and m.


makeCacheMatrix <- function(x = matrix()) {
    m<-NULL                             #creates an empty object m
    set <- function(y) {
        x <<- y         ##assigns value of y to x in the parent environment
        m <<- NULL      ## clears m
    }
    get <- function() x      ##returns x from parent environment
    setsolve <- function(solve) m <<- solve ##setter function for inverse m
    getsolve <- function() m                ## getter function for inverse m
    ## Stores everything in a lsit 
    myList<-list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
    myList      ##returns the list. I know this is unnecessary 
                ##but I was using it for debugging
}


## The second function returns the inverse of a square invertible matrix 
## but checks first to see if the solution has already been cahced and 
## if so it retreives it from the cache. Accepts a matrix created by 
## makeCacheMatrix function defined above
## Returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {    ##returns m if it's not empty
        message("getting cached data")
        return(m)
    }
    data <- x$get()     ##calls the get function from the list
    m <- solve(data, ...)  ##calls the solve function to compute the inverse and assigns it to m
    x$setsolve(m)         
    m
}

## Test code to run functions on a 3x3 invertible matrix
## m1<-matrix(c(1,0,0,3,2,0,4,5,6), 3,3) 
## or an alternate 3x3 invertible matrix m1<-matrix(c(4,0,0,2,7,0,2,9,1),3,3)
## myMatrix<-makeCacheMatrix(m1)
## cacheSolve(myMatrix)


