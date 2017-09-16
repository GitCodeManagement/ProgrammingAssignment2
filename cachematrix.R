## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #set inverse value to NULL in parent environment
        m <- NULL
        
        #set new matrix value to x (available in parent environment)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #get x from parent environment
        get <- function() x
        
        #set inverse value to m in parent environment
        setInverse <- function (inverse) m<<- inverse
        
        #get value of m from parent environment
        getInverse <- function() m
        
        #create list of named functions
        list(set =set, get= get, setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        
        # get the value of inverse
        m <- x$getInverse()
        
        #check whether inverse exists
        if(!is.null(m)) {
                message ("getting cached data")
                
                #if inverse exists return the value of its inverse for the given matrix
                return(m)
        }
        
        #if inverse does not exists, assign the new matrix to variable
        data <-x$get()
        #calculate inverse
        m <- solve(data,...)
        #cache inverse
        x$setInverse(m)
        #return inverse value
        m
        ## Return a matrix that is the inverse of 'x'
        
}
