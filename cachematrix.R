## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Initialize functions to get or set values to variables in parent environment
makeCacheMatrix <- function(x = matrix()) {
        #set inverse value to NULL 
        m <- NULL
        
        #set new matrix value to x (available in parent environment)
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #display x
        get <- function() x
        
        #set inverse value to m in parent environment
        setInverse <- function (inverse) m<<- inverse
        
        #display inverse value m
        getInverse <- function() m
        
        #create list of named functions
        list(set =set, get= get, setInverse = setInverse,getInverse = getInverse)
}


## Write a short comment describing this function

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
        # get the value of inverse
        m <- x$getInverse()
        
        #check whether inverse exists
        if(!is.null(m)) {
                message ("getting cached data")
                
                #if inverse exists return the value for the given matrix
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
}
