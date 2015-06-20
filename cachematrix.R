## The following functions allow the user to cache the inverse of a 
## square matrix so that the user need not recompute the inverse if the
## original matrix has not changed.

## These functions were described, and the general outline was given,
## as part of the R Programming Course under the direction of 
## Roger Peng, Johns Hopkins University.

## The makeCacheMatrix function does three things:
##     -- first, it accepts the argument and thus holds that value
##        in its environment
##     -- second, it sets the inverse matrix to be null (it does this
##        whenever it is alled or if the internal "set" function is 
##		  called, meaning that a new value for the matrix has
##		  been entered).  This also makes the inverse matrix part of
##        the environment.  		  
##     -- third, it creates a vector holding four functions that 
##        then give the user a way to
##            -- set the value of the original matrix
##            -- get the original matrix
##            -- set the inverse matrix
##            -- get the inverse matrix
##   

makeCacheMatrix <- function(x = matrix()) {
    ## create the inverse matrix identifier
	m <- NULL
	set <- function(y) {
	           x <<- y   ## thus changing the value of the original matrix
			   m <<- NULL
		   }
    get <- function() x  ## just returns the stored original matrix
	setInverse <- function( mat_inv) m <<- mat_inv  ## sets a new inverse
	getInverse <- function() m  ## just returns the inverse matrix
	
	list(set= set, get = get, setInverse = setInverse, getInverse = getInverse)
	
}


## The cacheSolve function accepts a vector created by makeCacheMatrix 
## and then either returns the previously computed inverse or, if such an 
## inverse does not yet exist then this function will both compute it and 
## store the result for later inspection.

cacheSolve <- function(x, ...) {
    ## first just try to get the inverse matrix
	m <- x$getInverse()
	## if the returned value is not NULL then we can just 
	##   return that previously computed value.
	if( !is.null(m)) return(m)
	
	## if the returned value was NULL then we need to 
	## both compute the inverse matrix and store that result
	## back in the environment of makeCacheMatrix
	
	tm <- x$get()          ## get  local copy so that we can work with it
	im <- solve(tm)        ## find the inverse
	x$setInverse( im )     ## store the result for later use
	im                     ## return the just computed inverse matrix
	
    
}
