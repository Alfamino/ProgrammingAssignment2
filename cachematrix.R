## makeCacheMatrix is the function that helps
## in management of the matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
      m_inv <- NULL # stored inverse of the matrix
      set <- function( new_x ) { # function that sets a new matrix
        x <<- new_x # set new matrix
        m_inv <<- NULL # inverse is null because it has not been claculated
      }
      get <- function() x # get the regular matrix
      setinverse <- function(inverse_m) m_inv <<- inverse_m # set the calculated inverse
      getinverse <- function() m_inv # return the chaced inverse
      #
      list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## cacheSolve is the function that return the inverse of the matrix
## If the inverse was caculated before the catched one is returned
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getinverse() # get the stored data
        if(!is.null(m_inv)) { # if the inverse was previously calculated...
          message("getting cached inverse of the matrix")
          return(m_inv) #..return it
        }
        data <- x$get() # get the up to date regular matrix
        m_inv <- solve(data) # calculate inverse
        x$setinverse(m_inv) # store the inverse for later
        m_inv # return the just caculated inverse
}
