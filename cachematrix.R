## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# this function make the list which contain 4 function one which set matrix 
#2ND to get matrix 
#3rd to get inverse matrix
#4th to set inverse matrix
makeCacheMatrix <- function(x = matrix()) {

    # make matrix to store inverse matrix
    im<-NULL
    
    # function to store matrix
    set<- function(y){
        x<<- y
        im<<- NULL
    }
    
    # function to get matrix
    get<- function() x
    
    #function to set inverse matrix
    setInverse<- function(inverse) im<<- inverse
    
    #function to get inverse matrix
    getInverse<- function() im
    
    # list fo function which will be return when we call that function
    list(set=set, get=get, setInverse= setInverse, getInverse=getInverse)
    
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    im <- x$getInverse()
    
    # checking either we have inverse already or not
    if(!is.null(im)){
        message("getting cached data")
        return(im)
    }
    
    # getting inverse
    data <- x$get()
    # calculating inverse
    im <- solve(data())
    # storing inverse
    x$setInverse(im)
    
    # return inverse od data
    im
}
