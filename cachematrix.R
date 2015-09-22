## R Programming Assignment 2
## This function calculates the inverse of a matrix x and caches it so
##you don't need to recalculate it every time you need it.

## This function creates a special matrix where it is gonna be store 
##the inverse of the matrix calculated in the cacheSolve function
makeCacheMatrix <- function(x = matrix()) {
        #Iniciate the special matrix
        inv <- NULL
        #function that allows to change the
        #the matrix assigned in the argument x
        #in the function makeCacheMatrix
        set <- function(y){
                X <<- y 
                inv <<- NULL
        }
        #A function that returns the matrix stored in x
        #in order to later calculate the inverse.
        get <- function() x
        #This function stores the calculated inverse matrix into m
        setinverse <- function(inversa) inv <<- inversa
        #This function returns the inverse matrix.
        getinverse <- function() inv
        #The output of the makeCacheMatrix function.
        #A list of 4 functions
        list(set = set, get = get, 
             setinverse = setinverse,
             getinverse = getinverse)

}


##This function calculates the inverse of the matrix x.
##If the inverse of the matrix was already calculated, it
##returns the stored inverse matrix.
cacheSolve <- function(x, ...) {
        inv <- x$getinverse() #get the inverse if already stored.
        if(!is.null(inv)){   #returns the inverse matrix if inv is not null.
                message("getting cache data")
                return(inv)
        }
        #If the inv matrix is null, we proceed to calculate it.
        data <- x$get() #get matrix stored in makeCacheMatrix function
        m <- solve(data, ...) #calculate the inverse of the matrix
        x$setinverse(m) #add the value of the inverse matrix
                        #to the function setinverse
        m
}
