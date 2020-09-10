## The first function, makeVector creates a special "vector"
##containing a function to

#set the value of the vector
#get the value of the vector
#set the value of the inverse
#get the value of the inverse

## Write a short comment describing this function

makeCachematrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){  #set the value of the vector
          x <<- y
          inv <<- NULL
        }
        get <- function(){x}  #get the value of the vector
        setinverse <- function(inverse){inv <<- inverse}  #set the value of the inverse
        getinverse <- function(){inv}   #get the value of the inverse
        list(set = set, get, setinverse=setinverse, getinverse=getinverse)
}


#The function calculates the mean of the special "vector" created with the above function.
#It first checks if the inverse of the matrix has already been calculated
cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinverse(inv)
  inv
}
