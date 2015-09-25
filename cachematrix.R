#
#test script for this program:
#mat<-makeCacheMatrix(matrix(rnorm(100),2,2))
#cacheSolve(mat)



#this function creates a list that can cache and solve
#an invertable matrix 
makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <-function(solve) inv<<-solve
	getinv <- function() inv
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


#the function calculates the inverse of a given matrix
#but it first checks to see if the inverse of the matrix
#already exists in the list of the previous function
#the function returns the inverted matrix
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inv<-x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data<-x$get()
	inv<-solve(data,...)
	x$setinv(inv)
	inv
}
