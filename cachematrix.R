## There are two functions here 'makeCacheMatrix' and 'cacheSolve':
##	1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##	2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##		If the inverse is already cached & the matrix has not changed, then cacheSolve will retrieve the inverse from the cache.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        invMatrix<-NULL
	setMatrix<-function(y){
		x<<-y
		invMatrix<<-NULL
	}
	getMatrix<-function()x
	setInverse<-function(inverse)invMatrix<<-inverse	
	getInverse<-function()invMatrix
	list(setMatrix=setMatrix,getMatrix=getMatrix,setInverse=setInverse,getInverse=getInverse)
}


## This function computes the inverse of the matrix returned by the makeCacheMatrix function above.
## If the inverse is already cached and the matrix has not changed, then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invMatrix<-x$getInverse()
	if(!is.null(invMatrix)){
		message("Getting cached inverted matrix")
		return(invMatrix)
	}
	matrixData<-x$getMatrix()
	invMatrix<-solve(matrixData,...)
	x$setInverse(invMatrix)
	return(invMatrix)
}
