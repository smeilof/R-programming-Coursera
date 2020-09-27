## Assume x is always invertible
## inverse matrix, or "sol" for solution, is usually retrieved by solve(x) 

## Write a function to Cache sol
## 1. set value of matrix
## 2. get the value of matrix
## 3. set the value of the inverse matrix "sol"
## 4. get the value of sol

makeCacheMatrix <- function(x = matrix()){
	sol <- NULL
	set <- function(y){
		x <<- y
		sol <<- NULL
	}
	get <- function() x
	setsolve <- function(solve) sol <<- solve
	getsolve <- function() sol
	list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## Write a function tht computes sol of x, x returned by makeCacheMatrix
## If the sol has already been computed it retrieves sol from the cache

## 1. check if sol has already been calculated
## 2. If so, get sol from cache and skip computation
## 3. If not, do computation of sol anyway and put in in the cache

cacheSolve <- function(x, ...){
	sol <- x$getsolve()
	if(!is.null(sol)){
		message("retrieving inverse matrix solution")
		return(sol)
	}
	data <- x$get()
	sol <- solve(data, ...)
	x$setsolve(sol)
	sol
}
