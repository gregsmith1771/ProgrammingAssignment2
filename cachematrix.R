## DISCLAIMER: the 2 functions below are based almost entirely on the 2 example functions from the R Programming Assignment 2 instructions.
## Each function contains slight changes from the example functions. Furthermore, all comments/explanations of these 2 functions
## are based almost exclusively from material in Daniele P's Assignment 2 instructions on GitHub, which are found at this link:
## https://github.com/DanieleP/PA2-clarifying_instructions. 

## Function 1: makeCacheMatrix
## makeCacheMatrix is a function that creates a special matrix object that caches its inverse. 
## makeCacheMatrix is a function that stores 4 functions: set, get, setinv, and getinv.
## Each of the 4 functions is stored in a list. In order to ensure that calling the function is intuitive and simple, each function in the 
## list is assigned its own name (e.g. set = set, get = get, etc.)
## set is a function that changes the vector x stored in makeCacheMatrix. Since x is assigned to y via the "<<-" operator, x is equivalent to y 
## throughout the entire vector, rather than only being equivalent to y in the set function. 
## get is a function that returns the vector x in makeCacheMatrix. get is defined in makeCacheMatrix and called from cacheSolve.
## setinv is a function that stores the value of the variable m in makeCacheMatrix.
## getinv is a function that returns the value of the variable m in makeCacheMatrix.
makeCacheMatrix <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## Function 2: cacheSolve
## cacheSolve either computes the inverse of a matrix or retrieves a cached value for the inverse of a matrix.
## cacheSolve takes as its input an object where makeCacheMatrix is stored. For the sake of simpler explanation, let's store makeCacheMatrix in the
## following variable z, after passing the matrix object mat to makeCacheMatrix: 
## > mat <- matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3)
## > z <- makeCacheMatrix(mat)
## The first thing that cacheSolve does is to check that the value m that was stored in makeCacheMatrix is not NULL. If m is not NULL 
## (i.e. if(!is.null(m))), then cacheSolve returns the stored value of m in makeCacheMatrix and the message: "getting cached data". 
## However, if m is NULL, then cacheSolve computes the inverse of the invertible matrix that is stored in our matrix example, mat.

## When cacheSolve is first used to compute the inverse of the matrix named mat, m <- solve(data) is the line that actually computes the inverse
## matrix and assigns that value to m. The line below that: x$setinv(m) then stores the just computed value of m in the setinv function located in
## makeCacheMatrix.
## The second time that cacheSolve is used, the line m <- x$getinv searches makeCacheMatrix for the value of m. Since the first use of cacheSolve
## computed the inverse of our matrix mat and stored the value of that inverse matrix in m, x$getinv will find that m is not NULL. In fact, m is
## equal to the inverse of the matrix mat that was computed by the first call of cacheSolve. Since m is not NULL, the if statement if(!is.null(m))
## TRUE and cacheSolve prints the message "getting cached data" and returns the value of m. Furthermore, when m is not NULL, the 4 lines underneath
## the if statement's second curly brace are superfluous.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

## You will find the results of two print-outs of the cacheSolve function. Each line with one # represents print-outs on the R console. 

#> mat <- matrix(c(1,0,5,2,1,6,3,4,0),nrow = 3,ncol = 3)
#> mat
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    0    1    4
#[3,]    5    6    0
#> z <- makeCacheMatrix(mat)
#> x$get()
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    0    1    4
#[3,]    5    6    0
#> x$getinv()
#NULL
#> cacheSolve(z)
#     [,1] [,2] [,3]
#[1,]  -24   18    5
#[2,]   20  -15   -4
#[3,]   -5    4    1
#> cacheSolve(z)
#getting cached data
#     [,1] [,2] [,3]
#[1,]  -24   18    5
#[2,]   20  -15   -4
#[3,]   -5    4    1

