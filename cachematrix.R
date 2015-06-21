###############################################################################
#
# Copyright (C) 2015, Diego Rabatone Oliveira <diraol(at)diraol(dot)eng(dot)br>
#
# This file is part of Radar Parlamentar.
#
# Radar Parlamentar is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Radar Parlamentar is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with it.  If not, see <http://www.gnu.org/licenses/>.
#
#
# Coursera R Programming Course <rprog-015>
# Programming Assignment 2: Lexical Scoping
#
# This Program creates an object that stores a Matrix and it's inverse matrix
# in a cached way (if there is no inverse matrix it will calculate and save,
# if there is a cached inverse matrix than it will return the cached matrix)
#
###############################################################################

###############################################################################
# This function stores the matrix and the inverse matrix. It has setter and   #
# getter methods for both matrix and inverse matrix.                          #
# It set's the inverse matrix as NULL by default when a new matrix is set.    #
# And it doesn't allow changes on specific positions of the matrix.           #
###############################################################################
makeCacheMatrix <- function(x = matrix()) {
    # inv = inverse from x matrix
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv

    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}

###############################################################################
# This function verify if the matrix has a cached version of the inverted     #
# matrix. If it has, then the function returns the cached version. If not,    #
# then it calculates the inverted matrix, caches it and then returns it.      #
###############################################################################
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()

    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data)
    x$setinverse(inv)

    #returns the inverted matrix
    inv
}

################################################################################
#                               Testing outputs                                #
#
# > set.seed(42)
# > mymat <- matrix(rnorm(25),5,5)
# > mymat
# [,1]        [,2]       [,3]       [,4]       [,5]
# [1,]  1.3709584 -0.10612452  1.3048697  0.6359504 -0.3066386
# [2,] -0.5646982  1.51152200  2.2866454 -0.2842529 -1.7813084
# [3,]  0.3631284 -0.09465904 -1.3888607 -2.6564554 -0.1719174
# [4,]  0.6328626  2.01842371 -0.2787888 -2.4404669  1.2146747
# [5,]  0.4042683 -0.06271410 -0.1333213  1.3201133  1.8951935
#
# > solve(mymat)
# [,1]      [,2]      [,3]       [,4]      [,5]
# [1,]  0.68183594 -1.078746 -1.142347  0.7439971 -1.484073
# [2,]  0.09613058 -1.419227 -2.292037  1.3808827 -2.411346
# [3,] -0.06909654  1.819392  2.192748 -1.1807929  2.654590
# [4,]  0.14181901 -1.118677 -1.694580  0.7147536 -1.640329
# [5,] -0.24590901  1.090359  1.502458 -0.6939420  2.093756
#
# > solve(solve(mymat))
# [,1]        [,2]       [,3]       [,4]       [,5]
# [1,]  1.3709584 -0.10612452  1.3048697  0.6359504 -0.3066386
# [2,] -0.5646982  1.51152200  2.2866454 -0.2842529 -1.7813084
# [3,]  0.3631284 -0.09465904 -1.3888607 -2.6564554 -0.1719174
# [4,]  0.6328626  2.01842371 -0.2787888 -2.4404669  1.2146747
# [5,]  0.4042683 -0.06271410 -0.1333213  1.3201133  1.8951935
#
# > cach1 <- makeCacheMatrix(mymat)
#
# > cach1$get()
# [,1]        [,2]       [,3]       [,4]       [,5]
# [1,]  1.3709584 -0.10612452  1.3048697  0.6359504 -0.3066386
# [2,] -0.5646982  1.51152200  2.2866454 -0.2842529 -1.7813084
# [3,]  0.3631284 -0.09465904 -1.3888607 -2.6564554 -0.1719174
# [4,]  0.6328626  2.01842371 -0.2787888 -2.4404669  1.2146747
# [5,]  0.4042683 -0.06271410 -0.1333213  1.3201133  1.8951935
#
# > cach1$getinverse()
# NULL
#
# > cacheSolve(cach1)
# [,1]      [,2]      [,3]       [,4]      [,5]
# [1,]  0.68183594 -1.078746 -1.142347  0.7439971 -1.484073
# [2,]  0.09613058 -1.419227 -2.292037  1.3808827 -2.411346
# [3,] -0.06909654  1.819392  2.192748 -1.1807929  2.654590
# [4,]  0.14181901 -1.118677 -1.694580  0.7147536 -1.640329
# [5,] -0.24590901  1.090359  1.502458 -0.6939420  2.093756
#
# > cach1$getinverse()
# [,1]      [,2]      [,3]       [,4]      [,5]
# [1,]  0.68183594 -1.078746 -1.142347  0.7439971 -1.484073
# [2,]  0.09613058 -1.419227 -2.292037  1.3808827 -2.411346
# [3,] -0.06909654  1.819392  2.192748 -1.1807929  2.654590
# [4,]  0.14181901 -1.118677 -1.694580  0.7147536 -1.640329
# [5,] -0.24590901  1.090359  1.502458 -0.6939420  2.093756
#
# > cacheSolve(cach1)
# getting cached data
# [,1]      [,2]      [,3]       [,4]      [,5]
# [1,]  0.68183594 -1.078746 -1.142347  0.7439971 -1.484073
# [2,]  0.09613058 -1.419227 -2.292037  1.3808827 -2.411346
# [3,] -0.06909654  1.819392  2.192748 -1.1807929  2.654590
# [4,]  0.14181901 -1.118677 -1.694580  0.7147536 -1.640329
# [5,] -0.24590901  1.090359  1.502458 -0.6939420  2.093756
#
# > round(cach1$get() %*% cach1$getinverse(),10)
# [,1] [,2] [,3] [,4] [,5]
# [1,]    1    0    0    0    0
# [2,]    0    1    0    0    0
# [3,]    0    0    1    0    0
# [4,]    0    0    0    1    0
# [5,]    0    0    0    0    1
