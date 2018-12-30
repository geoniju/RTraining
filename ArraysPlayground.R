#This file illustrates the usage of multidimensional arrays in R
#Please read the code and the comments, then experiment with the code to test 
#any points that remain unclear.  When you  finished experimenting, *******remove/fix******** any
#code that generates errors and click submit.

#Arrays are the multidimensinal generalization of vectors.
x = array(data = 1:6, dim = c(2,3))

#R uses column-major format
print(x)
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6

#Single indices can be used into multidimentional arrays.
#Left indices are less significant.
stopifnot( x[1] == x[1,1] )
stopifnot( x[2] == x[2,1] )
stopifnot( x[3] == x[1,2] )
stopifnot( x[4] == x[2,2] )
stopifnot( x[5] == x[1,3] )
stopifnot( x[6] == x[2,3] )

#Index and logical selection can be used to select subarrays
subx = x[c(1,2),c(TRUE,TRUE,FALSE)]
stopifnot(subx == array(data = 1:4, dim = c(2,2)))

#Refer to an entire row or column by not specifiying anything
row1 = x[1,]
stopifnot( row1 == c(1,3,5) )
col3 = x[,3]
stopifnot( col3 = c(5,6) )

#* is for element-by-element multiplication
x2 = x * x
stopifnot( x2[1,1] == x[1,1]*x[1,1] )
stopifnot( x2[2,1] == x[2,1]*x[2,1] )
stopifnot( x2[1,2] == x[1,2]*x[1,2] )
stopifnot( x2[2,2] == x[2,2]*x[2,2] )
stopifnot( x2[1,3] == x[1,3]*x[1,3] )
stopifnot( x2[1,3] == x[2,3]*x[2,3] )

#t is for transpose
xt = t(x)
stopifnot(x[1,3] == xt[3,1])
stopifnot(x[1,2] == xt[2,1])

#%*% is for matrix multiplication
M = x %*% xt
stopifnot( M == array(data=c(35,44,44,56), dim=c(2,2)) )

#%*% will also take the inner product of vectors
stopifnot( c(1,1,1) %*% x[1,] == 1 + 3 + 5 )

#Matrices can also be formed by outer product
M = outer(c(1,2), c(1,2,3))
print(M)
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    6   10

#Matrices can also be stacked rowwise
rbind(x,x)
print(M)
#      [,1] [,2] [,3]
# [1,]    1    3    5
# [2,]    2    4    6
# [3,]    1    3    5
# [4,]    2    4    6

#Or they can be concatenated columnwise
cbind(x,x)
print(M)
#      [,1] [,2] [,3] [,4] [,5] [,6]
# [1,]    1    3    5    1    3    5
# [2,]    2    4    6    2    4    6

