#This file illustrates the basic usage of vectors in R

#Please read the code and the comments, then experiment with the code to test 
#any points that remain unclear.  When you are finished experimenting, **********remove/fix******** any
#code that generates errors and click submit.

#The simplest container in R is the vector.
x = vector(length=3, mode="numeric")

#The c function concatenates its arguments into a vector.
y = c(4,3,3)

#The default value for the numeric mode is zero
stopifnot( x == c(0,0,0))

#The length function gives the length of a vector
stopifnot(length(y) == 3)

#Individual elements can be accessed using 1-based indexing
x[1] = 2
x[3] = 1
stopifnot( x == c(2,0,1) )

#Standard arithmetic operators can used for vectors
a = 2*x + y
stopifnot( a == c(8,3,4) )

a = a - 1
stopifnot( a == c(7,2,3) )

#Boolean-valued operators produce logical vectors
stopifnot( (a>=7) == c(TRUE,FALSE,FALSE))
stopifnot( (a==2) == c(FALSE,TRUE,FALSE))

#Logical vectors and vectors of indices can used to select 
#a subset of entries in a vector
mask = c(TRUE,FALSE,TRUE)
stopifnot( a[mask] == c(7,3) )

indices = c(1,3)
stopifnot( a[indices] == c(7,3))

#Negative indices cause elements to be excluded
stopifnot( a[c(-1,-3)] == c(2) )

#The any, all, and which methods have the intuitive meanings when applied
#to logical vectors
stopifnot( any(c(FALSE,TRUE,FALSE)) )
stopifnot( all(c(TRUE,TRUE,TRUE)) )
stopifnot( which(c(TRUE,FALSE,TRUE)) == c(1,3) )

#Various helper methods for initializing vectors are available
b = rep(3.2, times=5)
stopifnot( b == c(3.2, 3.2, 3.2, 3.2, 3.2))

w = seq(0,3)
stopifnot(w == c(0,1,2,3))

x = seq(0,1,by=0.2)
#Hint: You may have to change the comparison statement
stopifnot(x == c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0))

y = seq(0,1,length.out=3)
stopifnot( x == c(0.0, 0.5, 1.0) )

z = 1:10
stopifnot(z == seq(1,10,by=1))

#for each loops can loop over the elements of a vector
sum = 0
for(i in z){
  sum = sum + i
}
stopifnot(sum == 55)

#Vectors are passed by value
x = 1:10
f = function(a){
  a[1] = 10
}
f(x)
stopifnot(x == 1:10)
