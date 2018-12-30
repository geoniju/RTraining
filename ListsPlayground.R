#This file illustrates the basic usage of lists in R

#Please read the code and the comments, then experiment with the code to test 
#any points that remain unclear.  When you  finished experimenting,
#********remove/fix any********
#code that generates errors and click submit.

#A list is a powerful container that has the capabilities of several
#more classical containers.

#Unlike vectors it can contain elements of different types
x = list("Isaac", "Newton", 1643L, 1727L)

#To select a single element, use the [[]] operator
stopifnot(typeof(x[[1]]) == "character")
stopifnot( x[[1]] == "Isaac")

#The operator [] returns a list even when a single element is selected.
print x[1]
# [[1]]
# [1] "Isaac"
stopifnot( typeof(x[1]) == "list" )
stopifnot( as.character(x[1]) == as.character(list("Isaac")))

#Positions are often named like so
x = list(first_name = "Isaac", last_name = "Newton", birth_year = 1643L, death_year = 1727L)

#The names function returns the names of the fields as a vector
stopifnot( names(x) == c('first_name', 'last_name', 'birth_year', 'death_year'))

#With named positions, lists behave like dictionaries (a.k.a symbol tables)
stopifnot( x[['first_name']] == "Isaac")

print x['first_name']
# $last_name
# [1] "Newton"
stopifnot( as.character(x['last_name']) = as.character(list(last_name = "Newton")))

years = c("birth_year", "death_year")
print x[years]
# $birth_year
# [1] 1643

# $death_year
# [1] 1727
stopifnot( as.character(x[years]) == as.character(list(birth_year = 1643L, death_year = 1727L)) )

#for each loops can loop over the elements of a list
acc = c()
for(i in x){
  acc = c(acc,as.character(i))
}
stopifnot(acc == c("Isaac", "Newton", "1643", "1727"))
