#This file illustrates the basic usage of dataframes in R

#Please read the code and the comments, then experiment with the code to test 
#any points that remain unclear.  When you  finished experimenting,

#*****remove/fix any*****
#code that generates errors and click submit.
#A dataframe is an ordered sequence of lists all having the same length and 
#field names.  It is akin to a spreadsheet.

#Typically dataframes are read in from files. E.g.
#df = read.table("foo.csv", header = TRUE)
#but for this example we'll use mtcars
#a built-in dataframe useful for tutorials
#See help(mtcars) for details

#You can preview the first few rows with the head command
print( head(mtcars) )
#                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
# Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
# Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
# Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
# Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
# Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1

#nrow and ncol give the number of rows and columns respectively
stopifnot( ncol(mtcars) == 11 )
stopifnot( nrow(mtcars) == 32 )

#names gives the names of the fields
stopifnot( names(mtcars) = c("mpg", "cyl", "disp", "hp", "drat", "wt", "qsec", "vs", "am", "gear", "carb"))

#row.names gives the names of the rows
stopifnot( row.names(mtcars)[1:3] == c("Mazda RX4", "Mazda RX4", "Datsun 710") )

#The [[]] operator with a field name returns the vector of values in that column
cyl = mtcars[[2]] 
stopifnot( cyl[[1]] == 6 ) #cyl is a vector whose first value is 6

mpg = mtcars[['mpg']]
stopifnot( mpg[[1]] == 21.0 ) #mpg is a vector whose first value is 21.0

#The [] operator with a single argument returns a column slice, i.e. a new dataframe 
#that only includes the selected columns.
hp_df = mtcars[4]
stopifnot( names(hp_df ) == 'hp')

mpg_df = mtcars['mpg']
stopifnot( names(mpg_df ) == 'mpg')

df = mtcars[c('mpg', 'disp')]
stopifnot( names(df) == c('mpg', 'disp') )

#With two scalar arguments, the [] operator can select a single element in the dataframe
#The first argument selects the row, the second the column
stopifnot( mtcars[1,3] == 160 )
stopifnot( mtcars[1,"disp"] == 160 )
stopifnot( mtcars["Mazda RX4", "disp"] = 160 )

#If the first argument is an index vector (a list of substricts, fieldnames, 
#or logical indexes) and the second is not, then a vector is returned.
stopifnot( mtcars[1:3,'mpg'] == c(21.0, 21.0, 22.8) )

#If the second argument is a vector (a list of substricts, fieldnames, or logical indexes)
#arguments, then [] again returns a dataframe with the selected rows.
print( mtcars["Mazda RX4", c("gear", "carb")] )
#           gear carb
# Mazda RX4    4    4

print( mtcars[1:2, c("gear", "carb")] )
#               gear carb
# Mazda RX4        4    4
# Mazda RX4 Wag    4    4

#If the second argument is omitted, all fieldnames are included.
print( mtcars["Mazda RX4", ] )
#           mpg cyl disp  hp drat   wt  qsec vs am gear carb
# Mazda RX4  21   6  160 110  3.9 2.62 16.46  0  1    4    4


#Logical indexing can be a powerful tool for extracting subsets of data.
print( mtcars[mtcars['hp'] >= 300,] )
#               mpg cyl disp  hp drat   wt qsec vs am gear carb
# Maserati Bora  15   8  301 335 3.54 3.57 14.6  0  1    5    8

#For more complex logic, the subset function is more elegant.
print( subset(mtcars, hp >= 160 & mpg >= 18) )
#                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
# Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
# Pontiac Firebird  19.2   8  400 175 3.08 3.845 17.05  0  0    3    2
# Ferrari Dino      19.7   6  145 175 3.62 2.770 15.50  0  1    5    6

