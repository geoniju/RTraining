#Please read the code and the comments, then experiment with the code to test 
#any points that remain unclear.   When you  finished experimenting, 
#*****remove/fix any*******
#code that generates errors and click submit.

#Suppose that one column of a data set captures the season of the year
season_column = c("summer", "winter", "fall", "winter", "spring", "fall")

#The season is a categorical variable and so it is advantageous to store it as 
#a factor.
season = factor(season_column)
# [1] summer winter fall   winter spring fall  
# Levels: fall spring summer winter

#Internally, a factor stores a vector of levels containing the distinct
#values the variable takes on.
levels(season)
# "fall"   "spring" "summer" "winter"

#and vector of integers indicating which value is in each index
as.numeric(season)
# [1] 3 4 1 4 2 1

#Thus, 
stopifnot(levels(season)[as.numeric(season)] == season_column) 
#(Actually, the 'as.numeric' part isn't necessary here.)

#To give the categories an ordering, we add additional parameters to the factor 
#function
season = factor(season_column, levels = c("spring", "summer", "fall", "winter"), order = TRUE)
# [1] summer winter fall   winter spring fall  
# Levels: spring < summer < fall < winter

#factor variables allow you to make use of powerful built-in functions.
#Consider the iris dataframe, a built-in data set
head(iris)
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.0         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa

#As
ls.str(iris)
#shows, Species is stored as a factor

# Petal.Length :  num [1:150] 1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
# Petal.Width :  num [1:150] 0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
# Sepal.Length :  num [1:150] 5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
# Sepal.Width :  num [1:150] 3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
# Species :  Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

#This makes it convenient to use R's aggregate function to compute the median
#petal length.
median_petal_lengths = aggregate(iris["Petal.Length"], by = iris["Species"], FUN=median)
#      Species Petal.Length
# 1     setosa         1.50
# 2 versicolor         4.35
# 3  virginica         5.55
stopifnot(median_petal_lengths[which(median_petal_lengths[["Species"]] == "setosa"), "Petal.Length"] == 1.5)


