pkgs <- c('reshape2', 'plyr', 'ggplot2', 'dplyr', 'data.table', 'Lahman')
install.packages(pkgs)


pew <- read.delim(
  file = "http://stat405.had.co.nz/data/pew.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = F
)


tb <- read.csv(
  file = "http://stat405.had.co.nz/data/tb.csv",
  header = TRUE, 
  stringsAsFactors = FALSE
)



weather <- read.delim(
  file = "http://stat405.had.co.nz/data/weather.txt",
  stringsAsFactors = FALSE
)


Causes of Messiness

There are various features of messy data that one can observe in practice. Here are some of the more commonly observed patterns.

Column headers are values, not variable names
Multiple variables are stored in one column
Variables are stored in both rows and columns
Multiple types of experimental unit stored in the same table
One type of experimental unit stored in multiple tables


pew <- read.delim(
  file = "http://stat405.had.co.nz/data/pew.txt",
  header = TRUE,
  stringsAsFactors = FALSE,
  check.names = F
)


library(reshape2)
pew_tidy <- melt(
  data = pew,
  id = "religion",
  variable.name = "income",
  value.name = "frequency"
)



bnames2 <- read.csv("bnames2.csv")


greg <- subset(bnames2, name == "Greg")


library(ggplot2)
qplot(x = year, y = prop, data = greg, geom = 'line')


michelle <- subset(bnames2, name == "Michelle")
qplot(x = year, y = prop, data = michelle, geom = 'line')


qplot(x = year, y = prop, data = michelle, geom = 'point')

qplot(x = year, y = prop, data = michelle, geom = 'line', group = sex)


greg_soundex = greg$soundex[1]
greg_like <- subset(bnames2, soundex == greg_soundex)
qplot(x = year, y = prop, data = greg_like, geom = 'point')


Data Manipulation Verbs

It is good to think of data manipulation in terms of verbs. Those of you who have used SQL or pandas might be familiar with these already. Here are some basic verbs that we will be exploring

subset
mutate
arrange
summarize
join



births <- read.csv("births.csv")
qplot(year, births, data = births, color = sex, geom = 'line')


Join

Going back to the question we were trying to address, we need a way to join the births data with bnames2 so that we can compute total numbers. Base R has the merge function to achieve this. But, I am partial to the join function in plyr, and so we shall use that.

library(plyr)
bnames2_b <- join(bnames2, births, by = c("sex", "year"))


greg <- subset(bnames2_b, name == 'Greg')
greg <- mutate(greg, tot = prop * births)
qplot(year, births, data = greg, geom = 'line')


greg_tot <- summarize(greg, tot = sum(prop * births))


Split the dataset by name.
Apply the summary computations to each name.
Combine the summaries created into a single dataset.


# Split
pieces <- split(bnames2_b, bnames2_b$name)

# Apply
results <- vector("list", length(pieces))
for (i in seq_along(pieces)){
  results[[i]] <- summarize(pieces[[i]],
                            name = name[1],
                            tot = sum(prop * births)
  )
}

# Combine
result <- do.call("rbind", results)

most_pop_name <- arrange(result, desc(tot))[1,"name"]
