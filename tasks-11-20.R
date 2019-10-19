# Zadanie 11
# Determine all combinations of vectors (a,b) and (1,2,3) using the rep() and paste() functions.
x <- paste(rep(c(rep("a",3),rep("b",3))), rep(1:3),sep="")
x 

# Zadanie 12
# Create a vector of 30 strings of the form number.letter, where the number is consecutive natural
# numbers from 1 to 30, and the letter is three capital letters X, Y, Z occurring cyclically.
y <- paste(rep(1:30),rep(c("X","Y","Z"),10),sep=".")
y

# Zadanie 13
# In some situations, it may be useful to categorize variable, i.e., a different division into categories
# than would result from the data.
# a) Generate 100 observations that are the answers to the survey questions, each answer can take
#    one of the values: 'a', 'b', 'c', 'd', 'e'.
# b) Categorize the obtained observations so that category 1 includes 'a' and 'b' responses,
#   category 2 'c' and 'd' responses, and category 3 'e' response.
install.packages("car")
library("car")
z <- sample(letters[1:5], size=100, replace = T)
z
recode(z, "'a':'b'=1;'c':'d'=2;'e'=3")

# Zadanie 14
# Create the vector x of elements NA, 3, 14, NA, 33, 17, NA, 41.
x <- c(NA,3,14,NA,33,17,NA,41)
# a) Count the number of missing values.
counter <- length(which(is.na(x)))
counter
# b) Calculate the arithmetic mean without taking into account the missing values.
arithmeticMean <- mean(x, na.rm = TRUE)
arithmeticMean
# c) Remove missing data.
x <- x[!is.na(x)]
x
# d) Replace the missing values with 11
recode(x, "NA=11")

# Zadanie 15
# Create a list called my_list, whose first element will be a two-element character vector containing
# your first name and surname, the second element will be the number π, the third the unique()
# function, and the last element of the list will be a vector consisting of numbers 0.1, 0.2,..,1. Then
# remove the first element and the third element from this list. Finally, create a list containing values of
# the gamma() function for the elements of the my_list object.

# List 1 
my_list <- list(c("Joe", "Doe"), pi, unique(NULL), seq(0,1,0.1))
# List 2
my_list[c(1,3)] <- NULL
# List 3
my_list
my_list1 <- list(gamma(my_list[[1]]), gamma(my_list[[2]])) 
my_list1

# Zadanie 16
# Create a vector of squares of the first 100 natural numbers. Then count which numbers and how
# often they appear in the unity position in subsequent elements of this vector.
x <- c(rep(1:100)^2)
x

# Zadanie 17
# Use the outer() function to calculate the multiplication table for numbers smaller than 6.
outer(1:6,1:6,"*")

# Zadanie 18
# Calculate rank, determinant, inverse, eigenvalues, eigenvectors as well as sums and arithmetic
# means for rows and columns for the following matrix:
# |1 5 3|
# |2 0 5|
# |1 2 1|
# Moreover, multiply this matrix by its inverse.
x <- matrix(c(1,2,1,5,0,2,3,5,1), nrow=3, ncol = 3)
x
rank(x)
determinant(x)
