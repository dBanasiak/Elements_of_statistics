# Zadanie 2
# Use the rep() function to create a logical vector starting with three true values,
# then four false values, followed by two true values and finally five false values. 
# Assign this logical vector to the variable x. Finally, convert this vector onto 
# the numeric vector. How have true and false values changed?
x <- c(rep(TRUE,3),rep(FALSE,4),rep(TRUE,2),rep(FALSE,5))
x
as.numeric(x)

# Zadanie 3
# A palindrome is a vector whose elements read from the end form the same vector as those read from
# the beginning. Create such a vector of 100 numbers, where the first 20 numbers are consecutive
# natural numbers, then there are 10 zeros, then 20 consecutive even numbers, and the remaining
# elements are determined by palindromicity (symmetry condition).
z <- c(rep(1:20),rep(0,10),seq(2,40,2))
z <- c(z,rev(z))
z

# Zadanie 4
# From vector letters, choose letters at positions 5, 10, 15, 20, 25.
y <- c(seq(5,25,5))
y <- letters[y]
y

# Zadanie 5
# Create a vector x of natural numbers from 1 to 1000, and then replace even numbers to their inverse.
k <- c(rep(1:1000))
sequence <- seq(2,1000,2)
round(replace(k, sequence, 1/sequence), digits = 1)

# Zadaie 6
# Order the elements of the vector (6,3,4,5,2,3) from 
# largest to smallest using the function order().
x = c(6,3,4,5,2,3)
x = x[order(x, decreasing = TRUE)]
x

# Zadanie 7 
# Calculate the signs of vector elements  (−1.876,−1.123,−0.123,0,0.123,1.123,1.876).
# Then round the elements of this vector to two decimal points. At the end, calculate
# the floor of each element of the new vector.
n <- c(-1.876,-1.123,-0.123,0,0.123,1.123,1.876)
sign(n)
round(n,digits = 2)
floor(n)

# Zadanie 8
# Calculate the square root of each natural number from 1 to 100 million in two ways. First use built-in
# function in R, and then use exponentiation. Which way works faster? Hint: You can use the
# Sys.time() function to check the length of time the program has run.
x <- c(rep(1:100000000))

start_time <- Sys.time()
y <- sqrt(x)
end_time <- Sys.time()
end_time-start_time

start_time <- Sys.time()
z <- x^(0.5)
end_time <- Sys.time()
end_time-start_time

# Zadanie 9
# Calculate the sum, product, arithmetic mean, variance, standard deviation, median and quantiles of
# orders 0, 1/4, 1/2, 3/4, 1 of vector 1,2,...,10. What are the quantiles of order 0 and 1?
v <- c(rep(1:10))

summ <- sum(v)
summ

product <- prod(v)
product

arithmeticMean <- mean(v)
arithmeticMean

variance <- var(v)
variance

standardDeviation <- sd(v)
standardDeviation

med <- median(v)
med

quantilesOfOrders <- quantile(v, probs = c(seq(0,1,0.25)))
quantilesOfOrders

# Zadanie 10
# The schoolmath package contains the primlist data set, which contains prime numbers between
# 1 and 9999999.
library(schoolmath)
n <- schoolmath::primes(0,1000)
n
m <- length(schoolmath::primes(100,500))
m