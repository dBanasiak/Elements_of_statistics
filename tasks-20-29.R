# Zadanie 20
# Check which variables in the Cars93 dataset (US car sales in 1993) from the MASS package are
# factors. Moreover, calculate the number of cars for the place of origin (variable Origin) and type of
# car (variable Type) in the form of a table.
install.packages("MASS")
library(MASS)
sapply(Cars93, is.factor)
origin <- Cars93$Origin
type <- Cars93$Type
table(origin,type)

# Zadnaie 21
# Read the dataset data_cancer.csv and then:
data_cancer <- read.csv(file="data/data_cancer.csv", header = T, sep=",")
# 1) Display only even rows from the read data frame.
data_cancer[data_cancer$id %% 2 == 0, ]
# 2) Using logical operators, display only the rows that correspond to patients older than 50 years
#    with lymph node metastases (lymph_node = 1).
data_cancer[data_cancer$age > 50 & data_cancer$lymph_nodes == 1, ]
# 3) Display the column names in this dataset, and then calculate the length (number of characters)
#    of these names.
colnames(data_cancer)
nchar(colnames(data_cancer))

# Zadanie 22
# The following data gives the average monthly temperature (in oF) in New York.
# 1) Enter the above data into the data frame with one variable NY_F.
Temperature <- data.frame(row.names = c("January","Febuary","March","April","May","June","July","August","September","October","November","December"),NY_F=c(32,33,41,52,62,72,77,75,68,58,47,35))
Temperature
# 2) Create and include a new variable NY_C (rounded to two decimal points) giving the temperature
#    in Celsius degrees. Hint: (xoF)=(x−32)⋅5/9(oC)
NY_C <- round(c((Temperature$NY_F-32)*5/9), digits=2)
Temperature <- cbind(Temperature,NY_C)
Temperature
# 3) Change columns names to NY_Fahrenheit and NY_Celsius.
colnames(Temperature) <- c("NY_Fahrenheit", "NY_Celsius")
Temperature
# 4) Remove the column with temperature in Fahrenheit degrees.
Temperature$NY_Fahrenheit <- NULL
Temperature
# 5) Save the obtained data set in the file NY_temp.RData
save(Temperature, file="data/NY_temp.RData")

# Zadanie 23
# Write a function that converts an angle measure in degrees to radians. Check this function for angles
# of 0∘,30∘,45∘,60∘,90∘. Then prepare a data frame in which the values of the sine, cosine, tangent
# and cotangent functions for angles with such measures will be collected.
angels <- c(0,30,45,60,90)
deg <- round((angels*pi/180), digits = 3)
cosinus <- cos(deg)
sinus <- sin(deg)
tang <-tan(deg)
ctg <- 1/tang
ctg
frame_angels <- data.frame(rep(1:5), sin = sinus, cos=cosinus, tan = tang, ctg = ctg)
frame_angels

# Zadanie 24
# Calculate the product of elements of any vector x using the while, repeat and for loops (each
# separately).
vec <- rep(1:5)
whileLoop <- function(x) {
  temp <- 1
  iterator <- 1
  while(iterator <= length(x)){
    temp <- temp * x[iterator] 
    iterator <- iterator + 1
  }
  return(temp)
}
whileLoop(vec)

repeatLoop <- function(x) {
  temp <- 1
  iterator <- 1
  repeat {
    temp <- temp * x[iterator]
    
    if(iterator == length(x)){
      return(temp)
      break
    }
    iterator <- iterator + 1
  }
}
repeatLoop(vec)

forLoop <- function(x) {
  temp <- 1
  for(i in 1:length(x)) {
    temp <- temp * x[i]
  }
  return(temp)
}
forLoop(vec)

# Zadanie 25
# How many numbers (nr) are greater than a million for 1≤r≤n≤100?
binomTest <- function() {
  temp <- 0
  for(r in 1:100) {
    for(n in r:100) {
      if(choose(n,r)>1000000) {
        temp <- temp + 1
      }
    }
  }
  return(temp)
}
binomTest()

# Zadanie 26
# Write a function that checks if the vector is a palindrome.
vec <- c(1,3,4,4,3,1)
isPalindrme <- function(x){
  palindromeCheck <- 0
  for(i in 0:floor(length(x)/2)){
    if(x[i+1] == x[floor(length(x))-i] & i<floor((length(x)/2))) {
      palindromeCheck <- palindromeCheck + 1
    }
  }
  if(palindromeCheck == floor(length(x)/2)) {
    return(TRUE)
  }
  return(FALSE)
}
isPalindrme(vec)

# Zadanie 27
# Write a function whose argument is a numeric vector and the result is a vector containing the
# smallest three numbers and the largest three numbers in this vector. If the argument is less than
# three numbers, the function should return an error message with the comment “too short argument”.
findVectors <- function(x) {
  if(length(x)<3){
    return("Error in command 'extreme_3(x)': too short argument")
  }
  x <- sort(x)
  newX <- c(x[1],x[2],x[3],x[length(x)-2],x[length(x)-1],x[length(x)])
  return(newX)
}
vec <- c(2, 6, 1, 5, 7, 3, 4)
findVectors(vec)

