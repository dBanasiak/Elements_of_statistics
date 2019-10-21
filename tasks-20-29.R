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