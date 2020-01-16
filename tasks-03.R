# The variable result in the file survey.txt describes the results of the survey of the activities
# of the president of a certain city. 100 city residents were randomly selected, and they were asked
# the following question: How would you rate the activities of the city president? The following answers
# were available: definitely good (a), good (b), wrong (c), definitely wrong (d), I have no opinion (e).
# Which type is this variable? What are its possible values?

# 1,1. Import the data from the file survey.txt into the variable survey.

survey <- read.table("data/survey.txt", header = T)
survey

# 1.2. Present the empirical distribution of the result variable in the form of a table.

## TIP - empirical distribution described by a table
frequency <- table((survey$result))
frequency
tabelka <- cbind(frequency, prop.table(frequency))
colnames(tabelka) <- c('frequency', 'percent')
tabelka

# 1.3. Present the empirical distribution of result variable only for persons
#    with primary education in the form of a table.
freq <- table(survey$result[survey$education == "p"])
tab <- cbind(freq, prop.table(freq))
colnames(tab) <- c('frequency', 'percent')
tab

# 1.4. Illustrate the survey results using a bar chart and pie chart.
barplot(frequency, main="Empirical distribution of answers",
        xlab="Answers", ylab = "Frequency", col=c("black","red", "green", "blue", "cyan"), beside=TRUE)
barplot(prop.table(frequency), main="Empirical distribution of answers",
        xlab="Answers", ylab = "Probability", col=c("black","red", "green", "blue", "cyan"), beside=TRUE)
pie(frequency)

# 1.5 Illustrate the results of the survey using a bar chart, broken down by women and men.
man <- table(survey$result[survey$sex == 'm'])
wman <- table(survey$result[survey$sex == 'f'])
barplot(cbind(man, wman), main="Empirical distribution of answers",
        xlab="Answers", ylab = "Frequency", col=c("black","red", "green", "blue", "cyan"), beside=TRUE)

# 1.6 Interpret the above tabular and graphical results.
#

# 2.1 200 randomly selected 5-second time periods of work of a telephone exchange were examined.
# The number of calls was recorded. The results are contained in the file telephone_exchange.RData.
# Which type is this variable? What are its possible values?
tele_data <- load("data/telephone_exchange.RData", envir = parent.frame())
telephone_exchange

# 2.2 Present the empirical distribution of the number of calls in the form of a table.
tele_tab <- cbind(table(telephone_exchange), prop.table(table(telephone_exchange)))
colnames(tele_tab) <- c('frequency', 'percent')
tele_tab

# 2.3 Illustrate the empirical distribution of the number of calls using a bar chart and pie chart.
barplot(table(telephone_exchange), main="Empirical distribution of number of calls",
        xlab="Number of calls", ylab = "Frequency", col=c("black","red", "green", "blue", "cyan", "magenta"), beside=TRUE)
barplot(prop.table(table(telephone_exchange)), main="Empirical distribution of number of calls",
        xlab="Number of calls", ylab = "Probability", col=c("black","red", "green", "blue", "cyan", "magenta"), beside=TRUE)
pie(table(telephone_exchange))

# 2.4 Calculate the mean of the number of calls, the median of the number of calls, 
# the standard deviation of the number of calls, and the coefficient of variation of the number of calls.
mean(telephone_exchange$number)
median(telephone_exchange$number)
sd(telephone_exchange$number)
coefficientOfVariation <- sd(telephone_exchange$number) / mean(telephone_exchange$number) * 100

# 3.1 The variable in the file failures.txt describes the results of 50 measurements of failure-free 
# operation time of a given device (in hours). Which type is this variable? What are its possible values?
failures <- read.table("data/failures.txt")
failures

# 3.2 Present the empirical distribution of the failure-free operation time in the form of a table.
frequency = table(cut(failures$V1, breaks = seq(0, 3500, 500)))

fail_table <- cbind(frequency = frequency, percent =  prop.table(frequency))
fail_table

# 3.3 Illustrate the empirical distribution of the failure-free operation time using a histogram, 
#     boxplot and stemplot. What are advantages and disadvantages of these charts?
hist(failures$V1)$breaks
# końcówka $breaks pokazuję nam breaks do zadania 3.2

hist(failures$V1, 
     xlab = "Failure-free operation time", 
     main = "Empirical distribution of braking distance",
     probability = TRUE, 
     col = "lightblue")
lines(density(failures$V1), col = "green", lwd = 2)

boxplot(failures$V1, 
        ylab = "Braking distance", 
        main = "Empirical distribution of failure-free operation time")

stem(failures$V1)

# 3.4 Calculate the mean, median, standard deviation, coefficient of variation,
#     skewness and kurtosis of the failure-free operation time.
mean(failures$V1)
median(failures$V1)
sd(failures$V1)
coefficientOfVariation <- sd(failures$V1) / mean(failures$V1) * 100
coefficientOfVariation

library(e1071)
skewness(failures$V1)
kurtosis(failures$V1)
