# For the braking distance data in Example 2 of the lecture taking into account the
# theoretical distribution, calculate the values of the three estimators for the standard deviation. 
# Illustrate the three theoretical density functions on the histogram.

brakes <- read.table("data/brakes.txt", dec = ",")
# estimators
## 1 [1] 0.3603439
(sigma_est <- sd(brakes$V1))

## 2 [1] 0.3567222
mle_est <- (sqrt(mean((brakes$V1 - mean(brakes$V1))^2)))
## 2 - sposób 2
(mu_est <- mean(brakes$V1))
vari <- mean((brakes$V1 - mu_est)^2)
(mvue_est <- sqrt(vari))

## 3 [1] 0.3621869
# ZAPYTAJ O TO

#histogram
hist(brakes$V1, 
     xlab = "Braking distance", 
     main = "Empirical and theoretical distributions of braking distance",
     probability = TRUE)
lines(density(brakes$V1), col = "red", lwd = 2)
# Sigma estimated
curve(dnorm(x, mu_est, sigma_est), 
      add = TRUE, col = "blue", lwd = 2)
# MLE estimated
curve(dnorm(x, mu_est, mle_est), 
      add = TRUE, col = "green", lwd = 2)

legend("bottom", legend = c("empirical","S", "MLE", "MVUE"), col = c("red", "blue", "green", "pink"), lwd = 4)

#  Zadanie 2. 200 randomly selected 5-second time periods of work of a telephone exchange were examined.
# The number of calls was recorded. The results are contained in the file telephone_exchange.RData.
load("data/telephone_exchange.RData")

# 2.1 - sugestia - Prawdopodobnie rozkład Poissona - dane są rozłożone w określonym czasie,
# żadna z poprzednich próbek nie ma wpływu na następną, częstotliwość pobierania próbek jest znana

# 2.2 Calculate the values of the estimator of the model parameter. - [1] 1.74  
# Wyliczam wartość lambdy dla rozkładu Poissona
library(MASS)
parms <- fitdistr(telephone_exchange$number, "poisson")
(lamb <- parms$estimate)
(sd_x <- parms$sd )

# średnia też równa się 1.74 bo Var(x)=λ=E(x)
mean(telephone_exchange$number)

# 2.3 Compare the empirical probabilities of occurrence of individual values of the number of calls
# in the sample with theoretical values obtained on the basis of the theoretical distribution.
# [1] 0.9911019
(probs <- dpois(sort(unique(telephone_exchange$number)), lambda = lamb, log = F))
sum(probs)

##                     0         1         2         3          4          5
## empirical   0.1600000 0.3350000 0.2450000 0.1550000 0.07500000 0.03000000
## theoretical 0.1755204 0.3054055 0.2657028 0.1541076 0.06703681 0.02332881
counts <- matrix(c(prop.table(table(telephone_exchange$number)), probs), nrow = 2, byrow = TRUE)
rownames(counts) <- c("empirical", "theoretical")
colnames(counts) <- sort(unique(number_of_mistakes))
counts

barplot(counts, 
        xlab = "Number of calls", ylab = "Probability",
        main = "Empirical and theoretical distributions of number of calls",
        col = c("red", "blue"), legend = rownames(counts), beside = TRUE)

# 2.4 Check the goodness-of-fit of the theoretical distribution based on the Q-Q plot
qqplot(rpois(length(telephone_exchange$number), lambda = lamb), telephone_exchange$number)
qqline(telephone_exchange$number, distribution = function(probs) { qpois(probs, lambda = lamb) })

# 2.5 Based on the above considerations, does the theoretical distribution seem to be appropriate?
# ZAPYTAJ O TO

# 2.6 Calculate the empirical and theoretical probability that the number of calls is smaller than 4.
## [1] 0.895
(lamb <- mean(telephone_exchange$number < 4))
## [1] 0.9007363
parms <- fitdistr(telephone_exchange$number, "poisson")
(lamb <- parms$estimate)
(probs <- dpois(c(0,1,2,3), lambda = lamb, log = F))
sum(probs)

# 2.7 Calculate the confidence intervals for the model parameter based on three methods.
# LCL - Lower Controll Limit
# UCL - Upper Controll Limit
##      LCL      UCL 
## 1.561968 1.932765
  

##      LCL      UCL 
## 1.561968 1.932765


##      LCL      UCL 
## 1.557187 1.922813

# 3.0 The variable in the file failures.txt describes the results of 50 measurements of failure-free operation time of a given device (in hours).
# 3.1 Suggest the theoretical distribution of the examined variable.
# prawdopodobnie rozkład normalny
failures <- read.table("data/failures.txt", dec = ",")
failures

# 3.2 Calculate the values of the MLE of the model parameter.
## [1] 0.0009079683
# Zapytaj o to
curve(dnorm(x, mu_est, mle_est), 
      add = TRUE, col = "blue", lwd = 2)

# 3.3 Compare the empirical distribution of occurrence of individual values of the failure-free operation time in the sample with
#     theoretical distribution.
hist(failures$V1, 
     xlab = "Braking distance", 
     main = "Empirical distribution of failure-free operation time",
     probability = TRUE, 
     col = "lightgreen")
lines(density(failures$V1), col = "red", lwd = 2)
# TODO - theoretical distribution - ZAPYTAJ O TO

# 3.4 Check the goodness-of-fit of the theoretical distribution based on the Q-Q plot.
# 3.5 Based on the above considerations, does the theoretical distribution seem to be appropriate?
# 3.6 Calculate the empirical and theoretical probability that the failure-free operation time is contained in the interval [1000, 1500].
# 3.7 Calculate the confidence interval for the model parameter.
# 3.8 Calculate the maximum likelihood estimator values and the confidence interval limits for the expected 
#     value and variance of the theoretical distribution.
