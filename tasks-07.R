# 1.0 The table below presents the number of cases of respiratory tuberculosis in 1995-2002. 
#     The number of cases was given per 100,000 population. Assuming a linear relationship
#     between the year and the number of cases, make a comprehensive regression analysis.

# 1.1 Present the data on the scatter diagram. Does the linear regression model seem reasonable?
task1 <- data.frame(year = c(1995,1996,1997,1998,1999,2000,2001,2002), number_of_cases = c(39.7,38.2,34.7,33.1,30.1,28.4,26.3,24.7))
plot(task1, main = "Scatter diagram", pch = 16)
# 1.2 Fit the linear regression model to this data. What are the values of the regression coefficient 
#     estimators and confidence intervals? Draw the obtained regression line on the scatter diagram.
## (Intercept)        year 
## 4466.666667   -2.219048
coef(model)
year = task1$year
(model <- lm(task1$number_of_cases~year, data = task1))
##                  2.5 %      97.5 %
## (Intercept) 4066.82158 4866.511749
## year          -2.41912   -2.018975
confint(model)

plot(task1, main = "Scatter diagram", pch = 16)
abline(model, col = "red", lwd = 2)

# 1.3 Which coefficients are statistically significant in the constructed model? What is the model fit?
## Call:
## lm(formula = number_cases ~ year, data = data_set)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -0.69048 -0.26071 -0.00952  0.20952  0.75238 
## 
## Coefficients:
##               Estimate Std. Error t value Pr(>|t|)    
## (Intercept) 4466.66667  163.40805   27.33 1.58e-07 ***
## year          -2.21905    0.08177  -27.14 1.65e-07 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 0.5299 on 6 degrees of freedom
## Multiple R-squared:  0.9919, Adjusted R-squared:  0.9906 
## F-statistic: 736.5 on 1 and 6 DF,  p-value: 1.654e-07
summary(model)

# 1.4 Calculate fitted values by the model as well as residual values.
##        1        2        3        4        5        6        7        8 
## 39.66667 37.44762 35.22857 33.00952 30.79048 28.57143 26.35238 24.13333
# value fitted by model
fitted(model)

##           1           2           3           4           5           6 
##  0.03333333  0.75238095 -0.52857143  0.09047619 -0.69047619 -0.17142857 
##           7           8 
## -0.05238095  0.56666667
# residuals
residuals(model)

# 1.5 ent on the scatter diagram the limits of the 95% prediction interval.
temp_task <- data.frame(year = seq(min(task1$year), 
                                         max(task1$year)))
pred <- predict(model, temp_task, interval = "prediction")

plot(task1, main = "Scatter diagram", pch = 16)
abline(model, col = "red", lwd = 2)
lines(task1$year, pred[, 2], lty = 2, col = "red")
lines(task1$year, pred[, 3], lty = 2, col = "red")

# 1.6 Predict the number of cases of respiratory tuberculosis for 2003-2007.
#     Illustrate the results on scatter diagram.

##        fit      lwr      upr
## 1 21.91429 20.27052 23.55805
## 2 19.69524 17.93392 21.45656
## 3 17.47619 15.58342 19.36896
## 4 15.25714 13.22171 17.29258
## 5 13.03810 10.85098 15.22521
new_task <- data.frame(year = seq(2003, 2007, 1))
pred2 <- predict(model, new_task, interval = 'prediction')

task12 <- data.frame(year = c(task1$year, seq(2003,2007,1)),num = c(task1$number_of_cases, pred2[,"fit"]))

pred3 <- predict(model, task12, interval = 'prediction')

plot(task12, main = "Scatter diagram", pch = 16)
abline(model, col = "red", lwd = 2)
lines(task12$year, pred3[, 2], lty = 2, col = "green")
lines(task12$year, pred3[, 3], lty = 2, col = "green")

# 1.7 Would it make sense to remove the intercept from the model? If so, 
#     follow the instructions above for a no-intercept linear regression model.
# ZAPYTAJ O TO

# 2.0 The data set in the file braking.RData contains information on the 
#     braking distance at a given speed of a certain car model. There is an outlier 
#     in this data set. Identify it using a scatter diagram. Using a linear
#     regression model, describe the relationship between braking distance and 
#     speed using full data and data without outlier. What are the results for 
#     both models? Which model is better? More precisely, follow the same points 
#     2-7 as in Task 1 for each model separately. In point 6, predict braking distance 
#     for speed 30, 31, …, 50.
load("data/braking.RData")
attach(braking)
braking
plot(braking, main="Scatter diagram", pch = 16)

# 2.2 Model for full data set
speed <- braking$speed
distance <- braking$distance
(modelB <- lm(distance~speed, data = braking))
abline(modelB, col = "red", lwd = 2)

## (Intercept)       speed 
##  -22.726854    4.422338
coef(modelB)

##                  2.5 %    97.5 %
## (Intercept) -43.105778 -2.347930
## speed         3.177543  5.667134
confint(modelB)

# 2.3
## Call:
## lm(formula = distance ~ speed, data = braking)
## 
## Residuals:
##     Min      1Q  Median      3Q     Max 
## -33.720 -13.298  -3.186   7.814 119.858 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)    
## (Intercept) -22.7269    10.1409  -2.241   0.0296 *  
## speed         4.4223     0.6194   7.139 4.04e-09 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 23.18 on 49 degrees of freedom
## Multiple R-squared:  0.5099, Adjusted R-squared:  0.4999 
## F-statistic: 50.97 on 1 and 49 DF,  p-value: 4.037e-09
summary(modelB)

# 2.4 
##         1         2         3         4         5         6         7         8 
## -5.037501 -5.037501  8.229514  8.229514 12.651852 17.074190 21.496528 21.496528 
##         9        10        11        12        13        14        15        16 
## 21.496528 25.918867 25.918867 30.341205 30.341205 30.341205 30.341205 34.763543 
##        17        18        19        20        21        22        23        24 
## 34.763543 34.763543 34.763543 39.185881 39.185881 39.185881 39.185881 43.608220 
##        25        26        27        28        29        30        31        32 
## 43.608220 43.608220 70.142249 48.030558 48.030558 52.452896 52.452896 52.452896 
##        33        34        35        36        37        38        39        40 
## 56.875234 56.875234 56.875234 56.875234 61.297573 61.297573 61.297573 65.719911 
##        41        42        43        44        45        46        47        48 
## 65.719911 65.719911 65.719911 65.719911 74.564587 78.986926 83.409264 83.409264 
##        49        50        51 
## 83.409264 83.409264 87.831602
fitted(modelB)

##           1           2           3           4           5           6 
##   7.0375010  15.0375010  -4.2295137  13.7704863   3.3481480  -7.0741902 
##           7           8           9          10          11          12 
##  -3.4965285   4.5034715  12.5034715  -8.9188667   2.0811333 -16.3412050 
##          13          14          15          16          17          18 
## -10.3412050  -6.3412050  -2.3412050  -8.7635432  -0.7635432  -0.7635432 
##          19          20          21          22          23          24 
##  11.2364568 -13.1858815  -3.1858815  20.8141185  40.8141185 -23.6082197 
##          25          26          27          28          29          30 
## -17.6082197  10.3917803 119.8577508 -16.0305580  -8.0305580 -20.4528962 
##          31          32          33          34          35          36 
## -12.4528962  -2.4528962 -14.8752345  -0.8752345  19.1247655  27.1247655 
##          37          38          39          40          41          42 
## -25.2975727 -15.2975727   6.7024273 -33.7199110 -17.7199110 -13.7199110 
##          43          44          45          46          47          48 
##  -9.7199110  -1.7199110  -8.5645875 -24.9869257 -13.4092640   8.5907360 
##          49          50          51 
##   9.5907360  36.5907360  -2.8316022
residuals(modelB)

# 2.5
plot(braking, main="Scatter diagram", pch = 16)
(modelB <- lm(distance~speed, data = braking))
abline(modelB, col = "red", lwd = 2)

temp_dist <- data.frame(speed = seq(min(speed) - 50, 
                                         max(speed) + 50, 
                                         length = 51))
predDist <- predict(modelB, temp_dist, interval = 'prediction')

lines(temp_dist$speed, predDist[, 2], lty = 2, col = "green")
lines(temp_dist$speed, predDist[, 3], lty = 2, col = "green")

# 2.6
##         fit       lwr      upr
## 1  109.9433  59.56096 160.3256
## 2  114.3656  63.52436 165.2069
## 3  118.7880  67.46167 170.1143
## 4  123.2103  71.37362 175.0470
## 5  127.6326  75.26095 180.0043
## 6  132.0550  79.12441 184.9856
## 7  136.4773  82.96475 189.9899
## 8  140.8997  86.78270 195.0166
## 9  145.3220  90.57902 200.0650
## 10 149.7443  94.35444 205.1342
## 11 154.1667  98.10968 210.2237
## 12 158.5890 101.84545 215.3326
## 13 163.0114 105.56245 220.4603
## 14 167.4337 109.26136 225.6060
## 15 171.8560 112.94285 230.7692
## 16 176.2784 116.60757 235.9492
## 17 180.7007 120.25614 241.1453
## 18 185.1230 123.88919 246.3569
## 19 189.5454 127.50730 251.5835
## 20 193.9677 131.11105 256.8244
## 21 198.3901 134.70099 262.0791
