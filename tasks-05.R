# 1.  In a certain region, ten independent measurements of sea depth were made, and the following 
#    results were obtained: 862, 870, 876, 866, 871, 865, 861, 873, 871, 872. At the significance level of  
#    α = 0.05, verify the hypothesis that the average sea depth is 870m in this region.
(data_first <- c(862, 870, 876, 866, 871, 865, 861, 873, 871, 872))
## [1] 0.545861
test <- shapiro.test(data_first)
test$p.value

qqnorm(data_first, pch = 1, frame = FALSE, col="red")
qqline(data_first, lwd = 2)

## [1] 868.7
values <- t.test(data_first, mu = 870, alternative = "less")
values$estimate
## [1] 0.2136555
values$p.value

# 2. Manufacturer of washing powder A claims that its product is significantly better than competitive powder B. 
#    To verify this assurance, the CTA (Consumer Test Agency) tested both washing powders. For this purpose, the measurements
#    of the washing degree of 7 pieces of fabric with powder A were carried out to obtain the results (in %):
#   78.2, 78.5, 75.6, 78.5, 78.5, 77.4, 76.6,
#   and 10 pieces of fabric with powder B to obtain the results (in %):
#   76.1, 75.2, 75.8, 77.3, 77.3, 77.0, 74.4, 76.2, 73.5, 77.4.
#   What should the CTA’s conclusion about the quality of these powders be?
powderA <- c(78.2, 78.5, 75.6, 78.5, 78.5, 77.4, 76.6)
powderB <- c(76.1, 75.2, 75.8, 77.3, 77.3, 77.0, 74.4, 76.2, 73.5, 77.4)
boxplot(powderA, powderB)
## [1] 0.06832755
shapiro.test(powderA)$p.value
## [1] 0.2558752
shapiro.test(powderB)$p.value
#QQPlot dla PowderA
qqnorm(powderA, pch=1, frame = F, col="red")
qqline(powderA, lwd=2)
#QQPlot dla PowderB
qqnorm(powderB, pch=1, frame = F, col="red")
qqline(powderB, lwd=2)
## [1] 1.304762
(var(powderA))
## [1] 1.764
(var(powderB))
## [1] 0.3683809
var.test(powderA, powderB, alternative = "less", var.equal=F)$p.value
## [1] 77.61429
(mean(powderA))
## [1] 76.02
(mean(powderB))
## [1] 0.01059375
t.test(powderA, powderB, var.equal = TRUE, alternative = 'greater')$p.value

# 3. A group of 10 people was subjected to a study aimed at examining the attitude towards public schools. 
#    Then the group watched an educational film aimed at improving the attitude towards this type of school.
#    The results are as follows (higher value means better attitude):
#    
#    before: 84, 87, 87, 90, 90, 90, 90, 93, 93, 96,
#    after: 89, 92, 98, 95, 95, 92, 95, 92, 98, 101.
#
#    Investigate if the film significantly improves attitude towards public schools.
before <- c(84, 87, 87, 90, 90, 90, 90, 93, 93, 96)
after <- c(89, 92, 98, 95, 95, 92, 95, 92, 98, 101)
boxplot(before, after)
## [1] 0.7025892
shapiro.test(before)$p.value
qqnorm(before, pch=1, frame = F, col="red")
qqline(before, lwd=2)
## [1] 0.691489
shapiro.test(after)$p.value
qqnorm(after, pch=1, frame = F, col="red")
qqline(after, lwd=2)
## [1] 90
mean(before)
## [1] 94.7
mean(after)
## [1] 0.0003786878
# ZAPYTAJ O TO

# 4. The height of 13 men and 12 women in a sports center was examined. The results are as follows:
#    men: 171, 176, 179, 189, 176, 182, 173, 179, 184, 186, 189, 167, 177,
#    women: 161, 162, 163, 162, 166, 164, 168, 165, 168, 157, 161, 172.
#    Can we say that the average height of men is significantly greater than that of women?
men <- c(171, 176, 179, 189, 176, 182, 173, 179, 184, 186, 189, 167, 177)
women <- c(161, 162, 163, 162, 166, 164, 168, 165, 168, 157, 161, 172)
boxplot(men, women)
## [1] 0.8595396
shapiro.test(men)$p.value
qqnorm(men, pch=1, frame = F, col="red")
qqline(men, lwd=2)
## [1] 0.9447828
shapiro.test(women)$p.value
qqnorm(women, pch=1, frame = F, col="red")
qqline(women, lwd=2)
## [1] 45.74359
var(men)
## [1] 16.08333
var(women)
## [1] 0.04689163
var.test(men, women, alternative = "greater")$p.value
## [1] 179.0769
mean(men)
## [1] 164.0833
mean(women)
## [1] 6.928802e-07
t.test(men, women, alternative = 'greater')$p.value

# 5.  Use the appropriate F and t-Student tests to solve the tasks 1-4, but now use their critical regions instead of p-values.
## 1.
##          t 
## -0.8312939
mean(data_first)
(t_data <- t.test(data_first, mu=870)$statistic)
## [1] -1.833113

## 2.
##         F 
## 0.7396609
(powder_var <- var.test(powderA, powderB, alternative = "less", var.equal=F)$estimate)
## [1] 0.243961

##        t 
## 2.573477
t.test(powderA, powderB, var.equal = TRUE, alternative = 'greater')$statistic
## [1] 1.75305

## 3.
##        t 
## -4.98199
## [1] -1.833113

## 4.
##        F 
## 2.844161
(menWomen_var <- var.test(men, women, alternative = "greater")$estimate)
## [1] 2.787569

##        t 
## 6.801919
t.test(men, women, alternative = 'greater')$statistic
## [1] 1.725753
