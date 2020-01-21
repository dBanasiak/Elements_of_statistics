# 1.1. This task was developed on the basis of experiment in Smith (1979). His main goal was to show that being 
#    in the same mental context during learning and during its checking (test, exam) gives better results than
#    being in different contexts. During learning phase, students learned 80 words in a room painted orange, 
#    decorated with posters, paintings and lots of additional accessories. The first memory test was conducted 
#    to give students the impression that the experiment was over. The next day, the students were unexpectedly 
#    tested again. They had to write all the words they remembered. The test was carried out under 5 different 
#    conditions. 50 students were randomly divided into 5 groups:
  
#    - “Same context” - the test took place in the same room in which they studied.
#    - “Different context” - the test took place in a very different room, in another part of the campus, 
#       painted gray and looking very harsh.
#    - “Imaginary context” - the test took place in the same room as in the previous point. In addition,
#       students were to recall the room in which they were studying. To help them in this, 
#       the researcher asked additional questions about the room and its equipment.
#    - “Photographed context” - the test took place under the same conditions as in the previous point. 
#       In addition, they were shown a picture of the room they were studying in.
#    - “Placebo context” - the test took place under the same conditions as in “Different context”. 
#       In addition, students did “warming up” exercises (recalling their living room).
#       The number of remembered words is contained in the following table.
(data_table_1 <- data.frame(name = c(rep('same', 10),
                              rep('different', 10),
                              rep('imagery', 10),
                              rep('photo', 10),
                              rep('placebo', 10)),
                            value = c(25,26,17,15,14,17,14,20,11,21,
                                      11,21,9,6,7,14,12,4,7,19,
                                      14,15,29,10,12,22,14,20,22,12,
                                      25,15,23,21,18,24,14,27,12,11,
                                      8,20,10,7,15,7,1,17,11,4)))
summary(data_table_1)
##     CONTEXT  x
## 1 Different 11
## 2   Imagery 17
## 3     Photo 19
## 4   Placebo 10
## 5      Same 18
(aggData <- aggregate(data_table_1$value, 
          list(Context = data_table_1$name), 
          FUN = mean))

boxplot(data_table_1$value~data_table_1$name, data_table_1)

# 1.2 Perform an analysis of variance test to see if the number of words remembered depends 
#     on the context of the knowledge checking.

##             Df Sum Sq Mean Sq F value  Pr(>F)   
## context      4    700     175   5.469 0.00112 **
## Residuals   45   1440      32                   
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
summary(aov(data_table_1$value ~ data_table_1$name, data = data_table_1))

# 1.3 Check the assumptions of the one-way analysis of variance model.
qqnorm(data_table_1$value, pch=1, frame = F, col="red")
qqline(data_table_1$value, lwd=2)
# SHAPIRO WILK - normality of random errors
## [1] 0.05635956
(pValue <- shapiro.test(lm(data_table_1$value ~ data_table_1$name, data = data_table_1)$residuals)$p.value)
## [1] 0.9817694
bartlett.test(data_table_1$value ~ data_table_1$name, data = data_table_1)$p.value
## [1] 0.9759731
fligner.test(data_table_1$value ~ data_table_1$name, data = data_table_1)$p.value
## [1] 0.9550502
library(car)
leveneTest(data_table_1$value ~ data_table_1$name, data = data_table_1)$`Pr(>F)`
## [1] 0.9281122
leveneTest(data_table_1$value ~ data_table_1$name, data = data_table_1, center = "mean")$`Pr(>F)`

# 1.4 Perform post hoc tests to see which knowledge checking contexts are different.
## 
##  Pairwise comparisons using t tests with pooled SD 
## 
## data:  number and context 
## 
##         Different Imagery Photo Placebo
## Imagery 0.110     -       -     -      
## Photo   0.025     1.000   -     -      
## Placebo 1.000     0.057   0.009 -      
## Same    0.057     1.000   1.000 0.025  
## 
## P value adjustment method: holm
pairwise.t.test(data_table_1$value, data_table_1$name, data = data_table_1)

##                   diff        lwr       upr      p adj
## Imagery-Different    6  -1.188363 13.188363 0.14198584
## Photo-Different      8   0.811637 15.188363 0.02232998
## Placebo-Different   -1  -8.188363  6.188363 0.99466042
## Same-Different       7  -0.188363 14.188363 0.05967870
## Photo-Imagery        2  -5.188363  9.188363 0.93203553
## Placebo-Imagery     -7 -14.188363  0.188363 0.05967870
## Same-Imagery         1  -6.188363  8.188363 0.99466042
## Placebo-Photo       -9 -16.188363 -1.811637 0.00759672
## Same-Photo          -1  -8.188363  6.188363 0.99466042
## Same-Placebo         8   0.811637 15.188363 0.02232998
(model_aov <- aov(data_table_1$value ~ data_table_1$name, data = data_table_1))
TukeyHSD(model_aov)
plot(TukeyHSD(model_aov))
## 
## Study: model_aov ~ "context"
## 
## HSD Test for number 
## 
## Mean Square Error:  32 
## 
## context,  means
## 
##           number      std  r Min Max
## Different     11 5.617433 10   4  21
## Imagery       17 6.000000 10  10  29
## Photo         19 5.773503 10  11  27
## Placebo       10 5.906682 10   1  20
## Same          18 4.921608 10  11  26
## 
## Alpha: 0.05 ; DF Error: 45 
## Critical Value of Studentized Range: 4.018417 
## 
## Minimun Significant Difference: 7.188363 
## 
## Treatments with the same letter are not significantly different.
## 
##           number groups
## Photo         19      a
## Same          18     ab
## Imagery       17    abc
## Different     11     bc
## Placebo       10      c
library(agricolae)
hsd <- HSD.test(model_aov, "data_table_1$name", console = TRUE)

## 
## Study: model_aov ~ "context"
## 
## Student Newman Keuls Test
## for number 
## 
## Mean Square Error:  32 
## 
## context,  means
## 
##           number      std  r Min Max
## Different     11 5.617433 10   4  21
## Imagery       17 6.000000 10  10  29
## Photo         19 5.773503 10  11  27
## Placebo       10 5.906682 10   1  20
## Same          18 4.921608 10  11  26
## 
## Alpha: 0.05 ; DF Error: 45 
## 
## Critical Range
##        2        3        4        5 
## 5.095323 6.131311 6.748805 7.188363 
## 
## Means with the same letter are not significantly different.
## 
##           number groups
## Photo         19      a
## Same          18      a
## Imagery       17      a
## Different     11      b
## Placebo       10      b
SNK.test(model_aov, "data_table_1$name", console = TRUE)

## 
## Study: model_aov ~ "context"
## 
## LSD t Test for number 
## P value adjustment method: holm 
## 
## Mean Square Error:  32 
## 
## context,  means and individual ( 95 %) CI
## 
##           number      std  r       LCL      UCL Min Max
## Different     11 5.617433 10  7.397062 14.60294   4  21
## Imagery       17 6.000000 10 13.397062 20.60294  10  29
## Photo         19 5.773503 10 15.397062 22.60294  11  27
## Placebo       10 5.906682 10  6.397062 13.60294   1  20
## Same          18 4.921608 10 14.397062 21.60294  11  26
## 
## Alpha: 0.05 ; DF Error: 45
## Critical Value of t: 2.952079 
## 
## Minimum Significant Difference: 7.468235 
## 
## Treatments with the same letter are not significantly different.
## 
##           number groups
## Photo         19      a
## Same          18     ab
## Imagery       17    abc
## Different     11     bc
## Placebo       10      c
LSD.test(model_aov, "data_table_1$name", p.adj = "holm", console = TRUE)

## 
## Study: model_aov ~ "context"
## 
## Scheffe Test for number 
## 
## Mean Square Error  : 32 
## 
## context,  means
## 
##           number      std  r Min Max
## Different     11 5.617433 10   4  21
## Imagery       17 6.000000 10  10  29
## Photo         19 5.773503 10  11  27
## Placebo       10 5.906682 10   1  20
## Same          18 4.921608 10  11  26
## 
## Alpha: 0.05 ; DF Error: 45 
## Critical Value of F: 2.578739 
## 
## Minimum Significant Difference: 8.125006 
## 
## Means with the same letter are not significantly different.
## 
##           number groups
## Photo         19      a
## Same          18     ab
## Imagery       17     ab
## Different     11     ab
## Placebo       10      b
scheffe.test(model_aov, "data_table_1$name", console = TRUE)

# 1.5 We want to test the following specific hypotheses:

#     Groups with the same context when learning and testing (“Same” or “Imaginary” or 
#     “Photographed”) are better than groups with different contexts (“Different” or “Placebo”).
#     The “Same” group is different from the “Imaginary” and “Photographed” groups.
#     The “Imaginary” group is different from the “Photographed” group.
#     The “Different” group is different from the “Placebo” group.
#     To do this, follow these instructions:
  
#     ODPOWIEDZ NA TE PYTANIA
#     Write down the relevant hypotheses.
#     Express them using contrasts.
#     Is this contrast system orthogonal?
#     Test the contrasts proposed.
# ------------------------------------------------------------------------------------------------
##               Df Sum Sq Mean Sq F value   Pr(>F)    
## context        4    700     175   5.469  0.00112 ** 
##   context: C1  1    675     675  21.094 3.52e-05 ***
##   context: C2  1      0       0   0.000  1.00000    
##   context: C3  1     20      20   0.625  0.43334    
##   context: C4  1      5       5   0.156  0.69450    
## Residuals     45   1440      32                     
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
model2 <- aov(data_table_1$value ~ data_table_1$name, data = data_table_1)
summary(model2, 
        split = list('data_table_1$name' = list('C1' = 1, 'C2' = 2, 'C3' = 3, 'C4' = 4)))

# 1.6 Solve tasks 1, 2 and 4 using appropriate non-parametric methods.
#     Compare the results with parametric approach.

##     CONTEXT    x
## 1 Different 10.0
## 2   Imagery 14.5
## 3     Photo 19.5
## 4   Placebo  9.0
## 5      Same 17.0
(aggData <- aggregate(data_table_1$value, 
                      list(Context = data_table_1$name), 
                      FUN = median))

## [1] 0.002603633
kruskal.test(data_table_1$value ~ data_table_1$name, data = data_table_1)$p.value

## 
##  Pairwise comparisons using Wilcoxon rank sum test 
## 
## data:  number and context 
## 
##         Different Imagery Photo Placebo
## Imagery 0.138     -       -     -      
## Photo   0.081     1.000   -     -      
## Placebo 1.000     0.138   0.057 -      
## Same    0.096     1.000   1.000 0.081  
## 
## P value adjustment method: holm

# post hoc test using Mann-Whitney test for two samples
pairwise.wilcox.test(data_table_1$value, data_table_1$name, data = data_table_1)

##             Comparison          Z     P.unadj      P.adj
## 1  Different - Imagery -2.0753448 0.037954590 0.06325765
## 2    Different - Photo -2.8055587 0.005022943 0.02511471
## 3      Imagery - Photo -0.7302139 0.465259440 0.66465634
## 4  Different - Placebo  0.1844751 0.853640764 0.85364076
## 5    Imagery - Placebo  2.2598199 0.023832431 0.04766486
## 6      Photo - Placebo  2.9900338 0.002789466 0.02789466
## 7     Different - Same -2.5288461 0.011443820 0.02860955
## 8       Imagery - Same -0.4535013 0.650187828 0.81273479
## 9         Photo - Same  0.2767126 0.782000765 0.86888974
## 10      Placebo - Same -2.7133212 0.006661251 0.02220417

# Dunn test
library(FSA)
dunnTest(data_table_1$value ~ data_table_1$name, data = data_table_1, method = "bh")

# 2.0 TODO