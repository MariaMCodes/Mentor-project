## Two-way ANOVA

library(tidyverse)
library(ggplot2)
library(dplyr)

rp <- read.csv("data/Rpickettii.csv")
rp

rp$TSB = as.factor(rp$TSB)

anova <- aov(Log10.CFU.per.ml ~ Implant.type + TSB, data = rp)
summary(anova)

## Two-way ANOVA output
##               Df Sum Sq Mean Sq F value   Pr(>F)    
##  Implant.type  6 16.457  2.7429 2067.82  < 2e-16 ***
##  TSB           1  0.098  0.0981   73.97 4.77e-10 ***
##  Residuals    34  0.045  0.0013                     
##  ---
##  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## From the ANOVA table we can conclude that both Implant type and TSB are statistically significant
## Both have significant influence on log10 CFU concentration


## Two-way ANOVA with interaction effect
## These two calls are equivalent
anova2 <- aov(Log10.CFU.per.ml ~ Implant.type * TSB, data = rp)
anova2 <- aov(Log10.CFU.per.ml ~ Implant.type + TSB + Implant.type:TSB, data = rp)
summary(anova2)

## Output
##                  Df Sum Sq Mean Sq F value   Pr(>F)    
## Implant.type      6 16.457  2.7429 4056.40  < 2e-16 ***
## TSB               1  0.098  0.0981  145.10 1.36e-12 ***
## Implant.type:TSB  6  0.026  0.0044    6.45 0.000235 *** 
## Residuals        28  0.019  0.0007                     
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

## From output can see that the two main effects (implant type and TSB) are statistically significant, 
## as well as their interaction


## Multiple pairwise-comparison between the means of groups
## Tukey multiple pairwise-comparisons
TukeyHSD(anova2, which = "Implant.type")

## Output
## Tukey multiple comparisons of means
## 95% family-wise confidence level

## Fit: aov(formula = Log10.CFU.per.ml ~ Implant.type + TSB + Implant.type:TSB, data = rp)

## $Implant.type
##            diff           lwr          upr     p adj
## B-A -0.10500000 -0.1526238922 -0.057376108 0.0000026
## C-A -0.23833333 -0.2859572255 -0.190709441 0.0000000
## D-A -0.16166667 -0.2092905588 -0.114042775 0.0000000
## E-A  1.26166667  1.2140427745  1.309290559 0.0000000
## F-A  1.30833333  1.2607094412  1.355957225 0.0000000
## G-A  0.08833333  0.0407094412  0.135957225 0.0000471
## C-B -0.13333333 -0.1809572255 -0.085709441 0.0000000
## D-B -0.05666667 -0.1042905588 -0.009042775 0.0119878
## E-B  1.36666667  1.3190427745  1.414290559 0.0000000
## F-B  1.41333333  1.3657094412  1.460957225 0.0000000
## G-B  0.19333333  0.1457094412  0.240957225 0.0000000
## D-C  0.07666667  0.0290427745  0.124290559 0.0003745
## E-C  1.50000000  1.4523761078  1.547623892 0.0000000
## F-C  1.54666667  1.4990427745  1.594290559 0.0000000
## G-C  0.32666667  0.2790427745  0.374290559 0.0000000
## E-D  1.42333333  1.3757094412  1.470957225 0.0000000
## F-D  1.47000000  1.4223761078  1.517623892 0.0000000
## G-D  0.25000000  0.2023761078  0.297623892 0.0000000
## F-E  0.04666667 -0.0009572255  0.094290559 0.0576307
## G-E -1.17333333 -1.2209572255 -1.125709441 0.0000000
## G-F -1.22000000 -1.2676238922 -1.172376108 0.0000000





TukeyHSD(anova2, which = "TSB")   ## needed to convert TSB to a factor variable, see above

## Output
## Tukey multiple comparisons of means
## 95% family-wise confidence level

## Fit: aov(formula = Log10.CFU.per.ml ~ Implant.type + TSB + Implant.type:TSB, data = rp)

## $TSB
##             diff        lwr       upr p adj
##  10-5 0.09666667 0.08022839 0.1131049     0


## USing agricolae package
install.packages("agricolae")
library(agricolae)

rp_lm <- lm(Log10.CFU.per.ml ~ Implant.type + TSB, data = rp)
rp_anova <- aov(rp_lm)
summary(rp_anova)

## Output
##                Df Sum Sq Mean Sq F value   Pr(>F)    
## Implant.type    6 16.457  2.7429 2067.82  < 2e-16 ***
## TSB             1  0.098  0.0981   73.97 4.77e-10 ***
## Residuals      34  0.045  0.0013                     
## ---
## Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

tukey.test <- TukeyHSD(rp_anova)
tukey.test

## Output
## Tukey multiple comparisons of means
## 95% family-wise confidence level

## Fit: aov(formula = rp_lm)

##$Implant.type
##            diff         lwr         upr     p adj
## B-A -0.10500000 -0.17084417 -0.03915583 0.0003248
## C-A -0.23833333 -0.30417750 -0.17248917 0.0000000
## D-A -0.16166667 -0.22751083 -0.09582250 0.0000001
## E-A  1.26166667  1.19582250  1.32751083 0.0000000
## F-A  1.30833333  1.24248917  1.37417750 0.0000000
## G-A  0.08833333  0.02248917  0.15417750 0.0031300
## C-B -0.13333333 -0.19917750 -0.06748917 0.0000061
## D-B -0.05666667 -0.12251083  0.00917750 0.1304991
## E-B  1.36666667  1.30082250  1.43251083 0.0000000
## F-B  1.41333333  1.34748917  1.47917750 0.0000000
## G-B  0.19333333  0.12748917  0.25917750 0.0000000
## D-C  0.07666667  0.01082250  0.14251083 0.0139687
## E-C  1.50000000  1.43415583  1.56584417 0.0000000
## F-C  1.54666667  1.48082250  1.61251083 0.0000000
## G-C  0.32666667  0.26082250  0.39251083 0.0000000
## E-D  1.42333333  1.35748917  1.48917750 0.0000000
## F-D  1.47000000  1.40415583  1.53584417 0.0000000
## G-D  0.25000000  0.18415583  0.31584417 0.0000000
## F-E  0.04666667 -0.01917750  0.11251083 0.3121983
## G-E -1.17333333 -1.23917750 -1.10748917 0.0000000
## G-F -1.22000000 -1.28584417 -1.15415583 0.0000000

## $TSB
##            diff        lwr       upr p adj
## 10-5 0.09666667 0.07382488 0.1195085     0
