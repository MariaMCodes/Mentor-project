## One-way ANOVA - only have 10% TSB

## Needed to install Command Line Tools for Xcode 11.5 from 
## Apple Developer Website https://developer.apple.com/download/more/
## for the below packages to work

library(tidyverse)
library(tidyselect)
library(ggpubr)
library(rstatix)
library(ggplot2)

sa <- read.csv("data/Saureus.csv")
sa

## Summary statistics
sa %>% 
  group_by(Implant.type) %>%
  get_summary_stats(Log10.CFU.per.ml, type = "mean_sd")

## Visualization
## Create a box plot of Log10 CFU by TSB coloured by Implant.type
bxp <- ggboxplot(sa, x = "Implant.type", y = "Log10.CFU.per.ml", palette = "jco")
bxp


## Check assumptions
## Outliers
sa %>%
  group_by(Implant.type) %>%
  identify_outliers(Log10.CFU.per.ml)

## [1] Implant.type     TSB              Log10.CFU.per.ml
## [4] is.outlier       is.extreme      
## <0 rows> (or 0-length row.names)

## From above there are no extreme outliers


## Normality assumption
## Build the linear model
lmodel <- lm(Log10.CFU.per.ml ~ Implant.type, data = sa)
## Create a QQ plot of residuals
ggqqplot(residuals(lmodel))
## Compute Shapiro-Wilk test of normality using rstatix
shapiro_test(residuals(lmodel))
## A tibble: 1 x 3
## variable            statistic p.value
## <chr>                   <dbl>   <dbl>
## 1 residuals(lmodel)     0.954   0.411

## In the qqplot, as all the points fall approximately along the reference line, we can assume
## normality. This is supported by Shapiro-Wilk test. The p-value is not significant (p=0.411), 
## so we can assume normality


## Check normality assumption by groups
sa %>%
  group_by(Implant.type) %>%
  shapiro_test(Log10.CFU.per.ml)

## The score were normally distributed (p>0.5) for each cell, as assessed by the Shapiro-Wilk test 

## Create QQ plots for each cell of design
ggqqplot(sa, "Log10.CFU.per.ml", ggtheme = theme_classic()) + facet_grid(Implant.type ~ TSB)
## All the points fall approximately along the reference line, for each cell, so we can assume normality


## Homogeneity of variance assumption
## Checked using the Levene's test using rstatix
sa %>% levene_test(Log10.CFU.per.ml ~ Implant.type * TSB)
## BUT.. Levene's test is not appropriate with quantitative explanatory variables
## If Levene's test is not significant (p>0.05), then can assume the homogeneity (they're the same) of 
## variances in the different groups


## Computation
## Interaction and main effect of each variable
res.aov <- sa %>% anova_test(Log10.CFU.per.ml ~ Implant.type)
res.aov

## ANOVA Table (type II tests)

##        Effect  DFn DFd     F        p p<.05   ges
## 1 Implant.type   6  14 93.94 1.69e-10     * 0.976

## There are significant differences between implant types, which is highlighted with '*",  
## F(6,14) = 93.94, p < 0.0001


## Post-hoc tests
## A significant one-way ANOVA is generally followed up by Tukey post-hoc tests to
## perform multiple pairwise comparisons between groups. 
## Pairwise comparisons
pwc <- sa %>% tukey_hsd(Log10.CFU.per.ml ~ Implant.type)
pwc

pwc[1:11,]
## A tibble: 11 x 8
##    term         group1 group2 estimate conf.low conf.high        p.adj p.adj.signif
##    <chr>        <chr>  <chr>     <dbl>    <dbl>     <dbl>        <dbl> <chr>       
##  1 Implant.type A      B        -0.437  -0.832    -0.0416 0.0262       *           
##  2 Implant.type A      C        -1.5    -1.90     -1.10   0.0000000593 ****        
##  3 Implant.type A      D        -1.48   -1.88     -1.08   0.0000000706 ****        
##  4 Implant.type A      E         0.323  -0.0717    0.718  0.145        ns          
##  5 Implant.type A      F         0.433   0.0383    0.828  0.0276       *           
##  6 Implant.type A      G        -0.263  -0.658     0.132  0.319        ns          
##  7 Implant.type B      C        -1.06   -1.46     -0.668  0.00000438   ****        
##  8 Implant.type B      D        -1.04   -1.44     -0.648  0.0000055    ****        
##  9 Implant.type B      E         0.760   0.365     1.16   0.000198     ***         
## 10 Implant.type B      F         0.87    0.475     1.27   0.0000453    ****        
## 11 Implant.type B      G         0.173  -0.222     0.568  0.742        ns 

pwc[12:21,]
## A tibble: 10 x 8
##    term         group1 group2 estimate conf.low conf.high         p.adj p.adj.signif
##    <chr>        <chr>  <chr>     <dbl>    <dbl>     <dbl>         <dbl> <chr>       
##  1 Implant.type C      D         0.02    -0.375     0.415 1             ns          
##  2 Implant.type C      E         1.82     1.43      2.22  0.00000000437 ****        
##  3 Implant.type C      F         1.93     1.54      2.33  0.000000002   ****        
##  4 Implant.type C      G         1.24     0.842     1.63  0.000000688   ****        
##  5 Implant.type D      E         1.80     1.41      2.20  0.00000000508 ****        
##  6 Implant.type D      F         1.91     1.52      2.31  0.00000000229 ****        
##  7 Implant.type D      G         1.22     0.822     1.61  0.000000843   ****        
##  8 Implant.type E      F         0.11    -0.285     0.505 0.957         ns          
##  9 Implant.type E      G        -0.587   -0.982    -0.192 0.0025        **          
## 10 Implant.type F      G        -0.697   -1.09     -0.302 0.000485      *** 


