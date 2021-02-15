## Two-way ANOVA

## Needed to install Command Line Tools for Xcode 11.5 from 
## Apple Developer Website https://developer.apple.com/download/more/
## for the below packages to work
install.packages("tidyverse")
library(tidyverse)

install.packages("tidyselect")
library(tidyselect)

install.packages("ggpubr")
library(ggpubr)

install.packages("rstatix")
library(rstatix)

install.packages("ggplot2")
library(ggplot2)


rp <- read.csv("data/Rpickettii.csv")
rp

## Summary statistics
rp %>% 
  group_by(Implant.type, TSB) %>%
  get_summary_stats(Log10.CFU.per.ml, type = "mean_sd")


## Visualization
## Create a box plot of Log10 CFU by TSB coloured by Implant.type
bxp <- ggboxplot(rp, x = "TSB", y = "Log10.CFU.per.ml", color = "Implant.type", palette = "jco")
bxp


## Check assumptions
## Outliers
rp %>%
  group_by(Implant.type, TSB) %>%
  identify_outliers(Log10.CFU.per.ml)

## [1] TSB              Implant.type     Log10.CFU.per.ml
## [4] is.outlier       is.extreme      
## <0 rows> (or 0-length row.names)

## From above there are no extreme outliers

## Normality assumption
## Build the linear model
lmodel <- lm(Log10.CFU.per.ml ~ Implant.type * TSB, data = rp)
## Create a QQ plot of residuals
ggqqplot(residuals(lmodel))
## Compute Shapiro-Wilk test of normality using rstatix
shapiro_test(residuals(lmodel))
## A tibble: 1 x 3
## variable          statistic p.value
## <chr>                 <dbl>   <dbl>
## 1 residuals(lmodel)     0.975   0.495

## In the qqplot, as all the points fall approximately along the reference line, we can assume
## normality. This is supported by Shapiro-Wilk test. The p-value is not significant (p=0.495), 
## so we can assume normality

## Check normality assumption by groups
rp %>%
  group_by(Implant.type, TSB) %>%
  shapiro_test(Log10.CFU.per.ml)

## The score were normally distributed (p>0.5) for each cell, as assessed by the Shapiro-Wilk test 

## Create QQ plots for each cell of design
ggqqplot(rp, "Log10.CFU.per.ml", ggtheme = theme_classic()) + facet_grid(Implant.type ~ TSB)
## All the points fall approximately along the reference line, for each cell, so we can assume normality


## Homogeneity of variance assumption
## Checked using the Levene's test using rstatix
rp %>% levene_test(Log10.CFU.per.ml ~ Implant.type * TSB)
## BUT.. Levene's test is not appropriate with quantitative explanatory variables
## If Levene's test is not significant (p>0.05), then can assume the homogeneity (they're the same) of 
## variances in the different groups


## Computation
## Interaction and main effect of each variable
res.aov <- rp %>% anova_test(Log10.CFU.per.ml ~ Implant.type * TSB)
res.aov

## ANOVA Table (type II tests)

## Effect DFn DFd        F        p p<.05   ges
## 1     Implant.type   6  28 4056.400 8.38e-40     * 0.999
## 2              TSB   1  28  145.102 1.36e-12     * 0.838
## 3 Implant.type:TSB   6  28    6.450 2.35e-04     * 0.580

## There is a stastically significant interaction between implant type and TSB concentration for 
## Log10 CFU concentration, F(6,28) = 6.450, p = 0.000235


## Post-hoc tests
## This is procedure for significant two-way interaction
## Compute simple main effects
## Group data by TSB and fit anova
lmodel2 <- lm(Log10.CFU.per.ml ~ TSB * Implant.type, data = rp)
rp %>% 
  group_by(TSB) %>%
  anova_test(Log10.CFU.per.ml ~ Implant.type, error = lmodel2)

## A tibble: 2 x 8
## TSB Effect     DFn   DFd     F        p `p<.05`   ges
## <int> <chr>    <dbl> <dbl> <dbl>    <dbl> <chr>   <dbl>
## 1     5 Implant…     6    28 1906. 3.21e-35 *       0.998
## 2    10 Implant…     6    28 2157. 5.72e-36 *       0.998

## The simple main effect of implant type on Log10 CFU concentration was statistically
## significant for both 5% and 10% TSB, p < 0.0001.
## In other words, there is a statistically significant difference in mean Log10 CFU concentration
## between the various implant types grown in 5% TSB. The same holds true for 10% TSB. 


## Group data by Implant typ and fit anova
lmodel3 <- lm(Log10.CFU.per.ml ~ Implant.type * TSB, data = rp)
rp %>% 
  group_by(Implant.type) %>%
  anova_test(Log10.CFU.per.ml ~ TSB, error = lmodel3)

## A tibble: 7 x 8
## Implant.type Effect   DFn   DFd     F             p `p<.05`   ges
## <chr>        <chr>  <dbl> <dbl> <dbl>         <dbl> <chr>   <dbl>
## 1 A            TSB        1    28 30.2  0.00000718    "*"     0.519
## 2 B            TSB        1    28  4.83 0.036         "*"     0.147
## 3 C            TSB        1    28  3.55 0.07          ""      0.112
## 4 D            TSB        1    28  3.55 0.07          ""      0.112
## 5 E            TSB        1    28 35.6  0.00000201    "*"     0.56 
## 6 F            TSB        1    28 66.6  0.00000000691 "*"     0.704
## 7 G            TSB        1    28 39.4  0.000000865   "*"     0.585

## The simple main effect of implant type on Log10 CFU concentration was statistically
## significant for both implant types A, B, E, F, G (p < 0.05).
## In other words, there is a statistically significant difference in mean Log10 CFU concentration
## between implant A grown in 5% TSB and 10% TSB. The same holds true for B, E, F, G


## Compute pairwise comparisons
## Group by Implant type
install.packages("emmeans")
library(emmeans)
pwc1 <- rp %>%
  group_by(Implant.type) %>%
  emmeans_test(Log10.CFU.per.ml ~ TSB, p.adjust.method = "bonferroni")
pwc1
print(pwc1)
## A tibble: 7 x 9
## Implant.type .y.             group1 group2    df statistic            p       p.adj p.adj.signif
## * <fct>        <chr>           <chr>  <chr>  <dbl>     <dbl>        <dbl>       <dbl> <chr>       
## 1 A            Log10.CFU.per.… 5      10        28     -5.49      7.18e-6     7.18e-6 ****        
## 2 B            Log10.CFU.per.… 5      10        28     -2.20      3.64e-2     3.64e-2 *           
## 3 C            Log10.CFU.per.… 5      10        28     -1.88      7.00e-2     7.00e-2 ns          
## 4 D            Log10.CFU.per.… 5      10        28     -1.88      7.00e-2     7.00e-2 ns          
## 5 E            Log10.CFU.per.… 5      10        28     -5.97      2.01e-6     2.01e-6 ****        
## 6 F            Log10.CFU.per.… 5      10        28     -8.16      6.91e-9     6.91e-9 ****        ##
## 7 G            Log10.CFU.per.… 5      10        28     -6.28      8.65e-7     8.65e-7 **** 


## Group by TSB
pwc2 <- rp %>%
  group_by(TSB) %>%
  emmeans_test(Log10.CFU.per.ml ~ Implant.type, p.adjust.method = "bonferroni")
pwc2
print(pwc2)
## Selecting rows
pwc2[1,] ## Select row 1
pwc2[,1] ## Select column 1
pwc2[c(1,3,4),] ## Select multiple rows, in this case 1, 3, 4

pwc2
## A tibble: 42 x 9
## TSB .y.              group1 group2    df statistic        p    p.adj p.adj.signif
## * <int> <chr>            <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
## 1     5 Log10.CFU.per.ml A      B         28      3.30 2.66e- 3 5.59e- 2 ns          
## 2     5 Log10.CFU.per.ml A      C         28      9.42 3.54e-10 7.44e- 9 ****        
## 3     5 Log10.CFU.per.ml A      D         28      5.81 3.06e- 6 6.44e- 5 ****        
## 4     5 Log10.CFU.per.ml A      E         28    -59.2  5.83e-31 1.22e-29 ****        
## 5     5 Log10.CFU.per.ml A      F         28    -60.3  3.49e-31 7.34e-30 ****        
## 6     5 Log10.CFU.per.ml A      G         28     -3.77 7.80e- 4 1.64e- 2 *           
## 7     5 Log10.CFU.per.ml B      C         28      6.12 1.32e- 6 2.76e- 5 ****        
## 8     5 Log10.CFU.per.ml B      D         28      2.51 1.80e- 2 3.79e- 1 ns          
## 9     5 Log10.CFU.per.ml B      E         28    -62.5  1.29e-31 2.71e-30 ****        
##10     5 Log10.CFU.per.ml B      F         28    -63.6  7.95e-32 1.67e-30 ****        
## … with 32 more rows

pwc2[11:20,]
## A tibble: 10 x 9
## TSB .y.              group1 group2    df statistic        p    p.adj p.adj.signif
## <int> <chr>            <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
## 1     5 Log10.CFU.per.ml B      G         28     -7.06 1.10e- 7 2.31e- 6 ****        
## 2     5 Log10.CFU.per.ml C      D         28     -3.61 1.18e- 3 2.48e- 2 *           
## 3     5 Log10.CFU.per.ml C      E         28    -68.6  9.58e-33 2.01e-31 ****        
## 4     5 Log10.CFU.per.ml C      F         28    -69.7  6.16e-33 1.29e-31 ****        
## 5     5 Log10.CFU.per.ml C      G         28    -13.2  1.56e-13 3.27e-12 ****        
## 6     5 Log10.CFU.per.ml D      E         28    -65.0  4.31e-32 9.06e-31 ****        
## 7     5 Log10.CFU.per.ml D      F         28    -66.1  2.71e-32 5.68e-31 ****        
## 8     5 Log10.CFU.per.ml D      G         28     -9.58 2.48e-10 5.20e- 9 ****        
## 9     5 Log10.CFU.per.ml E      F         28     -1.10 2.81e- 1 1.00e+ 0 ns          
## 10    5 Log10.CFU.per.ml E      G         28     55.4  3.62e-30 7.60e-29 **** 

pwc2[21:30,]
## A tibble: 10 x 9
## TSB .y.              group1 group2    df statistic        p    p.adj p.adj.signif
## <int> <chr>            <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
## 1     5 Log10.CFU.per.ml F      G         28     56.5  2.10e-30 4.41e-29 ****        
## 2    10 Log10.CFU.per.ml A      B         28      6.59 3.76e- 7 7.90e- 6 ****        
## 3    10 Log10.CFU.per.ml A      C         28     13.0  2.08e-13 4.37e-12 ****        
## 4    10 Log10.CFU.per.ml A      D         28      9.42 3.54e-10 7.44e- 9 ****        
## 5    10 Log10.CFU.per.ml A      E         28    -59.7  4.68e-31 9.82e-30 ****        
## 6    10 Log10.CFU.per.ml A      F         28    -63.0  1.05e-31 2.20e-30 ****        
## 7    10 Log10.CFU.per.ml A      G         28     -4.55 9.41e- 5 1.98e- 3 **          
## 8    10 Log10.CFU.per.ml B      C         28      6.44 5.70e- 7 1.20e- 5 ****        
## 9    10 Log10.CFU.per.ml B      D         28      2.83 8.60e- 3 1.81e- 1 ns          
## 10   10 Log10.CFU.per.ml B      E         28    -66.3  2.53e-32 5.32e-31 ****  

pwc2[31:42,]
## A tibble: 12 x 9
## TSB .y.              group1 group2    df statistic        p    p.adj p.adj.signif
## <int> <chr>            <chr>  <chr>  <dbl>     <dbl>    <dbl>    <dbl> <chr>       
## 1    10 Log10.CFU.per.ml B      F         28    -69.5  6.55e-33 1.38e-31 ****        
## 2    10 Log10.CFU.per.ml B      G         28    -11.1  8.31e-12 1.75e-10 ****        
## 3    10 Log10.CFU.per.ml C      D         28     -3.61 1.18e- 3 2.48e- 2 *           
## 4    10 Log10.CFU.per.ml C      E         28    -72.7  1.92e-33 4.02e-32 ****        
## 5    10 Log10.CFU.per.ml C      F         28    -76.0  5.57e-34 1.17e-32 ****        
## 6    10 Log10.CFU.per.ml C      G         28    -17.6  1.15e-16 2.42e-15 ****        
## 7    10 Log10.CFU.per.ml D      E         28    -69.1  7.92e-33 1.66e-31 ****        
## 8    10 Log10.CFU.per.ml D      F         28    -72.4  2.16e-33 4.54e-32 ****        
## 9    10 Log10.CFU.per.ml D      G         28    -14.0  3.79e-14 7.97e-13 ****        
## 10   10 Log10.CFU.per.ml E      F         28     -3.30 2.66e- 3 5.59e- 2 ns          
## 11   10 Log10.CFU.per.ml E      G         28     55.1  4.24e-30 8.90e-29 ****        
## 12   10 Log10.CFU.per.ml F      G         28     58.4  8.44e-31 1.77e-29 ****      


## Post-hoc tests
## There is also a procedure for non-significant two-way interaction
## See https://www.datanovia.com/en/lessons/anova-in-r/#two-way-independent-anova


## Visualisation: box plots with p-values
pwc3 <- pwc1 %>% add_xy_position(x = "TSB")
bxp +
  stat_pvalue_manual(pwc3) +
  labs(subtitle = get_test_label(res.aov, detailed = TRUE), caption = get_pwc_label(pwc3))



