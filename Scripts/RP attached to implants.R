## Barplot with error bars for multiple groups 

library(tidyverse)
library(ggplot2)
library(dplyr)

rp <- read.csv("data/Rpickettii.csv")
rp

## Function to calculate the mean and the standard deviation for each group
## data : a data frame
## varname : the name of a column containing the variable to be summarized
## groupnames : vector of column names to be used as grouping variables


data_summary <- function(data, varname, groupnames){
  require(plyr)
  summary_func <- function(x, col){
    c(mean = mean(x[[col]], na.rm=TRUE),
      sd = sd(x[[col]], na.rm=TRUE))
  }
  data_sum<-ddply(data, groupnames, .fun=summary_func,
                  varname)
  data_sum <- rename(data_sum, c("mean" = varname))
  return(data_sum)
}

## Summarise the data
df <- data_summary(rp, varname = "Log10.CFU.per.ml", groupnames = c("Implant.type", "TSB"))
df
## Convert TSB to a factor variable
df$TSB = as.factor(df$TSB)
## Reorder fill (implant type) manually
df$Implant.type <- factor(df$Implant.type, levels = c("F", "E", "G", "A", "B", "D", "C"))
head(df)


## geom_errorbar() can be used to produce a bar graph with error bars
## Standard deviation of the mean as error bar
p <- ggplot(df, aes(x = TSB, y = Log10.CFU.per.ml, fill = Implant.type)) + 
  geom_bar(stat = "identity", color = "black", position = position_dodge()) + 
  geom_errorbar(aes(ymin = Log10.CFU.per.ml - sd, ymax = Log10.CFU.per.ml + sd), width = 0.4, position = position_dodge(0.9)) +
  scale_y_continuous(limits = c(0,7), expand = expand_scale(mult = c(0, 0.1)), breaks = seq(0,7, by = 1)) +
  scale_x_discrete(expand = c(0,0))

p + scale_fill_brewer(palette = "Paired") + theme_minimal() +
  labs(x = "TSB(%)", y = "", fill = "") + theme_classic() + theme(text = element_text(size = 15))




