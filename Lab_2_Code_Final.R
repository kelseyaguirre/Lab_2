##Call libraries
##Uploading the R code only just incase my package doesn't work!

library(tidyverse)
library(dplyr)
library(stringr)
library(magrittr)
library(knitr)

##set working directory
setwd("/Users/kelseyaguirre/Documents/Data_Science_I/Week_6/Lab_2")

##read the csv and turn it into "all_data"
all_data <- read.csv("Top_100_DRG_FY2011.csv") %>%
  as_tibble(("Top_100_DRG_FY2011.csv"))

#create a subset called "my_data"
my_data <- all_data %>%
  #select the columns I want
  select(1,10,11,12) %>%
  #mutate the factors to characters for DRG can be a character
  #and so the other three columns can be later turned into numbers
  mutate_if(is.factor, as.character) %>%
  #group by DRG.Definition
  group_by(DRG.Definition)

#the next sections is to get rid of the , in the numbers so that they're not NA after fun2
#also changing each to numbers
#tried to make this less repetitive but it wasn't working when I did that
my_data <- my_data %>%
  mutate(Average.Covered.Charges =
           gsub(",", "", Average.Covered.Charges)) %>%
  mutate(Average.Covered.Charges =
           as.numeric(Average.Covered.Charges)) %>%
  mutate(Average.Total.Payments =
           gsub(",", "", Average.Total.Payments)) %>%
  mutate(Average.Total.Payments =
           as.numeric(Average.Total.Payments)) %>%
  mutate(Average.Medicare.Payments =
           gsub(",", "", Average.Medicare.Payments)) %>%
  mutate(Average.Medicare.Payments =
           as.numeric(Average.Medicare.Payments))

#how I saved "my_data" it for the package
#save(my_data, file = 'Lab2/data/my_data.RData')

##function for part 1
fun1 <- function(df, y){
  num1 <- df %>%
    #select DRG.Def column and whatever column string y is
    select(1, y) %>%
    #grou by DRG Code
    group_by(DRG.Definition) %>%
    #rename input y to Payments for plot
    rename(Payments = y)
##data and mapping of x and y for the plot
  ggplot(data = num1, aes(x = DRG.Definition,
                          y = Payments)) +
    #plot a boxplot with no outlier points (clean it up)
    geom_boxplot(outlier.shape = NA)+
    #create the title
    ggtitle("Average Payments by DRG Code") +
    #label the x axis
    xlab("DRG Code") +
    #label the y axis
    ylab("Average Payment ($)") +
    #set limits so it's easier to look at
    #picked 100000 because it looked the best
    ylim(0,100000) +
    #git rid of background panels to make it cleaner
    theme(
      panel.grid = element_blank()
    )
}

##functions for part 2

#function to use in fun3 (my actual function for part 2)
#takes a string labeled at y to output three different types of calculations
fun2 <- function(x, y){
  ##to return the mean
  if (y == "mean"){
    return(mean(x))
  }
  ##to return the median
  if (y == "median"){
    return(median(x))
  }
  ##to return the standard deviation
  if (y == "sd"){
    return(sd(x))
  }
}

##my function for part 2
fun3 <- function(df, y){
  num2 <- df %>%
    #select the DRG.Def column and the Average.Medicare.Payments column
    select(1,4) %>%
    #group by DRG Definition
    group_by(DRG.Definition) %>%
    ##use fun2 to calculate either the mean, median or mode for Average.Medicare.Payments
    summarise(Calculation = fun2(Average.Medicare.Payments, y)) %>%
    ##output a table with my function
    kable(caption = "Average Medicare Payment Calculations")
  ##show the table from num2
  num2
}
