
#' Lab 2
#'
#' @param df is a dataframe we want to us
#' @param x a string for variable x in the dataframe df
#' @param y a string for variable y in the dataframe df
#'
#' @return fun1: boxplot based on df and y definded
#'         fun2: calculations based on what string is given
#'         fun3: table with either mean, median or mode calculations
#' @export
#'
#' @import ggplot2
#' @import tidyverse
#' @import dplyr
#' @import magrittr
#' @import stringr
#' @import knitr
#'
#' @examples
#'
#' fun1(my_data, "Average.Medicare.Payments")
#'
#' fun3(my_data, "mean")
#'
#'

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
