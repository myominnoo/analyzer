library(shiny)
library(bslib)
library(tidyverse)
library(sortable)
library(bsplus)

source("R/ui_helpers.R")


# an example data set, which contains the weight of 10 mice
# before and after the treatment.
# Data in two numeric vectors
# ++++++++++++++++++++++++++
# Weight of the mice before treatment
before <- c(200.1, 190.9, 192.7, 213, 241.4, 
            196.9, 172.2, 185.5, 205.2, 193.7)
# Weight of the mice after treatment
after <- c(392.9, 393.2, 345.1, 393, 434, 
           427.9, 422, 383.9, 392.3, 352.2)
# Create a data frame
my_data <- data.frame(
  id = rep(1:10, 2), 
  group = factor(rep(c("before", "after"), each = 10), 
                 c("before", "after")),
  weight = c(before, after)
)
my_data
