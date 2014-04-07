#source("../R/helper_functions.R")      # 
#library(reshape2)
#library(RUnit)

TestAllLogical <- function() {
  a <- c(TRUE, FALSE, TRUE, TRUE)
  b <- c("TRUE", "FALSE", "TRUE", "FALSE")
  c <- c("TRUE", "FALSE", "apple", "FALSE", as.character(NA))
  d <- as.factor(c(T, F, T, F))

  checkTrue(all.logical(a))
  checkTrue(all.logical(b))
  checkTrue(!all.logical(c))
  checkTrue(all.logical(d))
}

TestLongRoster <- function() {
  
  roster.df <-  data.frame(test.id = c(1, 2, 3),
                        ros.01.2008 = c(1.2, 3.4, 6.6),
                        ros.02.2009 = c(2.3, 3.3, 4.5),
                        ros.03.2010 = c(2.0, NA, 4.9),
                         x = c(3, 5, 4))
  
  goal <- data.frame(test.id = c(1, 1, 1, 2, 2, 3, 3, 3),
                     year = c(2008, 2009, 2010, 2008, 2009, 2008, 2009, 2010),
                     ros = c(1.2, 2.3, 2.0, 3.4, 3.3, 6.6, 4.5, 4.9))
  
  converted <- RosterToLongDf(roster.df, "ros", "test.id") 
  checkTrue(all(converted == goal))
  checkTrue(all(names(converted) == names(goal)))  

  roster.df.error <-  data.frame(test.id = c(1, 2, 3),
                                 ros.one.2008 = c(1.2, 3.4, 6.6),
                                 ros.two.2009 = c(2.3, 3.3, 4.5),
                                 ros.three.2010 = c(2.0, NA, 4.9),
                                 x = c(3, 5, 4))
  
  checkException(RosterToLongDf(roster.df.error, "ros", "test.id"))            
  
}
