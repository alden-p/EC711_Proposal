#Author: Alden Porter
#This code reads in Lalonde Data and uses RF algorithms to evaluate heterogeneous treatment effects

#####################################################################
# Initialize Libraries, Change Directory
#####################################################################

rm(list=ls(all=TRUE))

#library(causalTree)
library(FNN)

#library(randomForestCI)
library(grf)
library(ggplot2)

setwd("C:/Users/Porte/Documents/Courses/EC711/Research-Proposal/Emperics")

#####################################################################
# Import Data
#####################################################################

lalonde_data <- read.csv("Input/nsw.csv", header = TRUE)


dehejia_data <- read.csv("Input/nsw_dw.csv", header = TRUE)

#####################################################################
# Define Functions
#####################################################################

cfpredict <- function(X,Y,W, ntrees,s_frac){
  #This function takes data, and a fraction s_frac to be specified and returns causal forest predictions
  
  cf <- causal_forest(X, Y, W, num.trees = ntrees, honesty = TRUE,
          sample.fraction = s_frac, min.node.size = 1)
  
  cf.pred = predict(cf, X, estimate.variance = TRUE)
  
  return(cf.pred)
}

perc.rank <- function(x) trunc(rank(x))/length(x) # Return the percentiles as a vector


#####################################################################
# Run Experiment
#####################################################################

###########Lalonde Data
#Define the treatment, result and controls for the lalonde data
lalonde_Y <- data.matrix(lalonde_data["re78"]-lalonde_data["re75"])
lalonde_W <- data.matrix(lalonde_data["treat"])
age_pct_lalonde <- perc.rank(data.matrix(lalonde_data["age"]))
edu_pct_lalonde <- perc.rank(data.matrix(lalonde_data["education"]))
lalonde_X <- data.matrix(cbind(edu_pct_lalonde, age_pct_lalonde, lalonde_data["black"], lalonde_data["hispanic"], lalonde_data["married"]))

pred <- cfpredict(lalonde_X,lalonde_Y,lalonde_W, 700,.25)

pred.values <- pred["predictions"]
age_plot_vars <- data.frame(age_pct_lalonde, pred.values)
edu_plot_vars <-data.frame(edu_pct_lalonde, pred.values)
plot(plot_vars, main = "Predicted treatment effect by age pctile", xlab = "age", ylab = "predicted effect")
plot(edu_plot_vars, main = "Predicted treatment effect by educ pctile", xlab = "educ", ylab = "predicted effect")

tau_hat_lalonde <- mean(lalonde_Y[lalonde_W == 1]) - mean(lalonde_Y[(1-lalonde_W) == 1])


########### Dehejia Data
#Define the treatment, result and controls for the dehejia data
dehejia_Y <- data.matrix(dehejia_data["re78"]-dehejia_data["re75"])
dehejia_W <- data.matrix(dehejia_data["treat"])
age_pct_dehejia <- perc.rank(data.matrix(dehejia_data["age"]))
edu_pct_dehejia <- perc.rank(data.matrix(dehejia_data["education"]))
dehejia_X <- data.matrix(cbind(edu_pct_dehejia, age_pct_dehejia, dehejia_data["black"], dehejia_data["hispanic"], dehejia_data["married"]))

pred <- cfpredict(dehejia_X,dehejia_Y,dehejia_W, 700,.25)

pred.values <- pred["predictions"]
age_plot_vars <- data.frame(age_pct_dehejia, pred.values)
edu_plot_vars <-data.frame(edu_pct_dehejia, pred.values)
plot(age_plot_vars, main = "Predicted treatment effect by age pctile", xlab = "age", ylab = "predicted effect")
plot(edu_plot_vars, main = "Predicted treatment effect by educ pctile", xlab = "educ", ylab = "predicted effect")

tau_hat_dehejia <- mean(dehejia_Y[dehejia_W == 1]) - mean(dehejia_Y[(1-dehejia_W) == 1])

