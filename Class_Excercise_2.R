#EXCERCISE 1

library(tidyverse)
library(knitr)

set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
kable(head(dat))

#Scatter Plot of the dat

plot(dat,pch=16,col=3)


#Eyeball the above scatterplot of the data. What would you say is a reasonable estimate of the mean of Y at X=0? Why?
#The reasonable estimate of the mean of Y at X=0, is 2.75

#Estimate using loess and kNN (you choose the hyperparameters)

dat$d <- abs(dat$x-0)
dat$d
dat2 <-arrange(dat,d)
dat2
dat2_new <-  dat2[1:5,]
dat2_new

y_predict <- mean(dat2_new$y)
y_predict


dat_loess <-filter(dat2,d<1)
mean(dat_loess$y)

dat_loess <-filter(dat2,d<0.01)
mean(dat_loess$y)


#EXCERCISE 2

library(tidyverse)
xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x){
  dat$d <- abs(dat$x-x)
  dat2 <-arrange(dat, d)
  dat2_new <-  dat2[1:5,]
  mean(dat2_new$y)
})
loess_estimates <- map_dbl(xgrid, function(x){
  ## YOUR CODE HERE FOR LOESS
  ## Note: The variable "x" here is a single value along the grid.
  ## Hint1: Extend your code for loess from the previous exercise.
  ## Hint2: Say you store the prediction in the variable "yhat".
  ##         Then in a new line of code, write: return(yhat)
  dat$d <- abs(dat$x-x)
  dat2 <-arrange(dat, d)
  dat_loess <-filter(dat2,d<100)
  mean(dat_loess$y)
})
est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y), colour="orange") +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()
