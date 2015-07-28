#Structured, Active, In-Class Learning
#  Randomized Trial
#University of Pennsylvania
#Fall, 2015; MATH 104
#Michael Chirico and Rebecca Maynard

#(Reproducible) Random number generation

library(xlsx)
setwd("~/Desktop/research/SAIL_maynard_et_al")
#Using Hang Seng index
#  as cited in The Economist Espresso
#  July 28, 2015 edition
set.seed(2435196)

#Number of U[0,1] variates to draw
NN<-1000L

write.xlsx2(runif(NN),file="random_numbers_150728.xlsx")