########################################
##### Comparing Means of Two Groups ####
############### t-Test #################
########################################

## for small data sets statisticsl hypothesis test can be used, for 
##lasrge sets not feasible
##median to cut the varibale into two halves
##median is the middle of q1 nd q2. cutting variable accoridng to the data set
##better to cute from median so that forups are more ballanced. cutting from center is risky
##numerical dep and categorical dep-- carspeed, cartest
#p=0.55>o.o5 so accept null hypothesis
#var.eual since there is equal variance we mention true
#need to interpret the dist
#sepal is leaves
#t test-> more than two grps need to use anova bcasue will lose some data
#
#
##
##### Assumption 1 ########
#### Testing Normality ###

cars
summary(cars)

?shapiro.test

hist(cars$speed)
hist(cars$dist)

shapiro.test(cars$speed)
shapiro.test(cars$dist)


## Categorizing Distance ##
summary(cars$dist)

cars$dist_cat<- cut(cars$dist,c(1,36,120))

levels(cars$dist_cat)<-c("1","2")

### Checking distribution of speed across groups ###
?which
g1<-which(cars$dist_cat=="1")
g2<-which(cars$dist_cat=="2")

plot(density(cars$speed[g1]),main="Density curves of 2 Groups",xlim=c(0,35))
lines(density(cars$speed[g2]),col=2)


######### Assumption 2 ##################
### Homogenity of Variances ###
?var.test
var.test(cars$speed~cars$dist_cat,data=cars)

####### t-test #########
?t.test
t.test(cars$speed~cars$dist_cat,var.equal=TRUE)

plot(cars$speed~cars$dist_cat)

###########################################################
##### Example:  T test Application on Iris Data (in R) ####
###########################################################

data() 

iris<-iris
summary(iris)

for (i in 1:4){
  hist(iris[,i],main= paste(colnames(iris)[i]))
  print(shapiro.test(iris[,i]))
}

hist(iris$Sepal.Width)

g1<-which(iris$Species=="setosa")
g2<-which(iris$Species=="versicolor")
g3<-which(iris$Species=="virginica")

plot(density(iris$Sepal.Width[which(iris$Species=="setosa")]), main="Density curves of 3 Species",ylim=c(0,1.5))
lines(density(iris$Sepal.Width[which(iris$Species=="versicolor")]),ylim=c(0,2),col=2)
lines(density(iris$Sepal.Width[which(iris$Species=="virginica")]),col=3)
# when you cmpr 3 groups you see, longer sepal black line
#sig diff between black and the other 2
#
### Homogenity of Variance Assumption###

var.test(iris$Sepal.Width[c(g1,g2)]~iris$Species[c(g1,g2)],data=iris)
var.test(iris$Sepal.Width[c(g2,g3)]~iris$Species[c(g2,g3)],data=iris)
var.test(iris$Sepal.Width[c(g1,g3)]~iris$Species[c(g1,g3)],data=iris)


?boxplot

boxplot(iris$Sepal.Width~iris$Species)
#density curves quite similar
#boxplot best to visualise
#1 outlier of first group, different loc of medians

### Two sample t-test ###
t.test(iris$Sepal.Width[c(g1,g2)]~iris$Species[c(g1,g2)],var.equal=TRUE)
#sig diff between the means at 95% confidence level
t.test(iris$Sepal.Width[c(g1,g3)]~iris$Species[c(g1,g3)],var.equal=TRUE)
#bw 1s and 3rd also we are rejecting null hypothesis
t.test(iris$Sepal.Width[c(g2,g3)]~iris$Species[c(g2,g3)],var.equal=TRUE)
#by looking at den and box we saw there wasnt a sig diff, but looking at p value we cna see the sig diff, 
#plots can be mis,eading
#problem of this is time consuming, should have applied 10 diff t test if we have 5 groups
#for more than 2 groups we should apply anova and not t test hence
#this is an eg to apply anova and not t test

