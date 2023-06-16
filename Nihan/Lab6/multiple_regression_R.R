###############################################################
######                                                   ######
###### R SCRIPT: Multiple Linear Regression              ######                      
######                                                   ######
######           Nihan Acar-Denizli, PhD                 ######
######                                                   ######
###############################################################

###############################################################
#### Linear Regression on Heptathlon Data Set ################
###############################################################

install.packages("HSAUR")
library(HSAUR)

data("heptathlon", package = "HSAUR")


plot(heptathlon)
View(heptathlon)
#Changing direction of some variables
heptathlon$hurdles <- max(heptathlon$hurdles) - heptathlon$hurdles
heptathlon$run200m <- max(heptathlon$run200m) - heptathlon$run200m
heptathlon$run800m <- max(heptathlon$run800m) - heptathlon$run800m

### Correlation Matrix ###
cor(heptathlon)
plot(heptathlon)

### Correlation with score variable ###
cor(heptathlon)[8,]

### Simple Linear Regression ####
slr<-lm(score~longjump,data=heptathlon)
summary(slr)

#Rsquared us the square of the correlation coeffi, 90% of the variation
### Multiple Linear Regression Model ###
reg_model1<-lm(score~., data=heptathlon)
summary(reg_model1)

#found to be sig except the p vaue of hurdles whereas the other variabkes are cosntant 
#however 

### Testing Regression Assumptions ###
### 1. Normality of the Error Term ###
# Using QQ plot
qqnorm(residuals(reg_model1))
#violation of normality. should have been a diagonal
# Using Histogram
hist(residuals(reg_model1))
# It is skewed. bc of just one outlined value
#Shapiro Wilks Test
shapiro.test(residuals(reg_model1))
# The error term does not follow a Normal distribution. (p<0.05)

### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(reg_model1))
#Want to see rect pattern, but you can see a devivaition in the bottom value, outlier
##Breusch Pagan Test
library(lmtest)
bptest(reg_model1)

# H0 is accepted (p>0.05).
# The homogenity of variances is provided.

### 3. The independence of errors ### 
dwtest(reg_model1, alternative = "two.sided")
# There is not an autocorrelation in the data set (p>0.05).
# The errors/observations are independent.

### 4. Multicollinearity ###

## Correlation among Independent Variables ##
score <- which(colnames(heptathlon) == "score")
plot(heptathlon[,-score])
cor(heptathlon[,-score])

library(car)
vif(reg_model1)
#shows if there is multicollinearity or not
## Eliminating longjump from data set
reg_model2<-lm(score~highjump+shot+run200m+javelin+run800m, data=heptathlon)
summary(reg_model2)
vif(reg_model2)

###1. Normality###
#Shapiro Wilks Test
shapiro.test(residuals(reg_model2))
# Using Histogram
hist(residuals(reg_model2))

### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(reg_model2))
##Breusch Pagan Test
library(lmtest)
bptest(reg_model2)
#more than 2 models, use 29:27
### 3. The independence of errors ### 
dwtest(reg_model2, alternative = "two.sided")


### Model Validation ####
### Test-Train Models ###
n <- nrow(heptathlon)
train.sample1 <- sample(1:n, round(0.67*n))
train.set1 <- heptathlon[train.sample1, ] 
test.set1 <- heptathlon[-train.sample1, ] 
#-train.sample is the remaining ones from the line 108

train.model1 <- lm(score~highjump+shot+run200m+javelin+run800m , data = train.set1)
summary(train.model1)

?predict
#predict in test dattaset
yhat<-predict(train.model1, test.set1, interval="prediction")
yhat

y<-test.set1$score
#bind predicted with obsrvd vaslues
error<-cbind(yhat[,1,drop=FALSE],y,(y-yhat[,1])^2)
sqr_err<-error[,3]
#all from 3rd column
mse<-mean(sqr_err)

### Root Mean Square Error ###
RMSE1<-sqrt(mse/(nrow(test.set1)))
#def the ratio of mse to the total number of obs in the set
RMSE1

names(train.model1)
RMSE_train1<- sqrt(mean((train.model1$residuals)^2)/nrow(train.set1))
#same 
#we have 15 here and in tst we have 25
RMSE_train1


################################
##### Adding Dummy Variables ###
################################

data("mtcars")
head(mtcars)
summary(mtcars)

as.factor(mtcars$am)
#am and vs are not numerical. they are categorical variables 0 or 1
#am automaic or manusal, vm v shaped engie or not
reg1<-lm(mpg ~ hp + am , data = mtcars)
#since we have two categiries we use onyl one dummy variable
#more than 2 then num of categiries-1

summary(reg1)
#p value>0.05 so sig. explaiins 78% of the regression 
#if you have 3 categries you will have 3 lines
#0 for automatic 1 for manual, 0 for vshaped 1 for straight
#red line mpg and horse powee for manual cars
#other for mpg and automatic cars

names(reg1)
coefficients(reg1)

## Yauto = bo+b1x1
## Ymanual =(b0+b2)+b1x1+b2x2

b0man=coefficients(reg1)[1]+coefficients(reg1)[3]
b0auto=coefficients(reg1)[1]

### Plot Regression Lines for Manual and Automatic Cars ###
plot(mpg ~ hp, data = mtcars, col =  am+1, pch = am+1, cex = 1)
abline(b0auto, coefficients(reg1)[2], col = 1, lty = 1, lwd = 1) # add line for auto
abline(b0man,  coefficients(reg1)[2], col = 2, lty = 2, lwd = 1) # add line for manual
legend("topright", c("Automatic", "Manual"), col = c(1, 2), pch = c(1, 2))

### Simple Model vs. Dummy Variable Model ###

reg2<-lm(mpg ~ hp, data = mtcars)
summary(reg2)
anova(reg2,reg1)

#simple lin reg sig as rsquared