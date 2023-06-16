
##########################################
###  SMDE R Lab: Regression ##############
#### Nihan Acar-Denizli, Phd  ############
#### UPC, Barcelona ######################
##########################################
##XBr,YBar, sxy,sx2,sy2

#### Simple Linear Regression ####
library(FactoMineR)

data(decathlon)
colnames(decathlon)
head(decathlon)
summary(decathlon)

decathlon$'100m'
decathlon$'110m.hurdle'


colnames(decathlon)[c(1,6)]<-c("x100m","x110m.hurdle")

#we add x in front of the variables 100m and 110m.hurdle. Otherwise
#in the lm function they are not read since they begin with a number.

#to check linearity. linear association btw 2 vars. correlatio analysis of two tests
cor.test(decathlon[,6],decathl*on[,1])

## Scatter Plot of the variables including the regression line
??scatterplot
library(car)

#0.57 moderate positive linear association . imp for us to compute rquare
#how much the independent explains the dependendt
#rsquar should be as high as issibe
#more than pvalue we need to examine the scatterplot

#regLine is the regression line in the plot
#lm is used to fit linear models. Can be used to carry out regression...
#smooth is true then both mean and variance functions are drawn for ungrouped data
#and mean fn only for grouped data
scatterplot(x100m~x110m.hurdle, regLine=lm, smooth=FALSE, data=decathlon)

# Simple Linear Regression Model
RegModel.1 <-lm(x100m ~ x110m.hurdle, data=decathlon)
summary(RegModel.1)
#slope diff than 0 since p<0.05. significamt effect
#sd around the line. used to interpret the deviation around the line, no limit- residual
#adj is adj is adj measure possibility of variable
#for one vaar only interpret mul, not adj
#33% of the variation n the dependednt van be explained by independent 
#remaining 60 cannot be explained by the model
#f-statistic - used to test the null hypothesis of betag
#33% of.. 23:00
#66% cannpot be explainned using regression model
#tsqaured rsult folllows an f distribution. normally rhis f test is used to test
#p value is exact;y the same as t statistic because we habe only one line of regressiom
#single linear regression
#degree of freedom0.05,k-1,n-k-1
#
# The outputs of the model
names(RegModel.1)

#Coefficient vector of RegModel.1
coef_vec<-RegModel.1$coefficients
#6.27 is B0
#
#
#


#The intercept of the model
b0<-RegModel.1$coefficients[1]
b0

#The slope of the model
b1<-RegModel.1$coefficients[2]

b1

# Confidence intervals for the parameters of the model
confint(RegModel.1)
#conf intervals bor b0 and  b1. 95/100 times i will find a beetter slope btw 
#effect on ind var is not dependent on sig vaar

#Predicted Values of the Response
yhat<-RegModel.1$fitted.values

#
#
#
# Predicted Values of the Response with Their confidence Levels
predict.lm(RegModel.1,interval="confidence")

# Prediction interval for the Response (for x=16)
?predict.lm

new=data.frame(x110m.hurdle=16)
predict.lm(RegModel.1, newdata=new, interval="prediction")


### Test of Regression Assumptions ###
### 1. Normality of the Error Term ###
# Using QQ plot
qqnorm(residuals(RegModel.1))
# Since the values are taking part close to the diagonal, 
# the distribution is approximately normal.

# Using Histogram
hist(residuals(RegModel.1))
# It is approximately normal.

#Shapiro Wilks Test
shapiro.test(residuals(RegModel.1))
# The error term follows a Normal distribution. (p>0.05)

### 2. Homogenity of Variance ###
# Residual Analysis #
plot(residuals(RegModel.1))
# Residuals have a rectangular pattern around the zero mean.
# There is no violation of this assumption.

##Breusch Pagan Test
library(lmtest)
??bptest
bptest(RegModel.1)
# H0 is accepted (p>0.05).
# The homogenity of variances is provided.

### 3. The independence of errors ### 
dwtest(RegModel.1, alternative = "two.sided")
# There is not an autocorrelation in the data set (p>0.05).
# The errors/observations are independent.

#no violation of homogenity of variance test
#p value 0.999 so we accept null hypothesis
#error terms are independent when >0.05
##### Example: anscombe data set #####
# Check the data set

#In anscombe data there are four independent and four dependent variables.
anscombe

#Let us construct four different simple linear regression models.
reg1<-lm(y1~x1,data=anscombe)
summary(reg1)

reg2<-lm(y2~x2, data=anscombe)
summary(reg2)

reg3<-lm(y3~x3,data=anscombe)
summary(reg3)

reg4<-lm(y4~x4,data=anscombe)

summary(reg4)

#Both of the models seem significant with almost the same values for
# residual standard error, R^2, adjusted R^2 and F test. 
# Which model is better? 


# Let us check the coefficient of correlation between variables
# and scatter plots.

## Correlation Tests ##
cor.test(anscombe$x1,anscombe$y1)
zcor.test(anscombe$x2,anscombe$y2)
cor.test(anscombe$x3,anscombe$y3)
cor.test(anscombe$x4,anscombe$y4)
# In all cases the coefficient of correlation is 0.816
# and significant (p=0.002).

## Scatter Plots ##
op<-par(mfrow=c(2,2))
plot(y1~x1,data=anscombe)
plot(y2~x2,data=anscombe)
plot(y3~x3,data=anscombe)
plot(y4~x4,data=anscombe)
par(op)

# 1st scatter plot: The relationship is linear.

# 2nd scatter plot: The relationship is not linear. The shape is curvelinear.
#The plot of residuals vs. fitted values suggests a parabolic relationship /a quadratic model
names(reg2)
plot(reg2$fitted.values,reg2$residuals)

# Adding a quadratic term can increase R^2 value.
x2_2<-(anscombe$x2)^2

#If we add x^2 to the regression model the R^2 increases to 1.
# So all the variation of y could be explained by using x2 and 
# x2_2 together in the model.
reg2_2<-lm(y2~x2+x2_2,data=anscombe)
summary(reg2_2)

# 3rd scatter plot: The relationship is linear but there is 
# one outlier. Either this observation should be omitted or
# a robust regression analysis should be done so as to the 
# regression line is not effected from the outlier.

# When we omit that observation.(3rd observation)) 
anscombe2<-anscombe[-3,]
reg3_2<-lm(y3~x3,data=anscombe2)
summary(reg3_2)
# R squared value increase to 1. Perfect fit!

# 4th scatter plot: Most of the observations cumulated around 
# x=8. There is just one observation apart from other which 
# causes a linear relationship in the data set. This is an influential point.
# When it is removed, there is no reason to use regression.
anscombe3<-anscombe[-8,]
plot(anscombe3$x4,anscombe3$y4)
reg4<-lm(y4~x4,data=anscombe3)
summary(reg4)

