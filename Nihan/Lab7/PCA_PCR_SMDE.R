
###############################################################
######                                                   ######
###### R SCRIPT: Principal Component Analysis (PCA)      ######                      
######                                                   ######
######           Nihan Acar-Denizli, PhD                 ######
######                                                   ######
###############################################################

#####################################
### Example 1: USArrests data #######
### USArrests data PCA and FA #######
#####################################

#install.packages("HSAUR")
library(HSAUR)
data<-USArrests

### PCA with princomp function (based on spectral value decomposition) ##
?princomp
pcUSA<-princomp(data,cor=TRUE) 
summary(pcUSA)

eigs<-pcUSA$sdev^2
eigs
plot(eigs,type="b")

pcUSA$loadings
biplot(pcUSA)

#### PCA with prcomp function (based on singular value decompositon)###
prusa<-prcomp(data,scale. = TRUE)
summary(prusa)

prusa$sdev^2
prusa$rotation

biplot(prusa)

##########################################
### PCA to Factor computations
#aij=sqrt(eig_j)*u_ji
a_11<-pcUSA$loadings[1,1]*sqrt(eigs[1])
a_12<-pcUSA$loadings[1,2]*sqrt(eigs[2])
eigs[1]

# Factor correlations
pcUSA$loadings[1,]*sqrt(eigs)
pcUSA$loadings[2,]*sqrt(eigs)
pcUSA$loadings[3,]*sqrt(eigs)
pcUSA$loadings[4,]*sqrt(eigs)

### Communalities
com1<-(pcUSA$loadings[1,]*sqrt(eigs))^2
sum(com1[1:2])
#########################################

#### FACTOR ANALYSIS OF USA arrest data ###
library(FactoMineR)
?PCA
usafa <- PCA(data)
summary(usafa)
names(usafa)

usafa$eig

##Individual scores
usafa$ind$coord

#factor loadings
usafa$var
names(usafa$var)
usafa$var$cor
usafa$var$coord
usafa$var$coord

#Square of factor loadings communalities of variables
usafa$var$cos2
#col add both vars. how much two dims explain the corrlation of te
apply(usafa$var$cos2[,1:2],1,sum)

# Contributions of variables at each dimension 
usafa$var$contrib
apply(usafa$var$contrib[,1:2],2,sum)

#####################################
## Factor correlations to PC loadings
x1coef<-usafa$var$cor[1,]/sqrt(eigs)
x2coef<-usafa$var$cor[2,]/sqrt(eigs)
x3coef<-usafa$var$cor[3,]/sqrt(eigs)
x4coef<-usafa$var$cor[4,]/sqrt(eigs)

rbind(x1coef,x2coef,x3coef,x4coef)
#####################################


#########################################
### Example 2: PCA on mtcars data set ###
#########################################

###### Bartlett's Test of Spherecity#####

install.packages("psych")
library(psych)

## Bartlett test requires the computation of the correlation matrix R and the number of observations.
colnames(mtcars)
R<-cor(mtcars[,])
print(R)
plot(R)

## Bartlett tests if the correlation matrix is an identity matrix. 
#### (H0:R=I) ##
### If we reject the null hypothesis the variables are correlated.
??nrows
n<-nrow(mtcars)
cortest.bartlett(R,n)

###### Kaiser-Meyer-Olkin (KMO) Test ###
## We can use kmo function written by Prof. Shigenobu Aok.
### (http://minato.sip21c.org/swtips/factor-in-R.pdf)
kmo <- function(x)
{
  x <- subset(x, complete.cases(x))       # Omit missing values
  r <- cor(x)                             # Correlation matrix
  r2 <- r^2                               # Squared correlation coefficients
  i <- solve(r)                           # Inverse matrix of correlation matrix
  d <- diag(i)                            # Diagonal elements of inverse matrix
  p2 <- (-i/sqrt(outer(d, d)))^2          # Squared partial correlation coefficients
  diag(r2) <- diag(p2) <- 0               # Delete diagonal elements
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}

#KMO index
kmo(mtcars)


###################################
### PCA with FactoMineR Package ###
###################################
install.packages("FactoMineR")
library(FactoMineR)

res_PCA<-PCA(mtcars[,2:7],scale=TRUE, graph=FALSE) # by default scale=TRUE

# You can see the elements of the output of PCA 
res_PCA

# eigenvalues and percentage of variances
res_PCA$eig

# The loadings of the variables on the components
res_PCA$var$coord 
res_PCA$var$cor

plot(res_PCA,choix="var")

# The scores of the individuals
res_PCA$ind$coord
res_PCA$var$cos2

# The plot of individual scores on 2 dimensiones
plot(res_PCA,choix = "ind")


### Dimension Reduction in Regression ###
## Solving Multicollinearity Problem with PCR ##

# Assume we are interested in modelling "mpg"
reg_mpg<-lm(mpg~.,data=mtcars)
summary(reg_mpg)

## Multicollinearity ##
library(car)
vif(reg_mpg)

## Principal Component Regression #
## First you should repeat PCA excluding mpg

res_PCA<-PCA(mtcars,scale=TRUE, graph=FALSE, quanti.sup = 1, quali.sup = c(8,9)) # by default scale=TRUE
plot(res_PCA,choix="var")

# eigenvalues and percentage of variances
res_PCA$eig

# The loadings of the variables on the components
res_PCA$var$coord 
res_PCA$var$cor
res_PCA$var$contrib

mtcars$PC1<-res_PCA$ind$coord[,1]
mtcars$PC2<-res_PCA$ind$coord[,2]
#mtcars$PC3<-res_PCA$ind$coord[,3]
#uncoorr values bc on pc2 we dont have a specific var
cars_pcr<-lm(mpg~PC1 + PC2, data=mtcars)
summary(cars_pcr)

### Test-Train Models ###

n <- nrow(mtcars)
train.sample1 <- sample(1:n, round(0.67*n))
#2/3rd in train sample
#can see pc not sig
train.set1 <- mtcars[train.sample1, ] 
test.set1 <- mtcars[-train.sample1, ] 

train.model <- lm(mpg ~ PC1+PC2 , data = mtcars[train.sample1,])
summary(train.model)

yhat<-predict(train.model, mtcars[-train.sample1,], interval="prediction")
yhat

y<-test.set1$mpg

error<-cbind(yhat[,1,drop=FALSE],y,(yhat[,1]-y)^2)
sqr_err<-error[,3]

mse<-mean(sqr_err)

RMSE<-sqrt(mse/(nrow(test.set1)))
RMSE

########### ROTATION #################
####Factanal Package for rotation ####
#Factor Analysis (we use the correlation matrix R)
car<-mtcars[,2:7]

fit <- factanal(car, 2, covmat=R, rotation="promax")
names(fit)

#the number of factors

fit$factors 

# the loading of variables on the factors
fit$loadings

# Let us see just the loadings exceeding 0.3.
print(fit, digits=2, cutoff=.3, sort=TRUE)

#Varimax Rotation
fit_var <- factanal(car, 2, covmat=R, rotation="promax")
load_fit<-fit_var$loadings[,1:2] 
print(fit_var, digits=2, cutoff=.3, sort=TRUE) 

# The Graph of Factor Loadings
plot(load_fit,type="n") # set up plot
text(load_fit,labels=names(car),cex=.7)

