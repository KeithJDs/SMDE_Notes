
##########################################
###  SMDE R Lab 2 ########################
#### Nihan Acar-Denizli, Phd  ############
#### UPC, Barcelona ######################
##########################################


#### Importing Data ####

### Importing txt data ###

### From the website: 
### http://archive.ics.uci.edu/ml/datasets/Car+Evaluation

car<-read.table("car.txt")
?read.table
car
summary(car)
head(car)

car<-read.table("car.txt", sep=",")
summary(car)
colnames(car)
colnames(car)<-c("buying", "maint","doors", "persons", "lug_boot", "safety", "class")

car$buying<-as.factor(car$buying)
levels(car$buying)
str(car$buying)
summary(car$buying)
summary(car)

car<-read.table("car.txt", sep=",", stringsAsFactors = TRUE)
summary(car)
str(car)

### Importing xls data ###
??read_excel
install.packages("readxl")
library(readxl)

??read.xlsx
install.packages("openxlsx")
library(openxlsx)

wine_xl1<-read_excel("winequality-red.xlsx")
summary(wine_xl1)
#MEAN=MEDIAN->NORMAL DIS
#median><mean->skewed
#right/positive sweked median
#median<mean for right/positive
#median>mean for left/negative

wine_xl2<-read.xlsx("winequality-red.xlsx")
summary(wine_xl2)


### Importing csv data ###
?read.csv2
red<-read.csv2("winequality-red.csv")
summary(red)
red<-read.csv2("winequality-red.csv",dec=".")
summary(red)

white<-read.csv2("winequality-white.csv",dec=".")
summary(white)
head(white)

####### Creating a Factor ####

dim(red)
dim(white)

winequal<-rbind(red,white)
dim(winequal)

winequal$cat<-as.factor(rep(c(1,2),c(nrow(red),nrow(white))))
head(winequal)
summary(winequal)

levels(winequal$cat)<-c("red","white")
summary(winequal)
##############################################################
### Converting a Numerical Variable to a Categorical Variable 
##############################################################

summary(winequal$quality)
?cut

winequal$qual_cat<- cut(winequal$quality,c(1,5.5,10))
levels(winequal$qual_cat)<-c("Low","High")
summary(winequal$qual_cat)

winequal$qual_cat<- cut(winequal$quality,c(2,5,6,9))
summary(winequal$qual_cat)
levels(winequal$qual_cat)<-c("Low","Medium","High")
summary(winequal$qual_cat)

##################################
##### Factor Data and Tables #####
##################################

?mtcars
summary(mtcars)
View(mtcars)


mtcars[,8]<-as.factor(mtcars[,8])
mtcars[,8]
summary(mtcars)

mtcars[,9]<-as.factor(mtcars[,9])
summary(mtcars)

## Alternatively ##
for (i in 8:9){
mtcars[,i]<-as.factor(mtcars[,i])
}

summary(mtcars)

levels(mtcars[,8])<-c("V-shaped","straight")
levels(mtcars[,9])<-c("automatic","manual")

## Cross Table ##
tab<-table(mtcars$vs,mtcars$am)
tab

?prop.table

#Joint Probability
prop.table(tab)

#Marginal Probabilities
colSums(prop.table(tab))
rowSums(prop.table(tab))

##########################
#Conditional Probabilities
##########################

# Row Profile
prop.table(tab,1)

#Column Profile
prop.table(tab,2)


##########################
#### Birthday Problem ####
##########################

?qbirthday

qbirthday(prob=0.5,365,2)

?pbirthday
pbirthday(9,classes=365,coincident=2)
pbirthday (23,365,2)


#library(Rcmdr)
#library(RcmdrPlugin.IPSUR)

#### Data Sources ####
# http://archive.ics.uci.edu/ml/index.php
# https://www.kaggle.com/datasets




