
#Q1: Import data set from .csv file to R. 
?read.csv
cars<-read.csv("cars.csv")
cars<-read.csv2("cars.csv")

#Q2:Summarize the data. How many quantitative and qualitative variables are there in the data set?
summary(cars)
str(cars)

cars<-read.csv("cars.csv",stringsAsFactors = TRUE)
#cars$brand<-as.factor(cars$brand)

##
#write.csv2(cars,"cars.csv")

str(cars$year)
cars$year<-as.factor(cars$year)

#Q3: Visualize the distribution of the variables year and brand.
barplot(summary(cars$year),ylab="Frequency")
barplot(summary(cars$brand))

#Q4:a) Visualize the distribution of mpg, hp and weight of the cars by using a proper chart.
## b) Interpret the graphs. 
hist(cars$mpg)
hist(cars$hp)
hist(cars$weightlbs)

#Q5: Construct a descriptive statistics table with mean, median and standard deviation for the variables mpg, hp and weight. 
for(i in 2:6){
  cars[,i]<-as.numeric(cars[,i])
}
str(cars)
apply(cars[,c(1,4,5)],2,mean,na.rm=TRUE)

options(digits = 3)
avr<-apply(cars[,2:6],2,mean,na.rm=TRUE)
med<-apply(cars[,2:6],2,median,na.rm=TRUE)
std<-apply(cars[,2:6],2,sd,na.rm=TRUE)


tab<-rbind.data.frame(avr,med,std)
rownames(tab)<-c("mean","median","std. dev.")
colnames(tab)<-names(avr)
tab

#Q6: Compare the distribution of weight across car brands. Visualize it by using boxplot.
?boxplot
boxplot(cars$weightlbs~cars$brand)




