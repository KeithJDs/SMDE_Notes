

#1.Import "Ecommerce_Customers.csv" data to R. 

#Read data from Ecommerce_Customers.csv
ecommerce_customers<-read.csv("Ecommerce_Customers.csv")

#2. Check the structure of each variable (change if it is necessary). Summarize the variables.

#Check structure
str(ecommerce_customers)
#Summarize the variables
summary(ecommerce_customers)
#Change email from character to a factor
ecommerce_customers$email_new<-as.factor(ecommerce_customers$Email)

#3.Fit a simple linear regression model to predict Yearly Amount Spent. Which variable 
#would you use as predictor? Why?
ecomm_lm <- lm(Yearly.Amount.Spent , data = train.set1)




