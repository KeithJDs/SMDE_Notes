###########################################
### Solution of Probability Distribution ##
### Exercises    ##########################
### Nihan Acar-Denizli, Phd  ##############
### UPC, Barcelona ########################
###########################################

###################################
### Question 1  ###################
###################################

p<-6/10000
n<-1000

lam<-n*p

#Poisson distribution approximation
pp<-numeric()
for(i in 1:3){
pp[i]<-dpois(i-1,lam)
}
sum(pp)

#Binomial distribution
pb<-numeric()
for (i in 1:3){
 pb[i]<-dbinom(i-1,n,p) 
}
sum(pb)

###################################
### Question 2  ###################
###################################
p<-1/1200
n<-300
l<-p*n

##Cumulative probability of at most two accidents
?ppois
cump<-ppois(1,l)

## Probability of at least two fatal accidents
1-cump

## Alternatively
pa<-numeric()
for(i in 1:2){
  pa[i]<-dpois(i-1,l)
}
1-sum(pa)

###################################
### Question 3  ###################
###################################

lam<-4
n<-100

# a) P(x=0) #
dpois(0,lam)*100  
# [1] 1.831564  Approximately 2 days

# b) P(x>=1) #
(1-ppois(1,lam))*n 
#[1] 90.84218  Approximately 91 days

# c) P(X<=3) #
ppois(3,lam)*100   
#[1] 43.34701  Approximately 43.5 days

# Can be solved as well by adding the probabilities from 
# P(0,4) to P(3,4) in a for loop

###################################
### Question 4  ###################
###################################

l<-0.0003

1-pexp(10000,l) 
#[1] 0.04978707

1-pexp(10000,0.00035)
#[1] 0.03019738
#In the latter case, fewer fans to give at least 10000 hours service.

###################################
### Question 5  ###################
###################################

r<-1/8

# a) P(t<5) ## Since it is a continous distribution, 
## we should write 5 in the function as the upper bound.
pexp(5,r)
# [1] 0.4647386

# b) P(T>t) 
?qexp
qexp(1-0.95,r)
# [1] 0.4103464 minutes
# In terms of seconds:
qexp(1-0.95,r)*60
# [1] 24.62078 seconds

