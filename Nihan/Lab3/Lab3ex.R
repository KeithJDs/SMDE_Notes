##########################################
###  SMDE R Lab 3 ########################
#### Exercises ############
#### UPC, Barcelona ######################
#1) Suppose 1000 women are screened for a rare type of cancer that has a 
# nationwide incidence of 6 cases per 10,000. What is the probability of finding two 
# or fewer cases? (First solve the problem by using probability density functions, 
# then the cumulative distribution function.
#P(X<=2)
#P(0)+P(1)+P(2)
#or
#1-P(X>2)
#Binomial--- Binom(1000,p=6/10000=0.0006)
#dbinom,qbinom,pbinom
#size = 10000
#prob 6/10000
#n=1000
##########################################

?pbinom
pbinom(2,1000,0.0006)

#2)Assuming that a fatal accident in a factory during the year is 1/1200, calculate 
#the probability that in a factory employing 300 workers there will be at least two 
#fatal accidents in a year
#poisson
#P(X>=2)
#1-P(X<=1)
#poisson(lambda) lambda=mean = 300*1/1200 = 1/4
ppois(1,0.25)
?ppois

#1)
p<-6/10000
n<-1000
lam<-n*p
#poisson
pp<-numeric()
for(i in 1:3){
  pp[i]<-dpois(i-1,lam)
  pp[i]
}
sum(pp)
?dpois
?dbinom
#binomialpb<-numeric()
for(i in 1:3){
  pb[i]<-dbinom(i-1,n,p)
}
sum(pb)

#2)
p<-1/1200
n<-300
l<-p*n
cump<-ppois(1,l)
#p of at 2 fat acc
1-cump
#alt
pa<-numeric()
for(i in 1:3){
  pa[i]<-dpois(i-1,l)
}
1-sum(pa)

#3)
lam<-4
n<-100
#a)P(X=0)
dpois(0,lam)*100
#1.831approx 2 days

#b) P(X>=1)
(1-ppois(1,lam))*n
#90.84 approx 91 days

#c) P(X<=3)
ppois(3,lam)*100
# 43.34 approx 43 days

#4)
l<-0.0003
i-pexp(10000,l)
#0.0497
1-pexp(10000,0.000350)
#0.03019

#in the latter case, fewer fans to give atleast 10000 hours