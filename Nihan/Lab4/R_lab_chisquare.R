##########################################
###  SMDE R Lab 3 ########################
#### Nihan Acar-Denizli, Phd  ############
#### UPC, Barcelona ######################
##########################################

#################################
##### Chi square Test ###########
#################################

##################################
#### Example: Horoscope Signs ####
##################################

astro<-matrix(c(888,1025,1032,1055,861,1005,1065,1069,914,1031,1008,
                1047,850,991,1039,1120),nrow=4,ncol=4, byrow=TRUE)
astro

##Joint Probabilities
pt<-prop.table(astro)
pt

## Marginal Probabilities
?margin.table
margin.table(astro)
margin.table(astro,1)
margin.table(astro,2)

## Row and column probabilities  ##
row_marg<-margin.table(pt,1)
col_marg<-margin.table(pt,2)

### Conditional Prob ###
?sweep
sweep(pt,1,row_marg,'/')

## Test of Independence ##
chisq.test(astro)

################################################
###  Chi-Square Test for Given Probabilities ###
### Example: Is the die biased? ###
################################################
pr<-rep(1/6,6)
chisq.test(c(13,12,25,18,17,15),p=pr)

####Exercise 1: Epilachna varivestis (Poisson Dist) E(x)=sum (x.p) ###
### Expected Value

ind<-c(0,1,2,3,4,5)
freq=c(12,56,23,10,5,4)
l<-sum(ind*freq)/110

?dpois
p1<-dpois(0,l)
p1

p2<-dpois(1,l)
p2


p<-vector()

for (i in 0:5){
p[i+1]<-dpois(i,l)
}
p
sum(p)
#[1] 0.9945768

p<-p/sum(p)
p
sum(p)
#[1] 1

n=sum(freq)
exp<-n*p
chi2<-sum((freq-exp)^2/exp)
chi2

?pchisq
pval<-1-pchisq(chi2,5)
pval

chisq.test(freq,p=p)

### Merging Categories ###
exp
exp2<-c(exp[1:4],exp[5]+exp[6])
freq2<-c(freq[1:4],freq[5]+freq[6])

chi2_new<-sum((freq2-exp2)^2/exp2)
chi2_new

pval2<-1-pchisq(chi2_new,4)
pval2

p2<-c(p[1:4],p[5]+p[6])
sum(p2)

chisq.test(freq2,p=p2)

##############################################
######### Exercise 2: Internet Shopping ######
##############################################

shop<-matrix(c(399,119,39,261,72,50,284,97,20,263,51,15,393,143,41,
         531,145,97,502,150,86),nrow=3,ncol=7)

shop

margin.table(shop)
margin.table(shop,1)
margin.table(shop,2)


pshop<-prop.table(shop)
rm<-margin.table(pshop,1)
cm<-margin.table(pshop,2)

### Conditional Prob ###
?sweep
sweep(pshop,1,rm,'/')
sweep(pshop,2,cm,'/')


chi_shop<-chisq.test(shop)
names(chi_shop)
chi_shop$expected
chi_shop

