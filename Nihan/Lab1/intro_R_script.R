##############################
###### Introduction to R ######
##############################


#Addition
2+3

#Multiplication and division
4*5/6

#7 to the 8th power
7^8

options(digits=4)
10/3

#Square root
sqrt(2)

# Euler's constant e 
exp(1)

pi

options(digits=7) # back to the default

x<-7*41/pi
x

sqrt(-1)
sqrt(-1+0i)
sqrt(as.complex(-1))

(0+1i)^2
typeof((0+1i)^2)

x<-c(74, 31, 95, 61, 76, 34, 23, 54, 96)

x
class(x)
xa<-c(1,"2")
class(xa)

newXa <- sapply(xa, as.numeric)
class(newXa)
length(newXa)

##Scan
#x<-scan()

seq(from = 1, to = 5)
seq(from = 2, by = -0.1, length.out = 4)

x <- c(74, 31, 95, 61, 76, 34, 23, 54, 96)
x[1]
x[2:4]
x[c(1,3,4,8)]
x[-c(1,3,4,8)]

LETTERS[1:5]
letters[-(6:24)]

#Data Frames
x <- 5:8
x
y <- letters[3:6]
A <- data.frame(x, y)
A
?data.frame

A[3, ]
A[1, ]
A[, 2]

##Functions and Expressions
x <- 1:5 
sum(x)

length(x)
min(x)
max(x)

#Sample mean
mean(x)

#Sample standard deviation
sd(x)

?intersect
intersect

### Displaying Data ###

## Visualisation of Numerical Data
##Dot plot
stripchart(precip, xlab = "rainfall")
stripchart(rivers, method = "jitter", xlab = "length")
stripchart(discoveries, method = "stack", xlab = "number")

## Histogram
hist(precip)
hist(precip, main = "")
hist(precip, freq = FALSE, main = "")

hist(precip, breaks = 10, main = "My title")
hist(precip, breaks = 200, main = "")

## Stemplots
library(aplpack)
Ukdriver<-UKDriverDeaths
stem.leaf(UKDriverDeaths, depth = FALSE)

##Index Plot
plot(LakeHuron, type = "h")
plot(LakeHuron, type = "p")

## Box Plot
boxplot(count~spray, ylab="count", xlab="spray", data=InsectSprays,main="The number of killed insects")
?boxplot

head(InsectSprays)
colnames(InsectSprays)
summary(InsectSprays)



