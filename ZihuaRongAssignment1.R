##Assignment 1
##problem 0
firstName<- "Zihua"
lastName <- "Rong"
print(paste(firstName,lastName))

StudentID<-1505002
print(StudentID)
  
##problem 1
#a
library(foreign)
df.dta<-read.dta("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_dta.dta")

#b
df.csv <-read.csv("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_CSV.csv")

#c
df.td <- read.table("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_TSV.txt")

#d
df.rdata <-load(url("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/NHIS_2007_RData.RData"))

##program 2
#a
#df.dta is 188kb, df.cvs is 139kb, df.td is 139kb and NHIS_2007_RData.RData is 45.3kb

#b
#NHIS_2007_RData.RData is the smallest

#c
#as different file types have different way to encode 
#so it causes their variability 

##problem 3
typeof(NHIS_2007_RData)
# the type of this data structure is "list"
class(NHIS_2007_RData)
# the class of this data structure is "data.frame"
length(NHIS_2007_RData)
# the length of this dataset is 9
dim(NHIS_2007_RData)
# there are 9 variables and 4785 observations in the dataset
nrow(NHIS_2007_RData)
# there are 4785 rows in the dataset
ncol(NHIS_2007_RData)
# there are 9 columns in the dataset
summary(NHIS_2007_RData)
# the result is below
#HHX             FMX             FPX             SEX             BMI       
#Min.   :   16   Min.   :1.000   Min.   :1.000   Min.   :1.000   Min.   :12.91  
#1st Qu.:13404   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:1.000   1st Qu.:23.63  
#Median :27527   Median :1.000   Median :1.000   Median :2.000   Median :26.97  
#Mean   :27009   Mean   :1.019   Mean   :1.359   Mean   :1.549   Mean   :31.73  
#3rd Qu.:40192   3rd Qu.:1.000   3rd Qu.:2.000   3rd Qu.:2.000   3rd Qu.:31.51  
#Max.   :53955   Max.   :6.000   Max.   :8.000   Max.   :2.000   Max.   :99.99  
#SLEEP             educ           height          weight     
#Min.   : 3.000   Min.   : 0.00   Min.   :59.00   Min.   :100.0  
#1st Qu.: 6.000   1st Qu.:12.00   1st Qu.:64.00   1st Qu.:149.0  
#Median : 7.000   Median :13.00   Median :67.00   Median :175.0  
#Mean   : 9.507   Mean   :14.25   Mean   :69.58   Mean   :266.2  
#3rd Qu.: 8.000   3rd Qu.:16.00   3rd Qu.:71.00   3rd Qu.:215.0  
#Max.   :99.000   Max.   :99.00   Max.   :99.00   Max.   :999.0 

##problem 4
d.dta<-read.dta("http://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
str(d)
#So there are 30 variables and 1119754 observations in the dataset.

summary(d.dta$rw)
#  Min. 1st Qu.  Median   Mean   3rd Qu.  Max.   NA's 
#  1.8    10.7    15.9    19.8    24.4   354.8  521279 
#  So min is 1.8, mean is 19.8, median is 15.9, max is 354.8 
#  first quartile is 10.7, third quartile is 24.4
#  And there are 521279 NAs.

##problem 5
v<-c(1, 2, 3, 4, 5, 6, 7, 4, NULL, NA)
length(v)
#length of v is 9, and it does not match the number of values in the vector
# It??s because ??NULL?? is not counted in the length of v.

summary(v)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 1.00    2.75    4.00    4.00    5.25    7.00       1 
# We may find the that the mean ignoring the NA value is 4

##problem 6
#a
x <- matrix(c(1, 4, 7, 2, 5, 8, 3, 6, 9), nrow=3, ncol=3) 
t(x)
#we can use the function t(x) to get the transpose matrix of x
eigen(x)
#we can use the function eigen(x) to gen the eigenvalues and eigenvectors of x

# values
# 1.611684e+01 -1.116844e+00 -1.303678e-15

# vectors
# -0.2319707 -0.78583024  0.4082483
# -0.5253221 -0.08675134 -0.8164966
# -0.8186735  0.61232756  0.4082483

#b
y <- matrix(   c(1, 3, 2, 2, 2, 3, 3, 1, 0),   nrow=3,   ncol=3) 
z<-solve(y)
#we can use the function solve(y) to get the inverse matrix of y
y%*%z
#thw new matrix is called identity matrix

##problem 7
#First we should create the table with the functions below:
carat = c(5, 2, 0.5, 1.5, 5, NA, 3) 
cut = c("fair", "good", "very good", "good", "fair", "Ideal", "fair") 
clarity = c("SI1", "I1", "VI1", "VS1", "IF", "VVS2", "NA" )
price = c(850, 450, 450, 0, 750, 980, 420)
diamonds <- data.frame(carat, cut, clarity, price)
print(diamonds)

#a
summary(diamonds$price)
#So the mean price is 557.1 

#b
meanprice1<-subset(diamonds,(cut=="fair"))
summary(meanprice1$price)
#So the mean price of cut "fair" is 673.3

#c
meanprice2<-subset(diamonds,(cut=="good"|cut=="very good"|cut=="Ideal"))
summary(meanprice2$price)
#So the mean price of cut "good", "very good" and "ideal" is 470.0

#d
meanprice3<-subset(diamonds,(carat>2))
meanprice4<-subset(meanprice3,(cut=="very good"|cut=="Ideal"))
summary(meanprice4$price)
#So the median price is NA

