##Assignment 2
##Question 0
ZihuaRongAssignment2 <- list(
  firstName = "Zihua",
  lastName  = "Rong",
  email = "zirong@ucsc.edu",
  StudentID = 1505002
)

##Question 1
install.packages("repmis")
library("repmis")
diamondsURL <- source_data("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/diamonds.CSV")
View(diamondsURL)


#a
library(foreign)
ZihuaRongAssignment2$s1a <- nrow(diamondsURL)
#There are 7 observations 
#b
ZihuaRongAssignment2$s1b <- ncol(diamondsURL)
#There are 5 columns
#c
ZihuaRongAssignment2$s1c <- names(diamondsURL)
#The names are "V1" "carat" "cut" "clarity" "price"
#d
ZihuaRongAssignment2$s1d <- summary(diamondsURL$price)
# The result is below
# Min.  1st Qu.  Median  Mean  3rd Qu.  Max.  NA's   
# 420     450     600     650   825     980    1 

##Question 2
NHIS_2007_TSV <- source_data("https://raw.githubusercontent.com/EconomiCurtis/econ294_2015/master/data/NHIS_2007_TSV.txt")

#a
ZihuaRongAssignment2$s2a <- nrow(NHIS_2007_TSV)
#There are 4785 observations 
#b
ZihuaRongAssignment2$s2b <- ncol(NHIS_2007_TSV)
#There are 9 columns
#c
ZihuaRongAssignment2$s2c <- names(NHIS_2007_TSV)
#The names are "HHX"    "FMX"    "FPX"    "SEX"    "BMI"    "SLEEP"  
#"educ"   "height" "weight"
#d
ZihuaRongAssignment2$s2d <- mean(NHIS_2007_TSV$weight)
#The mean weight is 266.2357
#e
ZihuaRongAssignment2$s2e <- median(NHIS_2007_TSV[["weight"]])
#The median weight is 175

#We can create a histogram of weights 
weights <-NHIS_2007_TSV$weight
hist(weights)
table(weights)
#And we can find a group of data from 996 to 999 pounds

#We can create a new table to set those weight observations to NA
NHIS_2007_TSV$weights<- ifelse(test = NHIS_2007_TSV$weight<996 | NHIS_2007_TSV$weight>999,
                               yes = NHIS_2007_TSV$weight,
                               no = NA)
hist(NHIS_2007_TSV$weights)
table(NHIS_2007_TSV$weights)

#f
ZihuaRongAssignment2$s2f <- mean(NHIS_2007_TSV$weights, na.rm=TRUE)
#The new mean weight is 174
#g
ZihuaRongAssignment2$s2g <- median(NHIS_2007_TSV$weights, na.rm=TRUE)
#The new median weight is 170
#h
men <- subset(NHIS_2007_TSV,(SEX==1))
women <- subset(NHIS_2007_TSV,(SEX==2))
ZihuaRongAssignment2$s2h <- summary(women[["weights"]])
#The summary  of weights for women is
#Min.   1st Qu.  Median   Mean   3rd Qu.   Max.    NA's 
#100.0   130.0   150.0   158.2   178.0   274.0     329 
#i
ZihuaRongAssignment2$s2i <- summary(men[["weights"]])
#The summary  of weights for men is
#Min.   1st Qu.  Median  Mean    3rd Qu.  Max.     NA's 
#128.0   169.0   187.0   192.8   212.0   298.0     207 

##Question 3
vec <- c(letters,LETTERS)
#a
ZihuaRongAssignment2$s3a <- paste(vec[c(1:26*2)])
#b
ZihuaRongAssignment2$s3b <- paste(vec[c(52,9,8)]) 
#c
arr <- array(c(letters,LETTERS), dim=c(3,3,3))
View(arr)
arr1 <- arr[,,1]
arr2 <- arr[,,2]
arr3 <- arr[,,3]
ZihuaRongAssignment2$s3c <- arr2[,1]
#d
ZihuaRongAssignment2$s3d <- c(arr1[2,2],arr2[2,2],arr3[2,2])
#e
ZihuaRongAssignment2$s3e <- paste(arr[2,3,3],arr[3,3,1],arr[2,3,1],sep = "")


