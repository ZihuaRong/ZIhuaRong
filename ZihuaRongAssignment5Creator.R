##Assignment 5
print("Zihua Rong")
print("zirong@ucsc.edu")
StudentID <-1505002
print(StudentID)

##Question 1
#Part a
install.packages("ggplot2")
library(ggplot2)
data(diamonds)
q1pa <- ggplot(diamonds, aes(x=x*y*z,y=price,color=clarity))
q1pa + geom_point(aes(color+clarity))+geom_point(aes(size=carat))+scale_x_log10()+scale_y_log10()

#Part b
q1pb<-ggplot(diamonds,aes(carat,fill=clarity,..density..))
q1pb+geom_histogram()+facet_grid(cut~.)

#Part c
q1pc<-ggplot(diamonds,aes(x=cut,price))
q1pc+geom_jitter(alpha=0.1)+geom_violin()

##Question 3
#Part a
library(foreign)
require(dplyr)
d<- read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")

group_d <- d %>% dplyr::group_by(year,month)%>%
  dplyr::summarise(
    q_one = quantile(rw, .1, na.rm = T),
    q_nine = quantile(rw, .9, na.rm = T),
    q_first = quantile(rw, .25, na.rm = T),
    q_third = quantile(rw, .75, na.rm = T),
    Median.RW = median(rw, na.rm = T),
    count = n())

group_d<-group_d %>% mutate(date=paste(year,month,"01", sep="-"),
                            date=as.Date(date,format="%Y-%m-%d"))

q3pa <- ggplot(group_d, aes(x=date, y=Median.RW))
q3pa + geom_ribbon(aes(ymin=q_one , ymax=q_nine),alpha=0.6) + geom_ribbon(aes(ymin=q_first , ymax=q_third),alpha=0.2) + geom_line(aes(y=Median.RW))+lims(y=c(0,50))

#Part b
group_d2 <- d %>% dplyr::group_by(year,month,educ)%>%
  dplyr::summarise(
    Median.RW = median(rw, na.rm = T),
    count = n())

group_d2<-group_d2 %>% mutate(date=paste(year,month,"01", sep="-"),
                              date=as.Date(date,format="%Y-%m-%d"))

q3pb <- ggplot(group_d2, aes(x=date, y=Median.RW,group=educ))
q3pb + geom_line(aes(color=educ))
