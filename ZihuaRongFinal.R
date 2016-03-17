##Final Exam
##Name:Zihua Rong
##ID:1505002
##Email:zirong@ucsc.edu

install.packages("nycflights13")
library(nycflights13)
library(dplyr)
install.packages("RSQLite")
library(RSQLite)
library(ggplot2)

db <- src_sqlite("db.sqlite3", create = T)
df.flight<-tbl(db, sql("SELECT * FROM flights")) %>%  collect() %>% mutate(canceled=is.na(arr_time))
df.plane<-tbl(db, sql("SELECT * FROM planes")) %>%  collect()
df.airport<-tbl(db,sql("SELECT * FROM airports"))%>%  collect() %>% mutate(dest = faa)
df.weather<-tbl(db,sql("SELECT * FROM weather")) %>%   collect()

df.FP<- tbl(db, sql("SELECT * FROM flights join planes ON flights.tailnum = planes.tailnum")) %>% collect()
df.FA<-inner_join(df.flight, df.airport,by="dest")
df.FP<-inner_join(df.flight,df.plane,by="tailnum")
df.FW<-inner_join(df.flight,df.weather)

df.FT<-df.flight
df.FT$time_of_day<-NA
df.FT$time_of_day<-df.FT$hour+(df.FT$minute/60)
df.FT$date<-paste(df.FT$year,df.FT$month,df.FT$day)
df.FT$date<-as.Date(df.FT$date,"%Y%m%d")
df.FT$weekdays<-weekdays(df.FT$date)

View(df.FA)
df.FA$canceled <- ifelse(df.FA$canceled==FALSE,0,1)
View(df.FP)
df.FP$canceled <- ifelse(df.FP$canceled==FALSE,0,1)
View(df.FT)
df.FT$canceled <- ifelse(df.FT$canceled==FALSE,0,1)
View(df.FW)
df.FW$canceled <- ifelse(df.FW$canceled==FALSE,0,1)

##Part A
lm_a<-lm(dep_delay~humid+temp+wind_speed+pressure,data = df.FW)
summary(lm_a)
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 549.675401  17.438269  31.521  < 2e-16 ***
#  humid        -0.104740   0.006459 -16.217  < 2e-16 ***
#  temp          0.126600   0.006650  19.039  < 2e-16 ***
#  wind_speed    0.031717   0.006483   4.892 9.99e-07 ***
#  pressure     -0.527808   0.016955 -31.130  < 2e-16 ***

glm_a<-glm(canceled~humid+temp+wind_speed+pressure,family = binomial(link = probit), data = df.FW)
summary(glm_a)
#Coefficients:
#             Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  7.2611845  4.1039806   1.769  0.07684 .  
#humid        0.0004785  0.0014436   0.331  0.74029    
#temp         0.0073330  0.0015465   4.742 2.12e-06 ***
#wind_speed  -0.0000678  0.0017014  -0.040  0.96821    
#pressure    -0.0105735  0.0040014  -2.642  0.00823 ** 

plota1 <- ggplot(df.FW, aes(x= temp,y=dep_delay)) + geom_point()
plota1 
plota2 <- ggplot(df.FW, aes(x= humid,y=dep_delay)) + geom_point()
plota2
plota3 <- ggplot(df.FW, aes(x= pressure,y=dep_delay)) + geom_point()
plota3
plota4 <- ggplot(df.FW, aes(x= wind_speed,y=dep_delay)) + geom_point()
plota4

##Part B
lm_b<-lm(dep_delay~month+day+weekdays+time_of_day,data = df.FT)
summary(lm_b)

glm_b<-glm(canceled~month+day+weekdays+time_of_day,family = binomial(link = probit), data = df.FT)
summary(glm_b)

plot_b1 <- ggplot(df.FT, aes(df.FT$date,canceled)) + geom_bar(stat="identity", fill = "indianred")
plot_b1
plot_b2 <- ggplot(df.FT, aes(df.FT$month,canceled)) + geom_bar(stat="identity", fill = "indianred")
plot_b2
plot_b3 <- ggplot(df.FT, aes(df.FT$weekdays,canceled)) + geom_bar(stat="identity", fill = "indianred")
plot_b3

##Part C
lm_c<-lm(dep_delay~dest,data = df.FA)
summary(lm_c)

glm_c<-glm(canceled~dest,family = binomial(link = probit), data = df.FA)
summary(glm_c)

plot_c1 <- ggplot(df.FA, aes(df.FA$dest,canceled)) + geom_bar(stat="identity", fill = "indianred")
plot_c1
plot_c2 <- ggplot(df.FA, aes(df.FA$dest,dep_delay)) + geom_point()
plot_c2

##Part D
lm_d<-lm(dep_delay~seats+engines,data = df.FP)
summary(lm_d)

glm_d<-glm(canceled~seats+engines,family = binomial(link = probit), data = df.FP)
summary(glm_d)

plot_d1 <- ggplot(df.FP, aes(df.FP$seats,canceled)) + geom_bar(stat="identity", fill = "indianred")
plot_d1
plot_d2 <- ggplot(df.FP, aes(df.FP$engines,canceled)) + geom_bar(stat="identity", fill = "indianred")
plot_d2
plot_d3 <- ggplot(df.FP, aes(df.FP$seats,dep_delay)) + geom_point()
plot_d3
plot_d4 <- ggplot(df.FP, aes(df.FP$engines,dep_delay)) + geom_point()
plot_d4
