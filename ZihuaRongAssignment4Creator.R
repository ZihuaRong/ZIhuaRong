##Assignment 4
##Question 0
print("Zihua Rong")
print(1505002)
print("zirong@ucsc.edu")

##Question 1 
library(foreign)
weather<- read.csv ("E:/UCSC/294A/weather.csv",stringsAsFactors = FALSE)
flights<- read.csv ("E:/UCSC/294A/flights.csv",stringsAsFactors = FALSE)
airports<- read.csv ("E:/UCSC/294A/airports.csv",stringsAsFactors = FALSE)
planes<- read.csv ("E:/UCSC/294A/planes.csv",stringsAsFactors = FALSE)

##Question 2
flights$date<-as.Date(flights$date)
weather$date<-as.Date(weather$date)

##Question 3 
flights.2a<-subset(flights, dest=="OAK"|dest=="SFO")
nrow(flights.2a)
#we find 3508 observations in flights.2a

flights.2b<-subset(flights, 60<=dep_delay)
nrow(flights.2b)
#we find 10474 observations in flights.2b

flights.2c<-subset(flights, arr_delay>=2*dep_delay)
nrow(flights.2c)
#we find 77830 observations in flights.2C

##Question 4
library(dplyr)
select(flights,starts_with("arr"))
select(flights, ends_with("delay"))
select(flights, contains("delay"))

##Question 5
select(flights, dep_delay) %>% arrange(desc(dep_delay)) %>% head(5)
flights %>% arrange(desc(dep_delay - arr_delay)) %>% head(5)

##Qestion 6
flights <- mutate(flights, speed = dist/(time/60))
flights <- mutate(flights, delta = dep_delay - arr_delay)
View(flights)

flights %>% arrange(desc(speed)) %>% head(5)
flights %>% arrange(desc(delta)) %>% head(5)
flights %>% arrange(delta) %>% head(1)

##Question 7
flights.7a <- flights %>% group_by(carrier) %>% summarise (
  cancelled = sum(cancelled),
  total_flights = n(),
  cancelled_percent = cancelled/total_flights,
  min = min(delta, na.rm = T),
  quantile_1st = quantile(delta, 0.25, na.rm = T),
  mean = mean(delta, na.rm = T),
  median = median(delta, na.rm = T),
  quantile_3rd = quantile(delta, 0.75, na.rm = T),
  quantile_90th = quantile(delta, 0.90, na.rm = T),
  max = max(delta, na.rm = T)
)

print(flights.7a %>% arrange(desc(cancelled_percent)))
day_delay <- flights %>% dplyr::filter(!is.na(dep_delay)) %>% group_by(date) %>%
summarise(delay = mean(dep_delay), n = n()) %>% dplyr::filter(n > 10)

##Question 8
day_delay <- day_delay %>% mutate(diff = delay - lag(delay)) 
day_delay %>% arrange(desc(diff)) %>% head(5)

##Question 9
dest_delay <- flights %>% group_by(dest) %>%
  summarise (
    avg_arr_delay = mean(arr_delay, na.rm = T),
    number_flights = n()
  )

airports <- airports %>% select(dest = iata, name = airport, city, state, lat, long)

df.9a <- airports %>% left_join(dest_delay, by="dest")
df.9a %>% arrange(desc(avg_arr_delay)) %>% head(5)

df.9b<- airports %>% inner_join(dest_delay, by="dest")
#The observations do not match

df.9c<- airports %>% right_join(dest_delay, by="dest")
#we find 116 observations, and we don't find NA in avg_arr_delay

df.9d<- airports %>% full_join(dest_delay, by="dest")
#we find 3378 observations, and we find 3262 NAs in avg_arr_delay
#there are so many NAs because number of rows of both tables are same

##Question 10
hourly_delay <- flights %>% filter(!is.na(dep_delay)) %>% group_by(date,hour) %>%
  summarise(delay = mean(dep_delay), n = n())

hourly_delay %>% full_join(weather) %>% group_by(conditions) %>% 
  summarise(max_delay = max(delay, na.rm=T)) %>% arrange(desc(max_delay))

##Question 11
#Part a
library(tidyr)
library(dplyr)
df <- data.frame(treatment = c("a", "b"), subject1 = c(3, 4), subject2 = c(5, 6))
df
df %>% gather(subject, value, -treatment) %>% 
  mutate(subject = subject %>% substr(8,9)) %>% select(subject, treatment, value)

#Part B
df <- data.frame(
  subject = c(1,1,2,2),
  treatment = c("a","b","a","b"),
  value = c(3,4,5,6)
)
df

df %>% spread( key = subject, value = value) %>%
  rename(subject1 = `1`, subject2 = `2`)

#Part C
df <- data.frame(
  subject = c(1,2,3,4),
  demo = c("f_15_CA","f_50_NY","m_45_HI","m_18_DC"),
  value = c(3,4,5,6)
)
df
df %>% separate(demo, into = c('sex','age','state') , sep = '_')

#Part D
df <- data.frame(
  subject = c(1,2,3,4),
  sex = c("f","f","m",NA),
  age = c(11,55,65,NA),
  city = c("DC","NY","WA",NA),
  value = c(3,4,5,6)
)
df

df <- df %>% unite("demo", c(sex, age, city),sep = '.')
df[4,2] = NA
df

