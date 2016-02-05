##Assignment3##
##Question0
firstName<- "Zihua"
lastName <- "Rong"
print(paste(firstName,lastName))

studentID<- "1505002"
print(studentID)

email <- "zirong@ucsc.edu"
print(email)

##Question1
#install package pdlyr
install.packages("dplyr")
library(dplyr)
#install library foreign
library(foreign)

df.ex <-read.dta("https://github.com/EconomiCurtis/econ294_2015/raw/master/data/org_example.dta")
class(df.ex)

##Question2
df.ex.2a <- dplyr::filter(df.ex,year == 2013 & month == 12)
print(nrow(df.ex.2a))
df.ex.2b <- dplyr::filter(df.ex,year == 2013 & (month == 7 | month == 8 | month == 9))
print(nrow(df.ex.2b))

##Question3 
df.ex.3a<-df.ex %>% dplyr::arrange(year,month)

##Question4
df.ex.4a<-df.ex %>% dplyr::select(year:age)
df.ex.4b<-df.ex %>% dplyr::select(year,month,starts_with("i"))
unique(select(df.ex,state))



##Question5
stndz <- function(x){(x - mean(x, na.rm = T))  /  sd(x, na.rm = T)}
nrmlz <- function(x){(x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T))}

df.ex.5a<- df.ex %>% 
  mutate(rw.stndz= stndz(rw),rw.nrmlz=nrmlz(rw))

df.ex.5b <- df.ex %>% group_by(year, month) %>%
  mutate(rw_stndz=stndz(rw), rw_nrmlz=nrmlz(rw), count=n())

##QUestion 6 
df.ex.6<-
  df.ex %>% 
  group_by(year,month,state)%>%
  summarise(
    rw_min=min(rw,na.rm = T),
    rw_1st=quantile(rw,na.rm = T,0.25),
    rw_mean.art =mean(rw,na.rm = T),
    rw_3rd=quantile(rw,na.rm = T,0.75),
    rw_max=max(rw,na.rm = T),
    rw_med=median(rw,na.rm=T),
    count=n()
  )%>%
  select(state,starts_with("rw_"),count)

print(df.ex.6 %>% ungroup() %>% arrange(desc(rw_mean.art)) %>%
        select(year,month,state) %>% head(1))

##Question7
df.ex$state.char <-as.character(df.ex$state)
df.ex.7a <- df.ex %>% arrange(year,month,desc(state.char))
