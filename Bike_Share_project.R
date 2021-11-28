library(lubridate)
library(tidyverse)
library(tidyr)
library(readr)
library(dplyr)
library(ggplot2)
library(tibble)


ny=read_csv('new-york-city.csv',col_names = TRUE)
view(ny)
ch=read_csv('chicago.csv',col_names = TRUE)
view(ch)
wa=read_csv('washington.csv',col_names = TRUE)
view(wa)
names(ny)
names(ch)
names(wa)

#1  POPULAR TIMES OF TRAVEL

#new york most common month
a=select(ny,X1,Start_Time='Start Time',End_Time='End Time')
view(a)
b=mutate(a,start_month=month(as.Date(a$Start_Time,format='%Y/%m/%d/%h/%m/%s')))
c=group_by(b,start_month)
d=summarise(c,n=n())
view(head(d))
e=ggplot(data=d,mapping=aes(x=start_month,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('Month')+ylab('Number of occurrence')+ggtitle('new york most common month')
e

#chicago most common month
a=select(ch,X1,Start_Time='Start Time',End_Time='End Time')
view(a)
b=mutate(a,start_month=month(as.Date(a$Start_Time,format='%Y/%m/%d/%h/%m/%s')))
c=group_by(b,start_month)
d=summarise(c,n=n())
view(d)
e=ggplot(data=d,mapping=aes(x=start_month,y=n))+geom_bar(stat='identity',fill='blue')+
  xlab('Month')+ylab('Number of occurrence')+ggtitle('chicago most common month')
e
#washington monst common month
a=select(wa,X1,Start_Time='Start Time',End_Time='End Time')
view(a)
b=mutate(a,start_month=month(as.Date(a$Start_Time,format='%Y/%m/%d/%h/%m/%s')))
c=group_by(b,start_month)
d=summarise(c,n=n())
view(d)
e=ggplot(data=d,mapping=aes(x=start_month,y=n))+geom_bar(stat='identity',fill='sky blue')+
  xlab('Month')+ylab('Number of occurrence')+ggtitle('washington most common month')
e

#new york most common day of week
a=select(ny,X1,Start_Time='Start Time',End_Time='End Time')
view(a)
b=mutate(a,Day=day(as.Date(a$Start_Time,format='%Y/%m/%d/%h/%m/%s')))
view(head(d))
c=group_by(b,Day)
d=summarise(c,n=n())
view(d)
e=ggplot(data=d,mapping=aes(x=Day,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('Day')+ylab('Number of occurrence')+ggtitle('new york most common Day of week')
e


#chicago most common day of week
a=select(ch,X1,Start_Time='Start Time',End_Time='End Time')
view(a)
b=mutate(a,Day=day(as.Date(a$Start_Time,format='%Y/%m/%d/%h/%m/%s')))
view(head(d))
c=group_by(b,Day)
d=summarise(c,n=n())
view(d)
e=ggplot(data=d,mapping=aes(x=Day,y=n))+geom_bar(stat='identity',fill='navy blue')+
  xlab('Day')+ylab('Number of occurrence')+ggtitle('chicago most common Day of week')
e


#washington most common day of week
a=select(wa,X1,Start_Time='Start Time',End_Time='End Time')
view(a)
b=mutate(a,Day=day(as.Date(a$Start_Time,format='%Y/%m/%d/%h/%m/%s')))
view(head(d))
c=group_by(b,Day)
d=summarise(c,n=n())
view(d)
e=ggplot(data=d,mapping=aes(x=Day,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('Day')+ylab('Number of occurrence')+ggtitle('washington most common Day of week')
e


#New york most common hour of the day
a=select(ny,X1,Start_Time='Start Time',End_Time='End Time')
view(a)
b=mutate(a,Hour=hour(as.Date(a$Start_Time,format='%Y/%m/%d/%h/%m/%s')))
view(head(d))
c=group_by(b,Hour)
d=summarise(c,n=n())
view(d)
e=ggplot(data=d,mapping=aes(x=Hour,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('Hour')+ylab('Number of occurrence')+ggtitle('new york most common hour of day')
e



#2 POPULAR STATIONS AND TRIP

#what is the most common start station?
#New york most common start station

a=select(ny,X1,Start_Station='Start Station')
view(a)
b=group_by(a,Start_Station)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=Start_Station,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('Start Station')+ylab('Number of occurrence')+ggtitle('new york most common start station')
e


#what is the most common end station
#New york most common end station
a=select(ny,X1,End_Station='End Station')
view(a)
b=group_by(a,End_Station)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=End_Station,y=n))+geom_bar(stat='identity',fill='green')+
  xlab('End Station')+ylab('Number of occurrence')+ggtitle('new york most common End station')
e

#Chicago most common start station
a=select(ch,X1,Start_Station='Start Station')
view(a)
b=group_by(a,Start_Station)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
write.csv(d,'d.csv')
e=ggplot(data=d,mapping=aes(x=Start_Station,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('Start Station')+ylab('Number of occurrence')+ggtitle('Chicago most common start station')
e

#chicago most common end station
a=select(ch,X1,End_Station='End Station')
view(a)
b=group_by(a,End_Station)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=End_Station,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('End Station')+ylab('Number of occurrence')+ggtitle('chicago most common start station')
e

#Washington most common start station
a=select(wa,X1,Start_Station='Start Station')
view(a)
b=group_by(a,Start_Station)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=Start_Station,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('Start Station')+ylab('Number of occurrence')+ggtitle('Washington most common start station')
e

#Washington most common end station
a=select(wa,X1,End_Station='End Station')
view(a)
b=group_by(a,End_Station)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=End_Station,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('End Station')+ylab('Number of occurrence')+ggtitle('Washington most common start station')
e

#3 TRIP DURATION
#what are the total travel time for users in different cities

#Total travel time for users in new york

a=select(ny,X1,Trip_Duration='Trip Duration')
view(a)
b=group_by(a,X1)
d=summarise(b,n=sum(Trip_Duration))
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=X1,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('User')+ylab('Total travel time per user')+ggtitle('Total travel time for users in new york')
e
p=sum(d$n)
p

#Total travel time for users in chicago
a=select(ch,X1,Trip_Duration='Trip Duration')
view(a)
b=group_by(a,X1)
d=summarise(b,n=sum(Trip_Duration))
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=X1,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('User')+ylab('Total travel time per user')+ggtitle('Total travel time for users in Chicago')
e
p=sum(d$n)
p

#Total travel time for users in washington
a=select(wa,X1,Trip_Duration='Trip Duration')
view(a)
b=group_by(a,X1)
d=summarise(b,n=sum(Trip_Duration))
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=X1,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('User')+ylab('Total travel time per user')+ggtitle('Total travel time for users in Washington')
e
p=sum(d$n)
p

#What is the average travel time for users in different cities?

#avereage travel time for users in new york
a=select(ny,X1,Trip_Duration='Trip Duration')
view(a)
b=group_by(a,X1)
d=summarise(b,n=sum(Trip_Duration)/2)
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=X1,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('User')+ylab('Average travel time per user')+ggtitle('Average travel time for users in new york')
e
p=sum(d$n)/2
p

#average travel time for users in chicago
a=select(ch,X1,Trip_Duration='Trip Duration')
view(a)
b=group_by(a,X1)
d=summarise(b,n=sum(Trip_Duration)/2)
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=X1,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('User')+ylab('Average travel time per user')+ggtitle('Average travel time for users in chicago')
e
p=sum(d$n)/2
p

#average travel time for users in washington
a=select(wa,X1,Trip_Duration='Trip Duration')
view(a)
b=group_by(a,X1)
d=summarise(b,n=sum(Trip_Duration)/2)
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=X1,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('User')+ylab('Average travel time per user')+ggtitle('Average travel time for users in washington')
e
p=sum(d$n)/2
p

#4USER INFO
#what is the count of each user type

#count of new york user type
a=select(ny,X1,User_Type='User Type')
view(a)
b=group_by(a,User_Type)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=User_Type,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('User Types')+ylab('User Type count')+ggtitle('count of new york user type')
e

#count of Chicago user type
a=select(ch,X1,User_Type='User Type')
view(a)
b=group_by(a,User_Type)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=User_Type,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('User Types')+ylab('User Type count')+ggtitle('count of Chicago user type')
e

#count of washington user type
a=select(wa,X1,User_Type='User Type')
view(a)
b=group_by(a,User_Type)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=User_Type,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('User Types')+ylab('User Type count')+ggtitle('count of washinghton user type')
e

#what are the count of each gender(only available for NYC and Chicago)?

#count of new york user gender 
a=select(ny,X1,Gender)
view(a)
b=group_by(a,Gender)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=Gender,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('Gender')+ylab('Gender count')+ggtitle('count of new york user gender')
e

#count of Chicago user gender 
a=select(ch,X1,Gender)
view(a)
b=group_by(a,Gender)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=Gender,y=n))+geom_bar(stat='identity',fill='sky blue')+
  xlab('Gender')+ylab('Gender count')+ggtitle('count of Chicago gender')
e

#What are the earliest,most recent,most common year of birth(only available for NYC and Chicago)?  

#Birth Year count for new york users  
a=select(ny,X1,Birth_Year='Birth Year')
view(a)
b=group_by(a,Birth_Year)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=Birth_Year,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('Birth Year')+ylab('Birth Year count')+ggtitle('Birth Year count for new york user')
e
#OR
ggplot(data=d,mapping =aes(x=Birth_Year))+geom_histogram()




#Birth Year count for Chicago users  
a=select(ch,X1,Birth_Year='Birth Year')
view(a)
b=group_by(a,Birth_Year)
d=summarise(b,n=n())
view(arrange(d,desc(n)))
e=ggplot(data=d,mapping=aes(x=Birth_Year,y=n))+geom_bar(stat='identity',fill='red')+
  xlab('Birth Year')+ylab('Birth Year count')+ggtitle('Birth Year count for Chicago user')
e
#OR

ggplot(data=d,mapping =aes(x=Birth_Year))+geom_histogram()


