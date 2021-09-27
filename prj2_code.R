#R Project
#Comcast Telecom Consumer Complaints

#import library
library(lubridate)
library(dplyr)
library(ggplot2)
library(RColorBrewer)


#Import data into R environment.
comcast <- read.csv(file.choose(),header = T,stringsAsFactors = FALSE)

cast1 <- comcast
cast2 <- cast1
cast3 <- cast1

View(comcast)
View(cast1)
View(cast2)
View(cast3)


View(cast1["Date"])

View(day(cast1$Date))

#format all values in date to a single format
#cast1$Date <- as.Date(cast1$Date)
cast1$Date <- dmy(cast1$Date)





#Provide a table with the frequency of complaint types.
#Which complaint types are maximum i.e., around internet, network issues, or across any other domains.

cast1$Effectrp <- 'Others'
cast1$Effectrp[grepl('internet', cast1$Customer.Complaint,ignore.case = TRUE)] <- 'Internet'
cast1$Effectrp[grepl('network', cast1$Customer.Complaint,ignore.case = TRUE)] <- 'network'
cast1$Effectrp[grepl('billing', cast1$Customer.Complaint,ignore.case = TRUE)] <- 'billing'


View(table(cast1$Effectrp))


#Create a new categorical variable with value as Open and Closed. 
#Open & Pending is to be categorized as Open and 
#Closed & Solved is to be categorized as Closed.

cast1$ComplaintStatus <- 'Closed'
cast1$ComplaintStatus[grepl('open',cast1$Status,ignore.case = TRUE)] <- 'Open'
cast1$ComplaintStatus[grepl('pending',cast1$Status,ignore.case = TRUE)] <- 'Open'
cast1$ComplaintStatus[grepl('solved',cast1$Status,ignore.case = TRUE)] <- 'Closed'
cast1$ComplaintStatus[grepl('closed',cast1$Status,ignore.case = TRUE)] <- 'Closed'

View(cast1)

#Provide state wise status of complaints in a stacked bar chart. 
#Use the categorized variable from Q3.

gr<-group_by(cast1,State,ComplaintStatus)
gs<- summarise(gr,Count = n())

View(gs)

freq_df <- data.frame(table(cast1$State,cast1$ComplaintStatus))

#ggplot(freq_df)+
#  geom_bar(aes(x = Var1, y = Freq), 
#           stat = 'identity')

#ggplot(data.frame(chart_data), aes(fill=Count, x = State, y = ComplaintStatus)) + 
#  geom_bar(position="stack",stat='identity')+
ggplot(as.data.frame(gs) ,mapping = aes(State,Count))+
  geom_col(aes(fill = ComplaintStatus),width = 0.75)+
  labs(title = "Ticket Status Stacked Bar Chart ",
       x = "States",y = "No of Tickets")+
  theme(axis.text.x = element_text(angle = 90),
  legend.position = 'bottom'
       )

#Which state has the maximum complaints

cast1 %>% group_by(State) %>% summarize(count=n()) %>% arrange(desc(count)) %>% View()


#Which state has the highest percentage of unresolved complaints

cast1 %>% filter(ComplaintStatus=='Open') %>% 
  group_by(State) %>% summarize(count=n()) %>% arrange(desc(count))

cast1 %>% filter(ComplaintStatus=='Open') %>% 
  group_by(State) %>% summarize(count=n()) %>% 
  arrange(desc(count)) %>%
  mutate(percentage= prop.table(count) * 100) %>% View()

cast1 %>% filter(ComplaintStatus=='Closed') %>% 
  group_by(State) %>% summarize(count=n()) %>% 
  arrange(desc(count)) %>%
  mutate(percentage= prop.table(count) * 100)



#Provide the percentage of complaints resolved till date, 
#which were received through theInternet and customer care calls.

T <- cast1 %>% 
  summarize(countT=n()) 

TC <- cast1 %>% 
  filter(ComplaintStatus=='Closed') %>% 
  group_by(ComplaintStatus) %>% 
  summarize(countT=n()) 

I <- cast1 %>% filter(Received.Via=='Internet') %>% 
  filter(ComplaintStatus=='Closed') %>% 
  group_by(Received.Via,ComplaintStatus) %>% 
  summarize(countI=n()) 
 

C <- cast1 %>% filter(Received.Via=='Customer Care Call') %>% 
  filter(ComplaintStatus=='Closed') %>% 
  group_by(Received.Via,ComplaintStatus) %>% 
  summarize(countC=n()) 

ClosedPercent <- (TC$countT/T$countT)*100
InternetPercent <- (I$countI/T$countT)*100
CustomercarePercent <- (C$countC/T$countT)*100
View(ClosedPercent)
View(CustomercarePercent)
View(InternetPercent)


#Provide the trend chart for the number of complaints at 
#monthly and daily granularity levels.

df <- cast1 %>% group_by(Date) %>% summarize(count=n())

#daily granularity levels
ggplot(data = df,aes(as.POSIXct(Date),count))+
  geom_line()+
  geom_point(size = 1)+
  scale_x_datetime(breaks = "5 days",date_labels = "%d/%m")+
  labs(title = "Daily Ticket Count",x= "Days",y ="No. of Tickets")+
  theme(axis.text.x = element_text(angle = 75),
        plot.title = element_text(hjust = 0.5))

#monthly granularity levels
class(month(cast1$Date))
cast1$Month<-months(cast1$Date)
ans1<-cast1 %>% group_by(Month =as.integer(month(Date))) %>% 
  summarize(count=n()) %>% arrange(desc(count))
#Plotting for monthly granularity level
ggplot(data = ans1,aes(Month,count,label = count))+
  geom_line()+
  geom_point(size = 0.8)+
  geom_text()+
  labs(title = "Monthly Ticket Count",x= "Months",y ="No. of Tickets")+
scale_x_continuous(breaks = ans1$Month)

