library(dplyr)
library(ggplot2)
library(tidyr)
corona <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv", header=T, sep=",")
coro <- corona[,c(1,4)]
coro <- coro %>%
	group_by(date) %>% summarise(daily=sum(cases))%>% 
	mutate (newbyday= ifelse(row_number()==1, daily, daily-lag(daily)))
coro$date <- as.Date(as.character(coro$date))
coro <- filter(coro, date <= "2020-04-04")
pl <- ggplot(coro, aes(x=date, y=newbyday))+
	geom_col(fill="red")+
	geom_hline(yintercept=c(10000, 20000, 30000), linetype="dotted")+
	theme_classic()+
	coord_fixed(ratio=5/10000, xlim=as.Date(c("2020-02-26", "2020-04-04")))+
#5/10000 vì scale khác nhau 
print(pl)

