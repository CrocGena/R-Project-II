getwd()
setwd('C:\\Users\\User\\Documents\\2 project')
list.files()
pf<-read.csv('pseudo_facebook.tsv',sep='\t')
names(pf)
pff<-read.delim('pseudo_facebook.tsv')



install.packages("ggplot2")

library(ggplot2)
qplot(x=dob_day,data=pf)+scale_x_continuous(breaks=1:31)+facet_wrap(~dob_month,ncol = 3)

#scale_x_continuous(limits = c(0, 1000))
qplot(x=friend_count,data=pf,xlim=c(0,1000),ylim = c(0,10000))
qplot(x=friend_count,data=subset(pf,!is.na(gender)),binwidth=25)+scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))+facet_wrap(~gender)
table(pf$gender)
by(pf$friend_count,pf$gender,summary)
ggplot(aes(x = age), data = pf) +
  geom_histogram(binwidth = 1,colour='black',fill = '#5760AB') +
  scale_x_continuous(breaks = seq(0, 113, 1))+scale_x_log10()


install.packages('gridExtra')
library(gridExtra)

logscale<-qplot(x=log10(friend_count),data=pf)
countscale<-qplot(aes(x=friend_count),data=pf)+geom_histogram(binwidth = 1)+
  scale_x_log10()
grid.arrange(logscale,countscale, ncol=2)


qplot(x=friend_count,data=subset(pf,!is.na(gender)),binwidth=10,geom = 'freqpoly',color=gender)+scale_x_continuous(limits = c(0,1000),breaks = seq(0,1000,50))

ggplot(aes(x = www_likes), data = subset(pf, !is.na(gender))) +
  geom_freqpoly(aes(color = gender)) +
  scale_x_log10()
by(pf$www_likes,pf$gender,summary)

library(ggplot2)

qplot(data=subset(pf,!is.na(gender)),x=gender,y=friendships_initiated,geom = 'boxplot')+coord_cartesian(ylim = c(0,200))

by(pf$friendships_initiated,pf$gender,summary)


summary(pf$mobile_likes>0)



summary(pf$mobile_likes)


data("diamonds")
qplot(data=diamonds,x=price)
summary(diamonds$price < 500)
by(diamonds$price,diamonds$color,summary)

summary(diamonds$price >= 15000)

sum(diamonds$price >= 15000)
IQR(subset(diamonds, price <1000)$price)
IQR(diamonds$price[diamonds$color=="J"])



data<-read.csv('data.csv', header = T, row.names = "row", check.names = F)
dataa<-read.table('data.csv')

 


