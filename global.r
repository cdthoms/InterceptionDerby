library(RCurl)
library(ggplot2)
library(reshape2)
library(shiny)
library(XML)
library(markdown)
require(shinysky)

setwd('./Derby')

qb_url = 'http://espn.go.com/nfl/players/_/position/qb'
qb_list = readHTMLTable(qb_url,stringsAsFactors=F)

qbframe = qb_list[[1]]
qbframe = qbframe[,c(1:2)]

names(qbframe) = c('Name','Team')

qbframe = qbframe[grep(',',qbframe$Name),]

sched = read.csv('nfl2014sched.csv',stringsAsFactors=F)
sched$DATE = as.Date(sched$DATE,format='%m/%d/%y')

weeklyDate = aggregate(sched$DATE,list(week=sched$WEEK),max)

currentWeek = min(which(weeklyDate$x>=Sys.Date()))

info = read.csv('2014derbypicks.csv',stringsAsFactors=F)
### Establish pick order (Dropbox or Googledocs)
### Eliminate last week's picks
