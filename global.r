library(RCurl)
library(ggplot2)
library(reshape2)
library(shiny)
library(XML)
library(markdown)
library(RNeo4j)
library(mailR)
require(shinysky)

# setwd('/Users/Chris/Documents/Personal/Old Computer/Chris Documents/Derby')
# setwd('~/InterceptionDerby')

qb_int_url = 'http://www.sportingcharts.com/nfl/stats/quarterback-interception-rates/2014/'
qb_int_list = readHTMLTable(qb_int_url,stringsAsFactors=F)

team_int_url = 'http://www.sportingcharts.com/nfl/stats/team-interception-rate/2014/'
team_int_list = readHTMLTable(team_int_url,stringsAsFactors=F)

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

# info = read.csv('2014derbypicks.csv',stringsAsFactors=F)

myCsv = getURL('https://docs.google.com/spreadsheet/pub?key=0Akv8ehIfGJVKdGx3NXdib0p6Q01CSEZ4amY0RW53Ymc&single=true&gid=2&output=csv',
               ssl.verifypeer=F,useragent='R')
info = read.csv(textConnection(myCsv),stringsAsFactors=F)
info = info[!is.na(info$week),]

graph = startGraph(url = 'http://derby.sb02.stations.graphenedb.com:24789/db/data/',
                   username = 'derby',
                   password = 'xSj1GzPmqFWns6SYZo8V')
### Establish pick order (Dropbox or Googledocs)
### Eliminate last week's picks
