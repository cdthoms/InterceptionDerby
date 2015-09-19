library(RCurl)
library(ggplot2)
library(reshape2)
library(shiny)
library(XML)
library(markdown)
library(RNeo4j)
library(mailR)
require(shinysky)
suppressPackageStartupMessages(library(googleVis))

# setwd('/Users/Chris/Documents/Personal/Old Computer/Chris Documents/Derby')
# setwd('~/InterceptionDerby')

currentYear = 2015

qb_int_url = paste0('http://www.sportingcharts.com/nfl/stats/quarterback-interception-rates/',currentYear,'/')
qb_int_list = readHTMLTable(qb_int_url,stringsAsFactors=F)

team_int_url = paste0('http://www.sportingcharts.com/nfl/stats/team-interception-rate/',currentYear,'/')
team_int_list = readHTMLTable(team_int_url,stringsAsFactors=F)


# qb_url = 'http://espn.go.com/nfl/players/_/position/qb'
# qb_list = readHTMLTable(qb_url,stringsAsFactors=F)
# qbframe = qb_list[[1]]
# qbframe = qbframe[,c(1:2)]
# names(qbframe) = c('Name','Team')
# qbframe = qbframe[grep(',',qbframe$Name),]

qb_url = 'http://sports.yahoo.com/nfl/players?type=position&c=NFL&pos=QB'
qb_list = readHTMLTable(qb_url,stringsAsFactors=F)
qbs = qb_list[[9]]
qbs = qbs[-1,]
firstlast = colsplit(qbs[,1]," ",c("first","last"))
firstlast$first = substr(firstlast$first,3,100)
qbframe = data.frame(Name = paste0(firstlast$last,', ',firstlast$first),Team = qbs[,3])

sched = read.csv('nfl2014sched.csv',stringsAsFactors=F)
sched$DATE = as.Date(sched$DATE,format='%m/%d/%y')

weeklyDate = aggregate(sched$DATE,list(week=sched$WEEK),max)

currentWeek = min(which(weeklyDate$x>=Sys.Date()))

myCsv = getURL('https://docs.google.com/spreadsheet/pub?key=0Akv8ehIfGJVKdGx3NXdib0p6Q01CSEZ4amY0RW53Ymc&single=true&gid=2&output=csv',
               ssl.verifypeer=F,useragent='R')
info = read.csv(textConnection(myCsv),stringsAsFactors=F)
info = info[!is.na(info$week),]

derby = info

derby = derby[derby$name!='',c(1:7)]
names(derby) = c('Week','Person','pickorder','qb1','int.qb1','qb2','int.qb2')
derby$weeklytotal = as.numeric(derby$int.qb1)+as.numeric(derby$int.qb2)
person.total = aggregate(derby$weeklytotal,by=list(Person=derby$Person),sum,na.rm=T)
names(person.total) = c('Person','TotalINT')
person.total=person.total[order(person.total$TotalINT,decreasing=T),]
qb1 = derby[,c('Week','Person','qb1','int.qb1','pickorder')]
qb2 = derby[,c('Week','Person','qb2','int.qb2','pickorder')]
names(qb1) = names(qb2) = c('Week','Person','qb','int','pickorder')
qb = rbind(qb1,qb2)
qb = qb[!(is.na(qb$int)),]
qb.total = aggregate(qb$int,by=list(QB=qb$qb),sum)
names(qb.total) = c('QB','Int')
qb.playertotal = aggregate(qb$int,by=list(qb$qb,qb$Person),sum)
names(qb.playertotal)=c('QB','Person','Int')

PlayerQBPlot = dcast(qb.playertotal,QB~Person)

barchart = gvisColumnChart(PlayerQBPlot,options=list(isStacked=T,hAxis='{slantedText:true,slantedTextAngle:90}',width=1500,height=750))
cumulatives = c()
peeps = unique(derby$Person)
for (i in 1:4) {
  weekly.person = derby[derby$Person==peeps[i],]
  weekly.person$cumulative = cumsum(weekly.person$weeklytotal)
  cumulatives = rbind(cumulatives,weekly.person)
}

weekly = dcast(cumulatives[,c('Week','Person','cumulative')],Week~Person)
weeklytotals = gvisLineChart(weekly,options=list(width=1500,height=750))

graph = startGraph(url = 'http://derby.sb02.stations.graphenedb.com:24789/db/data/',
                   username = 'derby',
                   password = 'xSj1GzPmqFWns6SYZo8V')
### Establish pick order (Dropbox or Googledocs)
### Eliminate last week's picks
