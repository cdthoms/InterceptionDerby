library(shiny)
library(shinysky)
library(ggplot2)
library(mailR)

shinyServer(function(input, output, session) {
  
  getData = reactive({
    input$qb
  })
  
  output$data = reactive({
    getData()
  })
  
  output$qbintrate = renderDataTable({
    qb_int_list[[1]]
  })
  
  output$teamintrate = renderDataTable({
    team_int_list[[1]]
  })
  
  output$schedule = renderDataTable({
    sched[sched$WEEK>=currentWeek,]
  })
  
  output$qbbar = renderGvis({
    return(barchart)
  })
  
  output$standings = renderGvis({
    return(weeklytotals)
  })
  
#   output$standingsplot = renderGvis({
#     return(gvisMerge(barchart,weeklytotals))
#   })
  
  output$qbchoice = renderUI({
    switch(input$name,
           CT = select2Input('qb',paste0('Hello ',
                                         input$name,
                                         '. Please make your week ',
                                         currentWeek,
                                         ' selections. You have pick #',
                                         info[info$week==currentWeek&info$name=='CT','pickOrder'],
                                         ' this week. Make sure to select ',
                                         2*info[info$week==currentWeek&info$name=='CT','pickOrder'],
                                         ' QBs in ranked order. ',
                                         'Last week you selected ',info[info$week==currentWeek-1&info$name=='CT',4],' and ',
                                         info[info$week==currentWeek-1&info$name=='CT',6],'. Exclude them from your selections.'),
                             choices = qbframe$Name),
           Mike = select2Input('qb',paste0('Hello ',
                                           input$name,
                                           '. Please make your week ',
                                           currentWeek,
                                           ' selections. You have pick #',
                                           info[info$week==currentWeek&info$name=='Mike','pickOrder'],
                                           ' this week. Make sure to select ',
                                           2*info[info$week==currentWeek&info$name=='Mike','pickOrder'],
                                           ' QBs in ranked order. ',
                                           'Last week you selected ',info[info$week==currentWeek-1&info$name=='Mike',4],' and ',
                                           info[info$week==currentWeek-1&info$name=='Mike',6],'. Exclude them from your selections.'),
                               choices = qbframe$Name),
           Dangerous = select2Input('qb',paste0('Hello ',
                                                input$name,
                                                '. Please make your week ',
                                                currentWeek,
                                                ' selections. You have pick #',
                                                info[info$week==currentWeek&info$name=='Dangerous','pickOrder'],
                                                ' this week. Make sure to select ',
                                                2*info[info$week==currentWeek&info$name=='Dangerous','pickOrder'],
                                                ' QBs in ranked order. ',
                                                'Last week you selected ',info[info$week==currentWeek-1&info$name=='Dangerous',4],' and ',
                                                info[info$week==currentWeek-1&info$name=='Dangerous',6],'. Exclude them from your selections.'),
                                    choices = qbframe$Name),
           Burson = select2Input('qb',paste0('Hello ',
                                             input$name,
                                             '. Please make your week ',
                                             currentWeek,
                                             ' selections. You have pick #',
                                             info[info$week==currentWeek&info$name=='Burson','pickOrder'],
                                             ' this week. Make sure to select ',
                                             2*info[info$week==currentWeek&info$name=='Burson','pickOrder'],
                                             ' QBs in ranked order. ',
                                             'Last week you selected ',info[info$week==currentWeek-1&info$name=='Burson',4],' and ',
                                             info[info$week==currentWeek-1&info$name=='Burson',6],'. Exclude them from your selections.'),
                                 choices = qbframe$Name))
    
  })
  
  output$selections = renderUI({
    if(input$submitPicks != 0) {
      input$submitPicks
      isolate({
        submitTime = Sys.time()
        st = paste('You have submitted your picks at: ',submitTime)
        header = paste('Your pick order is: ')
        pick1 = paste('1:',getData()[2],getData()[1])
        pick2 = paste('2:',getData()[4],getData()[3])
        if (!is.na(getData()[6])) {
          pick3 = paste('3:',getData()[6],getData()[5])
        } else {
          pick3 = 'NA'
        }
        if (!is.na(getData()[8])) {
          pick4 = paste('4:',getData()[8],getData()[7])
        } else {
          pick4 = 'NA'
        }
        if (!is.na(getData()[10])) {
          pick5 = paste('5:',getData()[10],getData()[9])
        } else {
          pick5 = 'NA'
        }
        if (!is.na(getData()[12])) {
          pick6 = paste('6:',getData()[12],getData()[11])
        } else {
          pick6 = 'NA'
        }
        if (!is.na(getData()[14])) {
          pick7 = paste('7:',getData()[14],getData()[13])
        } else {
          pick7 = 'NA'
        }
        if (!is.na(getData()[16])) {
          pick8 = paste('8:',getData()[16],getData()[15])
        } else {
          pick8 = 'NA'
        }
        
        createNode(graph,'Picks',
                   name = input$name,
                   week = currentWeek,
                   year = currentYear,
                   time = as.character(submitTime),
                   pickOrder = info[info$week==currentWeek&info$name==input$name,'pickOrder'],
                   pick1 = pick1,
                   pick2 = pick2,
                   pick3 = pick3,
                   pick4 = pick4,
                   pick5 = pick5,
                   pick6 = pick6,
                   pick7 = pick7,
                   pick8 = pick8)
        if (cypher(graph,paste('match (n:Picks {week:',currentWeek,',year:',currentYear,'}) return count(distinct n.name)'))[1,1]==4) {
          ### Take the most recent picks for each person
          ### query goes here (Still need to figure out the max submission)
          ### Return picks for player 1
          firstpicks = substring(cypher(graph,paste('match (n:Picks {week:',currentWeek,
                                                    ',pickOrder:1,year:',currentYear,'}) return n.pick1,n.pick2'))[1,1:2],4)
          secondpicks = substring(cypher(graph,paste('match (n:Picks {week:',currentWeek,
                                                     ',pickOrder:2,year:',currentYear,'}) return n.pick1,n.pick2,n.pick3,n.pick4'))[1,1:4],4)
          thirdpicks = substring(cypher(graph,paste('match (n:Picks {week:',currentWeek,
                                                    ',pickOrder:3,year:',currentYear,'}) return n.pick1,n.pick2,n.pick3,n.pick4,n.pick5,n.pick6'))[1,1:6],4)
          fourthpicks = substring(cypher(graph,paste('match (n:Picks {week:',currentWeek,
                                                     ',pickOrder:4,year:',currentYear,'}) return n.pick1,n.pick2,n.pick3,n.pick4,n.pick5,n.pick6,n.pick7,n.pick8'))[1,1:8],4)
          picks1 = firstpicks
          picks2 = secondpicks[!(secondpicks %in% firstpicks)][1:2]
          picks3v1 = thirdpicks[!(thirdpicks %in% picks1)]
          picks3 = picks3v1[!(picks3v1 %in% picks2)][1:2]
          picks4v1 = fourthpicks[!(fourthpicks %in% picks1)]
          picks4v2 = picks4v1[!(picks4v1 %in% picks2)]
          picks4 = picks4v2[!(picks4v2 %in% picks3)][1:2]
          ### Email picks
          emailBody = paste0('<html>Here are the selections for week ',currentWeek,':','<br><br><b>',
                             info[info$week==currentWeek&info$pickOrder==1,'name'],'</b>: ',picks1[1],' and ',picks1[2],'<br><b>',
                             info[info$week==currentWeek&info$pickOrder==2,'name'],'</b> : ',picks2[1],' and ',picks2[2],'<br><b>',
                             info[info$week==currentWeek&info$pickOrder==3,'name'],'</b> : ',picks3[1],' and ',picks3[2],'<br><b>',
                             info[info$week==currentWeek&info$pickOrder==4,'name'],'</b> : ',picks4[1],' and ',picks4[2],'<br>'
          )
          
          derbylist = c('cdthoms@gmail.com')
          send.mail(from = 'intderby@gmail.com',
                    to = derbylist,
                    subject = paste0('Week ',currentWeek,' INT Derby Selections'),
                    body = emailBody,
                    html = T,
                    smtp = list(host.name = 'smtp.gmail.com',port=465,user.name='intderby',passwd='interceptions',ssl=T),
                    authenticate = T,
                    send = T
          )
        }
        output = paste(st,header,pick1,pick2,pick3,pick4,pick5,pick6,pick7,pick8,sep='<br/>')
        HTML(output)
        ### Add check here for the number of unique picks made
        ### If the number of unique picks is 4, then take the most recent submissions
      })  
    } else {
      NULL
    }
  })
  
})