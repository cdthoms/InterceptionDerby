library(shiny)
library(shinysky)
library(ggplot2)

shinyServer(function(input, output, session) {
  
  getData = reactive({
    input$qb
  })
  
  output$data = reactive({
    getData()
  })
  
  output$schedule = renderDataTable({
    sched
  })
  
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
                                        ' QBs in ranked order.'),
                             choices = qbframe$Name),
           Mike = select2Input('qb',paste0('Hello ',
                                           input$name,
                                           '. Please make your week ',
                                           currentWeek,
                                           ' selections. You have pick #',
                                           info[info$week==currentWeek&info$name=='Mike','pickOrder'],
                                           ' this week. Make sure to select ',
                                           2*info[info$week==currentWeek&info$name=='Mike','pickOrder'],
                                           ' QBs in ranked order.'),
                               choices = qbframe$Name),
           Dangerous = select2Input('qb',paste0('Hello ',
                                                input$name,
                                                '. Please make your week ',
                                                currentWeek,
                                                ' selections. You have pick #',
                                                info[info$week==currentWeek&info$name=='Dangerous','pickOrder'],
                                                ' this week. Make sure to select ',
                                                2*info[info$week==currentWeek&info$name=='Dangerous','pickOrder'],
                                                ' QBs in ranked order.'),
                                    choices = qbframe$Name),
           Burson = select2Input('qb',paste0('Hello ',
                                             input$name,
                                             '. Please make your week ',
                                             currentWeek,
                                             ' selections. You have pick #',
                                             info[info$week==currentWeek&info$name=='Burson','pickOrder'],
                                             ' this week. Make sure to select ',
                                             2*info[info$week==currentWeek&info$name=='Burson','pickOrder'],
                                             ' QBs in ranked order.'),
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
          pick3 = NULL
        }
        if (!is.na(getData()[8])) {
          pick4 = paste('4:',getData()[8],getData()[7])
        } else {
          pick4 = NULL
        }
        if (!is.na(getData()[10])) {
          pick5 = paste('5:',getData()[10],getData()[9])
        } else {
          pick5 = NULL
        }
        if (!is.na(getData()[12])) {
          pick6 = paste('6:',getData()[12],getData()[11])
        } else {
          pick6 = NULL
        }
        if (!is.na(getData()[14])) {
          pick7 = paste('7:',getData()[14],getData()[13])
        } else {
          pick7 = NULL
        }
        if (!is.na(getData()[16])) {
          pick8 = paste('8:',getData()[16],getData()[15])
        } else {
          pick8 = NULL
        }
        output = paste(st,header,pick1,pick2,pick3,pick4,pick5,pick6,pick7,pick8,sep='<br/>')
        HTML(output)
      })  
    } else {
      NULL
    }
  })
  
})