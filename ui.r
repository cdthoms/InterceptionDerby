library(shiny)
library(shinysky)
library(ggplot2)
library(markdown)

shinyUI(navbarPage(title = 'INT Derby',
                   
                   theme = 'bootstrap.css',
                   collapsable = TRUE,
                   
                   ###
                   ### Tab - Picks
                   ###
                   
                   tabPanel('Picks',pageWithSidebar(
                     headerPanel('Weekly Selections'),
                     sidebarPanel(
                       selectInput('name','Name:',choices = c('CT','Mike','Burson','Dangerous')),
                       conditionalPanel(
                         'input.name != NULL',
                         uiOutput('qbchoice')   
                       ),
                       actionButton('submitPicks','Submit Picks')
                     ),
                     ### Show selections, show interactive tables of INT by team defense, show QB Int totals overall
                     ### Show standings, show winnings
                     mainPanel(
                       htmlOutput('selections')
                     ))),
                   
                   ### Display the schedule
                   tabPanel('Schedule',
                            HTML('<h3>2014 NFL Schedule</h3>'),
                            dataTableOutput('schedule')),
                   
                   ### Include a list of current interception totals for QBs as well as Defenses
                   navbarMenu('Research',
                              tabPanel('QB INT Standings'),
                              tabPanel('Defensive INT Standings')),
                   
                   
                   ### Results graphs, standings
                   tabPanel('Standings')
                         
))