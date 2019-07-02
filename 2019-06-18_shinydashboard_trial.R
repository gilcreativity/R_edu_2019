rm(list = ls())
###########################################
# 1. Structure 
# server.R
# ui.R
# description
# readme
# <other file>  : data, scripts, etc
# folder : must be named 'www' (images, CSS, .js, etc)
###########################################





###########################################
# server.R
###########################################
# load the required packages
library(shiny)
require(shinydashboard)
library(ggplot2)
library(dplyr)

# load data
recommendation <- read.csv('recommendation.csv',stringsAsFactors = F,header=T)


###########################################
# dashboardPage : 
#      dashboardHeader(),
#      dashboardSidebar(),
#      dashboardBody()
###########################################


###########################################
# dashboardHeader
###########################################
#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "R_edu_shinydashboard", disable = FALSE,
                          dropdownMenu(type = "messages",
                                       messageItem(
                                         from = "Sales Dept",
                                         message = "Sales are steady this month."
                                       ),
                                       messageItem(
                                         from = "New User",
                                         message = "How do I register?",
                                         icon = icon("question"),
                                         time = "13:45"
                                       ),
                                       messageItem(
                                         from = "Support",
                                         message = "The new server is ready.",
                                         icon = icon("life-ring"),
                                         time = "2014-12-01"
                                       )
                          ),
                          dropdownMenu(type = "notifications",
                                       notificationItem(
                                         text = "5 new users today",
                                         icon("users")
                                       ),
                                       notificationItem(
                                         text = "12 items delivered",
                                         icon("truck"),
                                         status = "success"
                                       ),
                                       notificationItem(
                                         text = "Server load at 86%",
                                         icon = icon("exclamation-triangle"),
                                         status = "warning"
                                       )
                          ),
                          dropdownMenu(type = "tasks", badgeStatus = "success",
                                       taskItem(value = 90, color = "green",
                                                "Documentation"
                                       ),
                                       taskItem(value = 17, color = "aqua",
                                                "Project X"
                                       ),
                                       taskItem(value = 75, color = "yellow",
                                                "Server deployment"
                                       ),
                                       taskItem(value = 80, color = "red",
                                                "Overall project"
                                       )
                          ))  



###########################################
# dashboarderSidebar
###########################################
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(disable = FALSE,
  sidebarMenu(id = "sidebar",
    menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Widgets", icon = icon("th"), tabName = "widgets",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("Contact-us", icon = icon("send",lib='glyphicon'), 
             href = "https://www.google.com")
  )
)


###########################################
# Body
###########################################
recommendation

# infoBox
frow1 <- fluidRow(
  # A static infoBox
  infoBox("New Orders", 10 * 2, icon = icon("credit-card"), fill = FALSE),
  # Dynamic infoBoxes
  infoBoxOutput("progressBox"),
  infoBoxOutput("approvalBox"))

frow0 <- fluidRow(
  box(width = 4, actionButton("count", "Increment progress"))
)


frow2 <- fluidRow(
  
  box(
    title = "Revenue per Account"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("revenuebyPrd", height = "300px")
  )

  ,box(
    title = "Revenue per Product"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,plotOutput("revenuebyRegion", height = "300px")
  )
)

frow3 <- fluidRow(
  box(title = "Timeseries", 
       status = "primary",
       solidHeader = TRUE,
       plotOutput("plot1", height = 250))
  
  ,box(title = "Inputs", 
       # status = "warning", 
       background = "black",
       solidHeader = TRUE,
    "Box content here", br(), "More box content",
    sliderInput("slider", "Slider input:", 1, 100, 50),
    textInput('text', 'Text input:')
  )
)
 
frow4 <- fluidRow(
  tabBox(
    title = "First tabBox",
    id = "tabset1", height = "250px",
    tabPanel("Tab1", "First tab content"),
    tabPanel("Tab2", "Tab conetent 2")
  )
  ,tabBox(
    side = "right", height = "250px",
    selected = "Tab3",
    tabPanel("Tab1", "Tab content 1"),
    tabPanel("Tab2", "Tab content 2"),
    tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
  )
)

frow5 <- fluidRow(
  tabBox(
    # Title can include an icon
    title = tagList(shiny::icon("gear"), "tabBox status"),
    tabPanel("Tab1",
             "Currently selected tab from first box:",
             verbatimTextOutput("tabset1Selected")
    ),
    tabPanel("Tab2", "Tab content 2")
  )
)



# combine the two fluid rows to make the body
body <- dashboardBody(frow1, frow0,
                      frow2, frow3, frow4, frow5)








#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='blue')






# create the server functions for the dashboard  
server <- function(input, output) { 
  
  #some data manipulation to derive the values of KPI boxes
  total.revenue <- sum(recommendation$Revenue)
  sales.account <- recommendation %>% group_by(Account) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  prof.prod <- recommendation %>% group_by(Product) %>% summarise(value = sum(Revenue)) %>% filter(value==max(value))
  
  
  #creating the valueBoxOutput content
  output$value1 <- renderValueBox({
    valueBox(
      formatC(sales.account$value, format="d", big.mark=',')
      ,paste('Top Account:',sales.account$Account)
      ,icon = icon("stats",lib='glyphicon')
      ,color = "purple")
    
    
  })
  
  
  
  output$value2 <- renderValueBox({
    
    valueBox(
      formatC(total.revenue, format="d", big.mark=',')
      ,'Total Expected Revenue'
      ,icon = icon("gbp",lib='glyphicon')
      ,color = "green")
    
  })
  
  
  
  output$value3 <- renderValueBox({
    
    valueBox(
      formatC(prof.prod$value, format="d", big.mark=',')
      ,paste('Top Product:',prof.prod$Product)
      ,icon = icon("menu-hamburger",lib='glyphicon')
      ,color = "yellow")
    
  })
  
  #creating the plotOutput content
  
  output$revenuebyPrd <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Product, y=Revenue, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Product") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Product") + labs(fill = "Region")
  })
  
  
  output$revenuebyRegion <- renderPlot({
    ggplot(data = recommendation, 
           aes(x=Account, y=Revenue, fill=factor(Region))) + 
      geom_bar(position = "dodge", stat = "identity") + ylab("Revenue (in Euros)") + 
      xlab("Account") + theme(legend.position="bottom" 
                              ,plot.title = element_text(size=15, face="bold")) + 
      ggtitle("Revenue by Region") + labs(fill = "Region")
  })
  
  set.seed(122)
  tdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- tdata[seq_len(input$slider)]
    plot.ts(data)
  })
  
  
}


shinyApp(ui, server)
