#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(readxl)
library(here)
cupp22 <- read_xlsx(here("FY22-District-Profile-Report.xlsx"),sheet = "District Data")
# https://education.ohio.gov/getattachment/Topics/Finance-and-Funding/School-Payment-Reports/District-Profile-Reports/FY2022-District-Profile-Report/FY22-District-Profile-Report.xlsx.aspx?lang=en-US


dist_perf <- read_xlsx(here("DISTRICT_HIGH_LEVEL_2122.xlsx"), sheet = "DISTRICT_OVERVIEW")
# https://eduprdreportcardstorage1.blob.core.windows.net/data-download-2022/DISTRICT_HIGH_LEVEL_2122.xlsx?sv=2020-08-04&ss=b&srt=sco&sp=rlx&se=2031-07-28T05:10:18Z&st=2021-07-27T21:10:18Z&spr=https&sig=nPOvW%2Br2caitHi%2F8WhYwU7xqalHo0dFrudeJq%2B%2Bmyuo%3D


dist_perf <- dist_perf %>% rename("IRN" = `District IRN`)

dist_perf <- dist_perf %>% 
  mutate(across(c(5,8:20,22),as.numeric))

data <- inner_join(cupp22,dist_perf)

cupp22 <- data

library(shiny)
library(shinydashboard)
library(plotly)

# Define UI for application that draws a histogram

header <- dashboardHeader(title = "2022 District Profile Reports & District Performance")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Plot Tab", tabName = "plot_tab1", icon = icon("school"))
  )
)

body <- dashboardBody(
  tabItems(
    tabItem(tabName = "plot_tab1",
            h1("Make Your Plot"),
            fluidRow(box(selectInput(inputId = "select_dist",label = "Choose School District(s) To be Highlighted",choices = unique(cupp22$District), multiple = T)),
                     box(selectInput(inputId = "select_x", label = "Choose X Axis Stat", choices = names(cupp22),selected = "Performance Index Percent"),width = 3),
                     box(selectInput(inputId = "select_y", label = "Choose Y Axis Stat", choices = names(cupp22),selected = "% of Economically Disadvantaged Students FY22"),width = 3)),
            fluidRow(box(title = "Plot",plotlyOutput("plot1",width = 798,height = 600),width = 6),
                     box(title = "Regression Output",verbatimTextOutput(outputId = "lmSummary"),width = 6)))
  )
)

ui <- dashboardPage(header,sidebar,body, skin = "green")

# Define server logic required to draw a histogram
server <- function(session, input, output) {
  
  output$plot1 <- renderPlotly({
    
    col1 <- sym(input$select_x)
    col2 <- sym(input$select_y)
    
    g <- ggplot(data = cupp22,
                aes(x =!! col1, y= !! col2, text = `District`))+
      geom_point()+
      geom_point(data = cupp22 %>% filter(District %in% c(input$select_dist)), color= "red")+
      theme_bw()
    
    ggplotly(g)
    
  })
  
  output$lmSummary <- renderPrint({
    fit <- lm(unlist(cupp22[, input$select_y])~ unlist(cupp22[, input$select_x]))
    names(fit$coefficients) <- c("Intercept", input$select_y)
    summary(fit)
  })



}

# Run the application 
shinyApp(ui = ui, server = server)
