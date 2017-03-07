#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)

# data <- read.csv("../data/tab1-data.csv")
# weights <- read.csv("../data/weights.csv")
# data <- left_join(data, weights, by = "CASEID")
# data$X <- NULL
# 
# data$NEWRACE2[data$NEWRACE2 == 1] <- "White"
# data$NEWRACE2[data$NEWRACE2 == 2] <- "Black/Afr Am"
# data$NEWRACE2[data$NEWRACE2 == 3] <- "Native Am/AK Native "
# data$NEWRACE2[data$NEWRACE2 == 4] <- "Native HI/Other Pac Isl"
# data$NEWRACE2[data$NEWRACE2 == 5] <- "Asian"
# data$NEWRACE2[data$NEWRACE2 == 6] <- "More than one race"
# data$NEWRACE2[data$NEWRACE2 == 7] <- "Hispanic"
# 
# data$IRSEX[data$IRSEX == 1] <- "Male"
# data$IRSEX[data$IRSEX == 2] <- "Female"
# 
# data$CATAG6[data$CATAG6 == 1] <- "12-17 Years Old"
# data$CATAG6[data$CATAG6 == 2] <- "18-25 Years Old"
# data$CATAG6[data$CATAG6 == 3] <- "26-34 Years Old"
# data$CATAG6[data$CATAG6 == 4] <- "35-49 Years Old"
# data$CATAG6[data$CATAG6 == 5] <- "50-64 Years Old"
# data$CATAG6[data$CATAG6 == 6] <- "65 or Older"
# 
# data.user <- data %>% filter(MJEVER == 1)
# data.nonuser <- data %>% filter(MJEVER == 2)
# 
# sd.user <- svydesign(id=~CASEID, strata=~VESTR, weights=~ANALWT_C, data=data.user)
# sd.nonuser <- svydesign(id=~CASEID, strata=~VESTR, weights=~ANALWT_C, data=data.nonuser)
# 
# newrace2.user <- svytable(~NEWRACE2, design=sd.user)
# irsex.user <- svytable(~IRSEX, design=sd.user)
# catag6.user <- svytable(~CATAG6, design=sd.user)
# 
# newrace2.nonuser <- svytable(~NEWRACE2, design=sd.nonuser)
# irsex.nonuser <- svytable(~IRSEX, design=sd.nonuser)
# catag6.nonuser <- svytable(~CATAG6, design=sd.nonuser)
# 
# newrace2.user <- as.data.frame(newrace2.user)
# newrace2.nonuser <- as.data.frame(newrace2.nonuser)
# newrace2.user[, "Usage"] <- 1
# newrace2.nonuser[, "Usage"] <- 2
# newrace2 <- bind_rows(newrace2.user, newrace2.nonuser, id = NULL)
# write.csv(newrace2, file = "newrace2.csv")
# 
# irsex.user <- as.data.frame(irsex.user)
# irsex.nonuser <- as.data.frame(irsex.nonuser)
# irsex.user[, "Usage"] <- 1
# irsex.nonuser[, "Usage"] <- 2
# irsex <- bind_rows(irsex.user, irsex.nonuser, id = NULL)
# write.csv(irsex, file = "irsex.csv")
# 
# catag6.user <- as.data.frame(catag6.user)
# catag6.nonuser <- as.data.frame(catag6.nonuser)
# catag6.user[, "Usage"] <- 1
# catag6.nonuser[, "Usage"] <- 2
# catag6 <- bind_rows(catag6.user, catag6.nonuser, id = NULL)
# write.csv(catag6, file = "catag6.csv")

irsex <- read.csv("../data/irsex.csv")
irsex[1] <- NULL
newrace2 <- read.csv("../data/newrace2.csv")
newrace2[1] <- NULL
catag6 <- read.csv("../data/catag6.csv")
catag6[1] <- NULL

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
 
   # Application title
   titlePanel("Marijuana User and Non-user Demographic Information"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         selectInput(inputId = "demographics",
                     label = "Demographics",
                     choices = c("Age", "Sex", "Race"),
                     selected = "Age"),
         
         selectInput(inputId = "use",
                     label = "Marijuana Usage",
                     choices = list("User", "Non-user", "Both"),
                     selected = "User")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("usePlot")
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
   output$usePlot <- renderPlotly({
     if (input$demographics == "Age") {
       plot.data <- catag6
     } else if (input$demographics == "Sex") {
       plot.data <- irsex
     } else if (input$demographics == "Race") {
       plot.data <- newrace2
     }
     
     if (input$use == "User") {
       x <- plot.data %>% filter(Usage == 1)
       x <- x[,1]
       y <- plot.data %>% filter(Usage == 1) 
       y <- y[,2]
       
       plot_ly(
         plot.data, x = ~x, y = ~y, type = "bar", name = "Information"
       )
     } else if (input$use == "Non-user") {
       x <- plot.data %>% filter(Usage == 2)
       x <- x[,1]
       y <- plot.data %>% filter(Usage == 2)
       y <- y[,2]
       
       plot_ly(
         plot.data, x = ~x, y = ~y, type = "bar", name = "Information"
       )
     } else {
       x <- plot.data[,1]
       y <- plot.data[,2]
       
       plot_ly(plot.data) %>%
         add_trace(data = plot.data, type = "bar", x = ~x, y = ~y) %>%
         layout(barmode = "stack")
     }
     
     # draw the plot
     # plot_ly(
     #   plot.data, x = ~x, y = ~y, type = "bar", name = "Information"
     # )
   })
})

# Run the application 
shinyApp(ui = ui, server = server)

