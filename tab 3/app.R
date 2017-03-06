#
# Education & Income
#

library(shiny)
library(dplyr)
library(plotly)
library(ggplot2)

setwd("~/Documents/UW/Courses/2016-17/Winter/INFO_498/info498/tab 3")

# demo.data <- read.csv("../data/demo.csv")
# weights.data <- read.csv("../data/weights.csv")
# mj.data <- read.csv("../data/mjdata.csv")
# 
# demo.data <- select(demo.data, CASEID, EDUCCAT2, IRPINC3, IRFAMIN3)
# weights.data <- select(weights.data, CASEID, ANALWT_C, VESTR)
# mj.data <- select(mj.data, CASEID, MJEVER)
# 
# # Convert (1) Yes --> 1, (2) No --> 2
# mj.data$MJEVER <- as.character(mj.data$MJEVER)
# mj.data$MJEVER[mj.data$MJEVER == "(1) Yes"] <- 1
# mj.data$MJEVER[mj.data$MJEVER == "(2) No"] <- 2
# mj.data$MJEVER <- as.factor(mj.data$MJEVER)
# 
# data <- left_join(mj.data, demo.data, by = "CASEID") %>% 
#   left_join(weights.data, by = "CASEID")
# 
# write.csv(data, file = "../data/tab3-data.csv")
# 
# data <- read.csv("../data/tab3-data.csv")
# 
# # EDUCCAT2
# data$EDUCCAT2[data$EDUCCAT2 == 1] <- "Less than high school"
# data$EDUCCAT2[data$EDUCCAT2 == 2] <- "High school graduate"
# data$EDUCCAT2[data$EDUCCAT2 == 3] <- "Some college"
# data$EDUCCAT2[data$EDUCCAT2 == 4] <- "College graduate"
# data$EDUCCAT2[data$EDUCCAT2 == 5] <- "12 to 17 year olds"
# 
# # IRPINC3
# data$IRPINC3[data$IRPINC3 == 1] <- "Less than $10,000 (Including Loss)"
# data$IRPINC3[data$IRPINC3 == 2] <- "$10,000 - $19,999"
# data$IRPINC3[data$IRPINC3 == 3] <- "$20,000 - $29,999"
# data$IRPINC3[data$IRPINC3 == 4] <- "$30,000 - $39,999"
# data$IRPINC3[data$IRPINC3 == 5] <- "$40,000 - $49,999"
# data$IRPINC3[data$IRPINC3 == 6] <- "$50,000 - $74,999"
# data$IRPINC3[data$IRPINC3 == 7] <- "$75,000 or more"
# 
# # IRFAMIN3
# data$IRFAMIN3[data$IRFAMIN3 == 1] <- "Less than $10,000 (Including Loss)"
# data$IRFAMIN3[data$IRFAMIN3 == 2] <- "$10,000 - $19,999"
# data$IRFAMIN3[data$IRFAMIN3 == 3] <- "$20,000 - $29,999"
# data$IRFAMIN3[data$IRFAMIN3 == 4] <- "$30,000 - $39,999"
# data$IRFAMIN3[data$IRFAMIN3 == 5] <- "$40,000 - $49,999"
# data$IRFAMIN3[data$IRFAMIN3 == 6] <- "$50,000 - $74,999"
# data$IRFAMIN3[data$IRFAMIN3 == 7] <- "$75,000 or more"
# 
# user.data <- filter(data, MJEVER == 1)
# nonuser.data <- filter(data, MJEVER == 2)
# 
# user.sd <- svydesign(id=~1, strata=~VESTR, weights=~ANALWT_C, data=user.data)
# nonuser.sd <- svydesign(id=~1, strata=~VESTR, weights=~ANALWT_C, data=nonuser.data)
# 
# edu.user <- svytable(~EDUCCAT2, user.sd)
# edu.nonuser <- svytable(~EDUCCAT2, nonuser.sd)
# edu.user <- as.data.frame(edu.user)
# edu.nonuser <- as.data.frame(edu.nonuser)
# edu.user$Usage <- 1
# edu.nonuser$Usage <- 0
# edu.data <- bind_rows(edu.user, edu.nonuser)
# write.csv(edu.data, "../data/tab3/education.csv")
# 
# inc.user <- svytable(~IRPINC3, user.sd)
# inc.nonuser <- svytable(~IRPINC3, nonuser.sd)
# inc.user <- as.data.frame(inc.user)
# inc.nonuser <- as.data.frame(inc.nonuser)
# inc.user$Usage <- 1
# inc.nonuser$Usage <- 0
# inc.data <- bind_rows(inc.user, inc.nonuser)
# write.csv(inc.data, "../data/tab3/income.csv")
# 
# famin.user <- svytable(~IRFAMIN3, user.sd)
# famin.nonuser <- svytable(~IRFAMIN3, nonuser.sd)
# famin.user <- as.data.frame(famin.user)
# famin.nonuser <- as.data.frame(famin.nonuser)
# famin.user$Usage <- 1
# famin.nonuser$Usage <- 0
# famin.data <- bind_rows(famin.user, famin.nonuser)
# write.csv(famin.data, "../data/tab3/famincome.csv")

education <- read.csv("../data/tab3/education.csv")
education <- education[, 2:4]
income <- read.csv("../data/tab3/income.csv")
income <- income[, 2:4]
famincome <- read.csv("../data/tab3/famincome.csv")
famincome <- famincome[, 2:4]

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Looking at Education and Income Level"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        radioButtons("category", "Category",
                    choices = c("Education", "Individual Income", "Family Income"),
                    selected = "Education")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$distPlot <- renderPlot({
     if (input$category == "Education") {
       data <- education
     } else if (input$category == "Individual Income") {
       data <- income
     } else if (input$category == "Family Income") {
       data <- famincome
     }

     nonuser <- filter(data, Usage == 0)
     user <- filter(data, Usage == 1)
     x <- nonuser[, 1]
     y0 <- nonuser[, 2]
     y1 <- user[, 2]

     # draw the plot
     plot_ly(data, x =~x, y =~y0, type = 'bar', name = 'Non-Users') %>%
       add_trace(y = ~y1, name = 'Users') %>%
       layout(yaxis = list(title = 'Frequency'), barmode = 'group')
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

