#
# Education & Income
#

library(shiny)
library(dplyr)
library(plotly)
library(survey)

# setwd("~/Documents/UW/Courses/2016-17/Winter/INFO_498/info498/tab 3")

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
# data$IRPINC3[data$IRPINC3 == 1] <- "1"
# data$IRPINC3[data$IRPINC3 == 2] <- "2"
# data$IRPINC3[data$IRPINC3 == 3] <- "3"
# data$IRPINC3[data$IRPINC3 == 4] <- "4"
# data$IRPINC3[data$IRPINC3 == 5] <- "5"
# data$IRPINC3[data$IRPINC3 == 6] <- "6"
# data$IRPINC3[data$IRPINC3 == 7] <- "7"
# 
# # IRFAMIN3
# data$IRFAMIN3[data$IRFAMIN3 == 1] <- "1"
# data$IRFAMIN3[data$IRFAMIN3 == 2] <- "2"
# data$IRFAMIN3[data$IRFAMIN3 == 3] <- "3"
# data$IRFAMIN3[data$IRFAMIN3 == 4] <- "4"
# data$IRFAMIN3[data$IRFAMIN3 == 5] <- "5"
# data$IRFAMIN3[data$IRFAMIN3 == 6] <- "6"
# data$IRFAMIN3[data$IRFAMIN3 == 7] <- "7"
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
# edu.user$Percentage <- prop.table(edu.user$Freq) * 100
# edu.nonuser$Percentage <- prop.table(edu.nonuser$Freq) * 100
# edu.data <- bind_rows(edu.user, edu.nonuser)
# write.csv(edu.data, "../data/tab3/education.csv")
# 
# inc.user <- svytable(~IRPINC3, user.sd)
# inc.nonuser <- svytable(~IRPINC3, nonuser.sd)
# inc.user <- as.data.frame(inc.user)
# inc.nonuser <- as.data.frame(inc.nonuser)
# inc.user$Usage <- 1
# inc.nonuser$Usage <- 0
# inc.user$Percentage <- prop.table(inc.user$Freq) * 100
# inc.nonuser$Percentage <- prop.table(inc.nonuser$Freq) * 100
# inc.data <- bind_rows(inc.user, inc.nonuser)
# write.csv(inc.data, "../data/tab3/income.csv")
# 
# famin.user <- svytable(~IRFAMIN3, user.sd)
# famin.nonuser <- svytable(~IRFAMIN3, nonuser.sd)
# famin.user <- as.data.frame(famin.user)
# famin.nonuser <- as.data.frame(famin.nonuser)
# famin.user$Usage <- 1
# famin.nonuser$Usage <- 0
# famin.user$Percentage <- prop.table(famin.user$Freq) * 100
# famin.nonuser$Percentage <- prop.table(famin.nonuser$Freq) * 100
# famin.data <- bind_rows(famin.user, famin.nonuser)
# write.csv(famin.data, "../data/tab3/famincome.csv")





education <- read.csv("../data/tab3/education.csv")
education <- education[, 2:5]
income <- read.csv("../data/tab3/income.csv")
income <- income[, 2:5]
famincome <- read.csv("../data/tab3/famincome.csv")
famincome <- famincome[, 2:5]

education <- education %>% 
  select(EDUCCAT2, Usage, Percentage) %>% 
  spread(Usage, Percentage)
names(education)[2] <- "Non_Users"
names(education)[3] <- "Users"

income <- income %>% 
  select(IRPINC3, Usage, Percentage) %>% 
  spread(Usage, Percentage)
names(income)[2] <- "Non_Users"
names(income)[3] <- "Users"

famincome <- famincome %>% 
  select(IRFAMIN3, Usage, Percentage) %>% 
  spread(Usage, Percentage)
names(famincome)[2] <- "Non_Users"
names(famincome)[3] <- "Users"


# Key for Income Levels
income.key <- c("Less than $10,000", "$10,000-$19,999", "$20,000-$29,999", "$30,000-$39,999", "$40,000-$49,999", "$50,000-$74,999", "$75,000 or more")
key <- matrix(income.key, ncol = 7, byrow = TRUE)
colnames(key) <- c(1:7)
rownames(key) <- "Income Level"


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Looking at Education and Income Level"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        radioButtons("category", "Category",
                    choices = c("Education Level", "Individual Income", "Family Income"),
                    selected = "Education Level")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("distPlot"),
         tableOutput("tbl")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   output$distPlot <- renderPlotly({
     if (input$category == "Education Level") {
       data <- education
     } else if (input$category == "Individual Income") {
       data <- income
     } else if (input$category == "Family Income") {
       data <- famincome
     }

     # draw the plot
     plot_ly(data, x =~data[, 1], y =~Non_Users, type = 'bar', name = 'Non-Users') %>%
       add_trace(y = ~Users, name = 'Users') %>%
       layout(title = paste("Looking at Marijuana Usage Distribution by", input$category, sep = " "), xaxis = list(title = input$category), yaxis = list(title = 'Percent by Group (%)'), barmode = 'group')
     
   })
   output$tbl <- renderTable({
     if  (input$category == "Individual Income" || input$category == "Family Income") {
       key
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

