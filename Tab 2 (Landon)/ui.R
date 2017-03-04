### UI of second tab
### by Landon Young
### 
### This tab of the shiny app will be looking at correlations between 
### marijuana users' self-reported mental health and general health 

# Load in libraries
library(plotly)
library(ggplot2)
library(dplyr)
library(shiny)

# Read in marijuana and demographics data from survey 
mjdata <- read.csv("data/mjdata.csv")
demographics <- read.csv("data/demo.csv")

# Filter users of marijuana only, and select relevant columns
demo <- select(demographics, CASEID, NEWRACE2, IRSEX, CATAG6)
users.only <- filter(mjdata, MJEVER == "(1) Yes") %>% select(CASEID, MJAGE, MJREC, MJDAY30A)
users.only <- left_join(users.only, demo, by = "CASEID")

g <- ggplot(users.only, aes(input$question)) + geom_bar(aes(colour = input$age))

fluidPage(
  
  # Copy the line below to make a select box 
  selectInput("question", label = h3("Questions for Users"), 
              choices = list("Age first used?" = MJAGE, "How long since used?" = MJREC, "Last 30 days?" = MJDAY30A), 
              selected = 1),
  
  radioButtons("demo", label = h3("Demographic of Interest"),
               choices = list("Age" = CATAG6, "Sex" = IRSEX, "Race" = NEWRACE2), 
               selected = a),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
  
)