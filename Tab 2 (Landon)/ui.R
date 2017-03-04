### UI of second tab
### by Landon Young
### 
### This tab of the shiny app will be looking at correlations between 
### marijuana users' self-reported mental health and general health 

fluidPage(
  
  selectInput("question", label = h3("Questions for Users"), 
              choices = list("Age first used?" = 'MJAGE', "How long since used?" = 2, "Last 30 days?" = 3), 
              selected = 1),
  
  radioButtons("demo", label = h3("Demographic of Interest"),
               choices = list("Age" = 1, "Sex" = 2, "Race" = 3), 
               selected = 1),
  
  plotlyOutput(outputId = "demo_plot"),
  
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
  
)