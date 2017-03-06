### UI of second tab
### by Landon Young
### 
### This tab of the shiny app will be looking at correlations between 
### marijuana users' self-reported mental health and general health 

fluidPage(
  titlePanel("Demographics of Marijuana Users"),
  sidebarLayout(
    sidebarPanel(
      selectInput("question", label = h3("Questions for Users"), 
                  choices = c("Age first used?" = 'MJAGE', "How long since used?" = 'MJREC', "Last 30 days?" = 'MJDAY30A'), 
                  selected = 'MJAGE'),
      
      radioButtons("demo", label = h3("Demographic of Interest"),
                   choices = c("Age" = 'factor(CATAG6)', "Sex" = 'IRSEX', "Race" = 'factor(NEWRACE2)'), 
                   selected = 'factor(CATAG6)'),
      
      conditionalPanel(condition = "input.question == 'MJAGE'",
        sliderInput("agerange", label = h3("Age Range"), min = 0, max = 80, value = c(0, 30))
      )
    ),
  
    mainPanel(
    plotlyOutput(outputId = "demo_plot")
    )
  )
)