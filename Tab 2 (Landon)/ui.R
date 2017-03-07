### UI of second tab
### by Landon Young
### 
### This tab of the shiny app will be looking at correlations between 
### marijuana users' self-reported mental health and general health 

### Description
### This tab aims to show how marijuana users breakdown across age groups, sex, and race. We chose to focus
### on three questions given in the 2014 National Survey on Drug Use and Health: 
### 1. How old were you the first time you used marijuana or hashish?
### 2. How long has it been since you last used marijuana or hashish?
### and 3. During the past 30 days, on how many days did you use marijuana or hashish?
### For the first question, we show the number of respondents that reported each age, across demographics.
### For the second question, we show the national percentage (estimated using the survey weights for each respondent,
### given in the survey data) broken down across demographics for each response.
### For the third question, we show the national count (estimated using the survey weights for each respondent, given in the survey data)
### for each response, broken down across demographics.

fluidPage(
  # Title of the page
  titlePanel("Demographics of Marijuana Users"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
      
      # Input for user to select question of interest.
      selectInput("question", label = h3("Questions for Users"), 
                  choices = c("Age first used?" = 'MJAGE', "How long since used?" = 'MJREC', "Last 30 days?" = 'MJDAY30A'), 
                  selected = 'MJAGE'),
      
      # Input for user to choose demographic of interest
      radioButtons("demo", label = h3("Demographic of Interest"),
                   choices = c("Age" = 'CATAG6', "Sex" = 'IRSEX', "Race" = 'NEWRACE2'), 
                   selected = 'CATAG6'),
      
      # Input for user to select an age range; only applies to MJAGE question
      conditionalPanel(condition = "input.question == 'MJAGE'",
        sliderInput("agerange", label = h3("Age Range"), min = 0, max = 80, value = c(0, 30))
      )
    ),
  
    # Main panel where plot is shown
    mainPanel(
    plotlyOutput(outputId = "demo_plot")
    )
  )
)