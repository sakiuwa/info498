library(shiny)
library(shinythemes)
library(plotly)

shinyUI(
  navbarPage(title="INFO498C", theme = "mytheme.css",
             tabPanel("Home", id="home",
                      h1("INFO498C Population Health Metrics"),
                      h2("Final Project: Marijuanalytics"),
                      a(href="https://github.com/sakiuwa/info498", "Github Repo Can be found here"),
                      br(),
                      h2("Project Description"),
                      h3("Purpose"),
                      p("The purpose of our study is to explore the impact of marijuana use in the US population and bring awareness to the topic, especially as more states are moving to legalize marijuana. Specifically, we will be exploring how marijuana use breakdown across different demographics (age, sex, and race), education and income levels, and mental health state."),
                      p("Because we are simply exploring the possible correlation and patterns, we attempt to take a relatively neutral and unbiased approach when displaying and analyzing our data. We agreed on an interactive Shiny application as the deliverable for this study to give the users the freedom to explore the information on their own, allowing them to come up with their own opinions regarding the topic with the guidance of our tool. We hope this tool helps answer relevant questions and provide insight regarding marijuana use to policy makers and potential users."),
                      h3("Primary Questions:"),
                      tags$ul(tags$li("How does marijuana use break down across ", tags$b("age"), ", ", tags$b("sex"), ","," and ", tags$b("race?")),
                         tags$li("What correlations can be found between the self-reported ", tags$b("mental health")," and ", tags$b("emotional stability")," of marijuana users?"),
                         tags$li("What is the spread of marijuana users across varying ",tags$b("education")," and ", tags$b("income levels"),"?")
                      ),
                      h3("Dataset"),
                      p("The dataset we will be working with is from The Interuniversity Consortium for Political and Social Research (ICPSR), and is called the National Survey on Drug Use and Health. The data was collected in 2014 from across all 50 states and the District of Columbia. The mode of data collection was through audio computer-assisted self interview (ACASI), computer-assisted personal interview (CAPI), and computer-assisted self interview (CASI). The dataset can be accessed through the following link at the top of this page. The number of people surveyed through in this study was 67,901, however the number of records available in the public file is 55,271."),
                      br(),
                      p("Dataset Source can be found", tags$a(href="http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/36361", "here.")),
                      h3("Target Audience"),
                      p("The primary target audience will be marijuana users, as our hope is for them to understand the correlations between this drug use and the factors we are analyzing. Secondary target audiences include policy makers, health organizations, and the general public to raise awareness about our findings in order to help them make educated decisions."),
                      div(id="danke", img(src="dankest.png", width=700, height=300, alt= "Danke Memes"))),
             tabPanel("Overall Demographic Breakdown", id="demo1",
                        p("For this section, people will be able to explore demographic information about both marijuana users and non-users. The data being represented comes from the following question: Have you ever, even once, used marijuana or hashish? People who responded “yes” are represented as Users, and people who responded “no” are represented as Non-users under the Marijuana Usage section."),
                        br(),
                        p("The filter options: age, sex, race, are intended to provide a very general overview about two populations — those who have used marijuana and those who have not. This is important to keep in mind because those who have used marijuana may not continue to participate in the usage of the drug. The option 'both' under the section Marijuana Usage allows people to compare the proportions of those who have used marijuana against those who have not in order to receive a more complete understanding of how both proportions relate to the total population that responded to the question."),
                        br(),
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
              ),
             tabPanel("Marijuana Users Demographics", id="demo2",
                      p("This tab aims to show how marijuana users breakdown across age groups, sex, and race. We chose to focus on three questions given in the 2014 National Survey on Drug Use and Health:"),
                      tags$ol(
                        tags$li("How old were you the first time you used marijuana or hashish?"),
                        tags$li("How long has it been since you last used marijuana or hashish?"),
                        tags$li("During the past 30 days, on how many days did you use marijuana or hashish?")
                      ),
                      p("For the first question, we show the number of respondents that reported each age, across demographics."),
                      p("For the second question, we show the national percentage (estimated using the survey weights for each respondent, given in the survey data) broken down across demographics for each response."),
                      p("For the third question, we show the national count (estimated using the survey weights for each respondent, given in the survey data) for each response, broken down across demographics."),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("question", label = h3("Questions for Users"), 
                                      choices = c("Age first used?" = 'MJAGE', "How long since used?" = 'MJREC', "Last 30 days?" = 'MJDAY30A'), 
                                      selected = 'MJAGE'),
                          
                          radioButtons("demo", label = h3("Demographic of Interest"),
                                       choices = c("Age" = 'CATAG6', "Sex" = 'IRSEX', "Race" = 'NEWRACE2'), 
                                       selected = 'CATAG6'),
                          conditionalPanel(condition = "input.question == 'MJAGE'",
                                           sliderInput("agerange", label = h3("Age Range"), min = 0, max = 80, value = c(0, 30))
                          )
                        ),
                        mainPanel(
                          plotlyOutput(outputId = "demo_plot")
                        )
                      )    
             ),
             tabPanel("Education & Income", id="socio",
                      p("This tab explores possible correlation between marijuana usage and education or income levels. Select one of the three categories on the left to see how the usage distribution varies between marijuana users and nonusers. The table represents what percentage of users or nonusers fall under each category, which is differentiated by the different color bars. The percentages are separated by whether they have used marijuana before or have never used it in their life. For example, roughly 30% of the population that answered they have used marijuana are college graduates. Same logic applies to the other categories."),
                      br(),
                      p("To see the Individual Income and Family Income levels, hover over the colored bars."),
                      br(),
               sidebarLayout(
                 sidebarPanel(
                   radioButtons("category", "Category",
                                choices = c("Education" = 'educ', "Individual Income" = 'indincome', "Family Income" = 'famincome'),
                                selected = "educ")
                 ),
                 # Show a plot of the generated distribution
                 mainPanel(plotlyOutput('distPlot')
                 )
               )
             ),
             tabPanel("Suicidal Behaviors", id="suicidal",
                                 tags$p("This section of the site visualizes responses to questions about suicidal behavior between marijuana users and non-users. The three questions we focused on for this section were, “In the past 12 months, did you (1) seriously think about trying to kill yourself?, (2) make any plans to kill yourself?, or (3) try to kill yourself?”. Within each of these questions, you can explore the percentage of respondents who responded yes or no and were users or non-users, as well as the national counts (estimated using survey weights given in the survey data), and counts of the survey respondents. We have also given the attributable risk calculations found under the “calculations” tab."),
                                 tags$br(),
                      sidebarLayout(
                               sidebarPanel(
                                 selectInput("suicide", label = "In the past 12 months,",
                                             choices = c("Did you seriously think about trying to kill yourself?" = 'think',
                                                         "Did you make any plans to kill yourself?" = 'plan',
                                                         "Did you try to kill yourself?" = 'try'),
                                             selected = "thinkkill"
                                 ),
                                 conditionalPanel(condition = "input.tabs =='breakdown'",
                                 radioButtons("unit2", label = "Present Data In",
                                              choices = c("Percentage of Respondents" = 'percent', "US Population (WT)" = "weighted", "Survey Respondent" = 'total'),
                                              selected = "percent"
                                 ))),
                        mainPanel(
                          tabsetPanel(id="tabs",
                            tabPanel("breakdown", br(), plotlyOutput("suicidalPlot")),
                            tabPanel("calculations", br(), htmlOutput("calc"))
                          )
                        )
              )),
             tabPanel("Emotional Stability", id="health",
                          tags$p("This section of the site explores questions of mental health, visualizing their relationship to marijuana users and non-users. The three questions we focused on for this section were, “During the past 30 days, how often did you feel (1) restless?, (2) nervous?, and (3) hopeless?”. Answers to these questions ranged from “none of the time” to “all of the time”. 
Within these questions, you can explore how responses to them breakdown across marijuana users, non-users, and both. You can also see the data presented as percentages of respondents, counts of respondents, and national counts (estimated using survey weights given in the survey data).
                                 "),
                          tags$br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("questions", label = "During the past 30 days,",
                                      choices = c("All Questions" = 'all',
                                                  "How often did you feel nervous?" = 'nervous30',
                                                  "How often did you feel hopeless?" = 'hopeless30',
                                                  "How often did you feel restless?" = 'restless30'),
                                      selected = "all"
                          ),
                          radioButtons("type", label = "Filter By",
                                       choices = c("Both" = 'both', "User" = "user", "Non-User" = 'nonuser'),
                                       selected = "both"
                          ),
                          radioButtons("unit", label = "Present Data In",
                                       choices = c("Percentage of Respondents" = 'percent', "US Population (WT)" = "weighted", "Survey Respondent" = 'total'),
                                       selected = "percent"
                          )
                          
                        ),
                        mainPanel(plotlyOutput('questionPlot'), br())
                      )
                ),
             tabPanel('About Us', id = "about", align ="center",
               titlePanel('Meet our Tweet Team!'),
               p("INFO 498C Population Health Metrics Winter 17"),
               div(class = "outer",
                 div(class = "pics", img(src="Bao-pic.png", alt="Picture of Bao Dinh"), p(class="names" ,"Bao Dinh")),
                 div(class = "pics", img(src="Iris-pic.png", alt="Picture of Iris Sun"), p(class="names" ,"Iris Sun")),
                 div(class = "pics", img(src="Landon-pic.png", alt="Picture of Landon Young"), p(class="names" ,"Landon Young")),
                 div(class = "pics", img(src="Saki-pic.png", alt="Picure of Saki Uwagawa"), p(class="names" ,"Saki Uwagawa")))
             ),
             tags$head(
               tags$link(rel="shortcut icon", href="favicon.png")
             )
  )
)
