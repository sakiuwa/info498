library(shiny)
library(shinythemes)
library(plotly)

shinyUI(
  navbarPage(title="INFO498C", theme = "mytheme.css",
             tabPanel("Home", id="home",
                      titlePanel("INFO498C Final Project"),
                      a(href="https://github.com/sakiuwa/info498", "Github Repo Can be found here"),
                      br(),
                      h2("Project Description"),
                      h3("Purpose"),
                      p("The purpose of our study is to explore the impact of marijuana use in the US population and bring awareness to the topic especially now that more states are moving to legalize marijuana. We are also looking to see if there are any correlations between self-reported mental health and that of those who used marijuana."),
                      h3("Primary Questions:"),
                      tags$ul(tags$li("How does marijuana use break down across ", tags$b("age"), ", ", tags$b("sex"), ","," and ", tags$b("race?")),
                         tags$li("What correlations can be found between the self-reported ", tags$b("mental health")," and ", tags$b("general health")," of marijuana users?"),
                         tags$li("What is the spread of marijuana users across varying ",tags$b("education")," and ", tags$b("income levels"),"?")
                      ),
                      h3("Dataset"),
                      p("The dataset we will be working with is from The Interuniversity Consortium for Political and Social Research (ICPSR), and is called the National Survey on Drug Use and Health. The data was collected in 2014 from across all 50 states and the District of Columbia. The mode of data collection was through audio computer-assisted self interview (ACASI), computer-assisted personal interview (CAPI), and computer-assisted self interview (CASI). The dataset can be accessed through the following link at the top of this page. The number of people surveyed through in this study was 67,901, however the number of records available in the public file is 55,271."),
                      br(),
                      p("Dataset Source can be found", tags$a(href="http://www.icpsr.umich.edu/icpsrweb/ICPSR/studies/36361", "here.")),
                      h3("Target Audience"),
                      p("The primary target audience will be marijuana users, as our hope is for them to understand the correlations between this drug use and the factors we are analyzing. Secondary target audiences include policy makers, health organizations, and the general public to raise awareness about our findings in order to help them make educated decisions.")
              ),
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
             tabPanel("Marijuana User Demographics", id="demo2",
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
             tabPanel("Income x Education", id="socio",
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
                                 tags$p("Lorem ipsum dolor sit amet, an eos decore noluisse, epicurei definitionem et has. Nec in blandit detraxit perpetua. Ut sea dolorem tacimates. No diam nominavi voluptaria sea. Et nihil admodum quo, pro cu tota laoreet posidonium.
                                        
                                        Est brute possit reformidans no, quodsi audiam has no. Ea libris labore vulputate has, autem torquatos persecuti no eos. Albucius maiestatis moderatius mei eu. Et harum recusabo petentium eam. Minimum singulis vim ad, legimus necessitatibus no sea, id apeirian lobortis mea. In augue postea qui.
                                        
                                        Movet persius partiendo mel ne, ferri scribentur suscipiantur ne eum. Vel id enim congue nusquam, eu dico iudico eos. Copiosae electram consetetur ex vel. Apeirian delicata laboramus te mel, his vide saepe consequuntur id, cu omnis accusamus prodesset vel. Vis cu laoreet pericula similique, eu alii denique dissentiunt quo, option corrumpit id quo. Cu accusamus corrumpit vel. Id iudico volutpat usu, copiosae vituperata te nec.
                                        
                                        Duo vero lorem partiendo cu, pro iuvaret imperdiet ea. Ne mea atqui albucius instructior. No vim exerci temporibus, tota nostro repudiandae duo id, eam alia intellegebat et. Ad vim omnis quaestio quaerendum, eum tota mundi aliquam no.
                                        
                                        Sea eirmod tritani ne, his molestie oportere ne. Duo vide congue delicatissimi at, in his eius nostro signiferumque, his hinc tollit cu. Mel hinc cetero aeterno ut. Ex sed diam discere volutpat, id per maiorum sapientem voluptaria, nam alienum perpetua eu. Mei nibh concludaturque no, putant constituam sadipscing ut sed, cum ad altera dicunt commodo. Pro alii clita volutpat ut."),
                                 tags$br(),
                      sidebarLayout(
                               sidebarPanel(
                                 selectInput("suicide", label = "In the past 12 months,",
                                             choices = c("Did you seriously think about trying to kill yourself?" = 'think',
                                                         "Did you make any plans to kill yourself?" = 'plan',
                                                         "Did you try to kill yourself?" = 'try'),
                                             selected = "thinkkill"
                                 ),
                                 radioButtons("unit2", label = "Present Data In",
                                              choices = c("Percentage of Respondents" = 'percent', "US Population (WT)" = "weighted", "Survey Respondent" = 'total'),
                                              selected = "percent"
                                              
                                 )),
                        mainPanel(
                          tabsetPanel(
                            tabPanel("Breakdown", br(), plotlyOutput("suicidalPlot")),
                            tabPanel("Calculations", br(), htmlOutput("calc"))
                          )
                        )
                        
              )),
             tabPanel("Mental Health Question", id="health",
                          tags$p("Lorem ipsum dolor sit amet, an eos decore noluisse, epicurei definitionem et has. Nec in blandit detraxit perpetua. Ut sea dolorem tacimates. No diam nominavi voluptaria sea. Et nihil admodum quo, pro cu tota laoreet posidonium.
  
                                 Est brute possit reformidans no, quodsi audiam has no. Ea libris labore vulputate has, autem torquatos persecuti no eos. Albucius maiestatis moderatius mei eu. Et harum recusabo petentium eam. Minimum singulis vim ad, legimus necessitatibus no sea, id apeirian lobortis mea. In augue postea qui.
                                 
                                 Movet persius partiendo mel ne, ferri scribentur suscipiantur ne eum. Vel id enim congue nusquam, eu dico iudico eos. Copiosae electram consetetur ex vel. Apeirian delicata laboramus te mel, his vide saepe consequuntur id, cu omnis accusamus prodesset vel. Vis cu laoreet pericula similique, eu alii denique dissentiunt quo, option corrumpit id quo. Cu accusamus corrumpit vel. Id iudico volutpat usu, copiosae vituperata te nec.
                                 
                                 Duo vero lorem partiendo cu, pro iuvaret imperdiet ea. Ne mea atqui albucius instructior. No vim exerci temporibus, tota nostro repudiandae duo id, eam alia intellegebat et. Ad vim omnis quaestio quaerendum, eum tota mundi aliquam no.
                                 
                                 Sea eirmod tritani ne, his molestie oportere ne. Duo vide congue delicatissimi at, in his eius nostro signiferumque, his hinc tollit cu. Mel hinc cetero aeterno ut. Ex sed diam discere volutpat, id per maiorum sapientem voluptaria, nam alienum perpetua eu. Mei nibh concludaturque no, putant constituam sadipscing ut sed, cum ad altera dicunt commodo. Pro alii clita volutpat ut."),
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
                        mainPanel(plotlyOutput('questionPlot'), br()
                                  
                        )
                    
                      ),
                      fluidRow(
                        tags$div(class = "info", 
                                 tags$p("Lorem ipsum dolor sit amet, an eos decore noluisse, epicurei definitionem et has. Nec in blandit detraxit perpetua. Ut sea dolorem tacimates. No diam nominavi voluptaria sea. Et nihil admodum quo, pro cu tota laoreet posidonium.
                                        
                                        Est brute possit reformidans no, quodsi audiam has no. Ea libris labore vulputate has, autem torquatos persecuti no eos. Albucius maiestatis moderatius mei eu. Et harum recusabo petentium eam. Minimum singulis vim ad, legimus necessitatibus no sea, id apeirian lobortis mea. In augue postea qui.
                                        
                                        Movet persius partiendo mel ne, ferri scribentur suscipiantur ne eum. Vel id enim congue nusquam, eu dico iudico eos. Copiosae electram consetetur ex vel. Apeirian delicata laboramus te mel, his vide saepe consequuntur id, cu omnis accusamus prodesset vel. Vis cu laoreet pericula similique, eu alii denique dissentiunt quo, option corrumpit id quo. Cu accusamus corrumpit vel. Id iudico volutpat usu, copiosae vituperata te nec.
                                        
                                        Duo vero lorem partiendo cu, pro iuvaret imperdiet ea. Ne mea atqui albucius instructior. No vim exerci temporibus, tota nostro repudiandae duo id, eam alia intellegebat et. Ad vim omnis quaestio quaerendum, eum tota mundi aliquam no.
                                        
                                        Sea eirmod tritani ne, his molestie oportere ne. Duo vide congue delicatissimi at, in his eius nostro signiferumque, his hinc tollit cu. Mel hinc cetero aeterno ut. Ex sed diam discere volutpat, id per maiorum sapientem voluptaria, nam alienum perpetua eu. Mei nibh concludaturque no, putant constituam sadipscing ut sed, cum ad altera dicunt commodo. Pro alii clita volutpat ut."),
                                 tags$br()
                                 )
                        )
                      # sidebarLayout(
                      #   sidebarPanel(
                      #     selectInput("suicide", label = "During the past 30 days,",
                      #                 choices = c("All Questions" = 'all',
                      #                             "How often did you feel nervous?" = 'nervous30',
                      #                             "How often did you feel hopeless?" = 'hopeless30',
                      #                             "How often did you feel restless?" = 'restless30'),
                      #                 selected = "all"
                      #     )
                      #     
                      #   ),
                      #   mainPanel(plotlyOutput('suicidePlot')
                      #   )
                      # )
   
                )
  )
)
