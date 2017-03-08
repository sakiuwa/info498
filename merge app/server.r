source("men-gen-health.r")
source("demographics.r")
source("drawGraph.R")
source("educ-income.r")

shinyServer(function(input, output) {
  
  ########## IRIS' DEMO TAB #############
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
        plot.data, x = ~x, y = ~y, type = "bar", name = "Information", marker = list(color = 'rgba(83,122,85, 0.6)', 
                                                                                     line = list(color = 'rgba(83,122,85, 1.0)', width = 1))
      ) %>% layout(yaxis = list(title = ""), xaxis = list(title = ""), barmode = 'group')
    } else if (input$use == "Non-user") {
      x <- plot.data %>% filter(Usage == 2)
      x <- x[,1]
      y <- plot.data %>% filter(Usage == 2)
      y <- y[,2]
      plot_ly(
        plot.data, x = ~x, y = ~y, type = "bar", name = "Information", marker = list(color = 'rgba(159,190,160, 0.6)', 
                                                                                     line = list(color = 'rgba(159,190,160, 1.0)', width = 1))
      ) %>% layout(yaxis = list(title = ""), xaxis = list(title = ""), barmode = 'group')
    } else {
      x <- unique(plot.data[,1])
      y1 <- (plot.data %>% filter(Usage == "1") %>% select(Freq))
      y2 <- (plot.data %>% filter(Usage == "2") %>% select(Freq))
      y <- (data.frame(y1, y2))
      
      plot_ly(y, x=~x, y=~Freq, type = "bar", name= "User", marker = list(color = 'rgba(83,122,85, 0.6)', 
                                                                          line = list(color = 'rgba(83,122,85, 1.0)', width = 1))) %>%
        add_trace(y =~Freq.1, name="Non-User", marker = list(color = 'rgba(159,190,160, 0.6)', 
                                                             line = list(color = 'rgba(159,190,160, 1.0)', width = 1))) %>%
        layout(yaxis = list(title = ""), xaxis = list(title = ""), barmode = "stack", hovermode = "closest") 
    }
    
  })
  
  
  ###### LANDON'S DEMO TAB ###############
  
  # Reactive function to choose particular graph depending on the selected question and demographic.
  plot <- reactive({
    if(input$question == "MJAGE") { # If "Age first used?" selected
      
      # Deals with the slider input and adjusts data accordingly
      slidevec <- input$agerange
      slidemin <- slidevec[1]
      slidemax <- slidevec[2]
      slider.data <- filter(users.only, MJAGE >= slidemin, MJAGE <= slidemax)
      
      if(input$demo == "CATAG6") { # If Age demographic is selected
        data <- read.csv("data/age_age.csv") %>% filter(options >= slidemin, options <= slidemax)
        plot <- plot_ly(data, x=~options, y=~x1, type = "bar", name="12-17", marker = list(color = '426144')) %>%
          add_trace(y =~x2, name = "18-25", marker = list(color = '537A55')) %>%
          add_trace(y =~x3, name = "26-34", marker = list(color = '7AA27B')) %>%
          add_trace(y =~x4, name = "35-49", marker = list(color = '9FBEA0')) %>%
          add_trace(y =~x5, name = "50-64", marker = list(color = 'CDE0CE')) %>% 
          add_trace(y =~x6, name = "65+", marker = list(color = 'E5F1E5')) %>% 
          layout(yaxis = list(title = "# of respondents"), xaxis = list(title = "Age first used marijuana"), barmode = "stack", hovermode = "closest")
        return(plot)
      } else if(input$demo == "IRSEX") { # If Sex demographic is selected
        data <- read.csv("data/age_sex.csv") %>% filter(options >= slidemin, options <= slidemax)
        plot <- plot_ly(data, x=~options, y=~x1, type = "bar", name="Male", marker = list(color = '537A55')) %>%
          add_trace(y =~x2, name = "Female", marker = list(color = 'CDE0CE')) %>%
          layout(yaxis = list(title = "# of respondents"), xaxis = list(title = "Age first used marijuana"), barmode = "stack", hovermode = "closest")
        return(plot)
      } else { # If Race demographic is selected
        data <- read.csv("data/age_race.csv") %>% filter(options >= slidemin, options <= slidemax)        
        plot <- plot_ly(data, x=~options, y=~x1, type = "bar", name="White", marker = list(color = '314933')) %>%
          add_trace(y =~x2, name = "Black", marker = list(color = '426144')) %>%
          add_trace(y =~x3, name = "Native American", marker = list(color = '537A55')) %>%
          add_trace(y =~x4, name = "Pacific Islander", marker = list(color = '7AA27B')) %>%
          add_trace(y =~x5, name = "Asian", marker = list(color = '9FBEA0')) %>% 
          add_trace(y =~x6, name = "More than one Race", marker = list(color = 'CDE0CE')) %>% 
          add_trace(y =~x7, name = "Hispanic", marker = list(color = 'E5F1E5')) %>% 
          layout(yaxis = list(title = "# of respondents"), xaxis = list(title = "Age first used marijuana"), barmode = "stack", hovermode = "closest")
        return(plot)
      }
    } else if(input$question == "MJREC") { # If "How long since used?" selected
      if(input$demo == "CATAG6") {
        data <- read.csv("data/rec_age.csv")
        plot <- plot_ly(data, x=~options, y=~x1, type = "bar", name="12-17", marker = list(color = '426144')) %>%
          add_trace(y =~x2, name = "18-25", marker = list(color = '537A55')) %>%
          add_trace(y =~x3, name = "26-34", marker = list(color = '7AA27B')) %>%
          add_trace(y =~x4, name = "35-49", marker = list(color = '9FBEA0')) %>%
          add_trace(y =~x5, name = "50-64", marker = list(color = 'CDE0CE')) %>% 
          add_trace(y =~x6, name = "65+", marker = list(color = 'E5F1E5')) %>% 
          layout(yaxis = list(title = "National %"), xaxis = list(title = "Time since last used marijuana"), barmode = "stack", hovermode = "closest")
        return(plot)
      } else if(input$demo == "IRSEX") {
        data <- read.csv("data/rec_sex.csv")
        plot <- plot_ly(data, x=~options, y=~x1, type = "bar", name="Male", marker = list(color = '537A55')) %>%
          add_trace(y =~x2, name = "Female", marker = list(color = 'CDE0CE')) %>% 
          layout(yaxis = list(title = "National %"), xaxis = list(title = "Time since last used marijuana"), barmode = "stack", hovermode = "closest")
        return(plot)
      } else {
        data <- read.csv("data/rec_race.csv")
        plot <- plot_ly(data, x=~options, y=~x1, type = "bar", name="White", marker = list(color = '314933')) %>%
          add_trace(y =~x2, name = "Black", marker = list(color = '426144')) %>%
          add_trace(y =~x3, name = "Native American", marker = list(color = '537A55')) %>%
          add_trace(y =~x4, name = "Pacific Islander", marker = list(color = '7AA27B')) %>%
          add_trace(y =~x5, name = "Asian", marker = list(color = '9FBEA0')) %>% 
          add_trace(y =~x6, name = "More than one Race", marker = list(color = 'CDE0CE')) %>% 
          add_trace(y =~x7, name = "Hispanic", marker = list(color = 'E5F1E5')) %>% 
          layout(yaxis = list(title = "National %"), xaxis = list(title = "Time since last used marijuana"), barmode = "stack", hovermode = "closest")
        return(plot)
      }
    } else { # If "Last 30 days?" selected
      if(input$demo == "CATAG6") {
        data <- read.csv("data/day_age.csv")
        plot <- plot_ly(data, x=~options, y=~x1, type = "bar", name="12-17", marker = list(color = '426144')) %>%
          add_trace(y =~x2, name = "18-25", marker = list(color = '537A55')) %>%
          add_trace(y =~x3, name = "26-34", marker = list(color = '7AA27B')) %>%
          add_trace(y =~x4, name = "35-49", marker = list(color = '9FBEA0')) %>%
          add_trace(y =~x5, name = "50-64", marker = list(color = 'CDE0CE')) %>% 
          add_trace(y =~x6, name = "65+", marker = list(color = 'E5F1E5')) %>% 
          layout(yaxis = list(title = "National count, based on weights"), xaxis = list(title = "Days used in last 30 days"), barmode = "stack", hovermode = "closest")
        return(plot)
      } else if(input$demo == "IRSEX") {
        data <- read.csv("data/day_sex.csv")
        plot <- plot_ly(data, x=~options, y=~x1, type = "bar", name="Male", marker = list(color = '537A55')) %>%
          add_trace(y =~x2, name = "Female", marker = list(color = 'CDE0CE')) %>% 
          layout(yaxis = list(title = "National count, based on weights"), xaxis = list(title = "Days used in last 30 days"), barmode = "stack", hovermode = "closest")
        return(plot)
      } else {
        data <- read.csv("data/day_race.csv")
        plot <- plot_ly(data, x=~options, y=~x1, type = "bar", name="White", marker = list(color = '314933')) %>%
          add_trace(y =~x2, name = "Black", marker = list(color = '426144')) %>%
          add_trace(y =~x3, name = "Native American", marker = list(color = '537A55')) %>%
          add_trace(y =~x4, name = "Pacific Islander", marker = list(color = '7AA27B')) %>%
          add_trace(y =~x5, name = "Asian", marker = list(color = '9FBEA0')) %>% 
          add_trace(y =~x6, name = "More than one Race", marker = list(color = 'CDE0CE')) %>% 
        add_trace(y =~x7, name = "Hispanic", marker = list(color = 'E5F1E5')) %>% 
          layout(yaxis = list(title = "National count, based on weights"), xaxis = list(title = "Days used in last 30 days"), barmode = "stack", hovermode = "closest")
        return(plot)
      }
    }
  })
  
  output$demo_plot <- renderPlotly({
    # Calls reactive function above to output the correct plot
    plot()
    
  })
  
  ############ SAKI'S SOCIO TAB ##############
  
  output$distPlot <- renderPlotly({
    if (input$category == "educ") {
      data <- education
      plot_ly(data, x =~EDUCCAT2, y =~Non_Users, type = 'bar', name = 'Non-Users', text=~EDUCCAT2, marker = list(color = 'rgba(159,190,160, 0.6)', 
                                                                                                                 line = list(color = 'rgba(159,190,160, 1.0)', width = 1))) %>%
        add_trace(y = ~Users, name = 'Users', marker = list(color = 'rgba(83,122,85, 0.6)', 
                                                            line = list(color = 'rgba(83,122,85, 1.0)', width = 1))) %>%
        layout(title = paste("Looking at Marijuana Usage Distribution by Education Level", input$category, sep = " "), xaxis = list(title = input$category), yaxis = list(title = 'Percent by Group (%)'),
               barmode = 'group', hovermode = "closest")
      
    } else if (input$category == "indincome") {
      data <- income
      plot_ly(data, x=~labels,y =~Non_Users, type = 'bar', name = 'Non-Users', text=~labels, marker = list(color = 'rgba(159,190,160, 0.6)', 
                                                                                                           line = list(color = 'rgba(159,190,160, 1.0)', width = 1))) %>%
        add_trace(y = ~Users, name = 'Users', marker = list(color = 'rgba(83,122,85, 0.6)', 
                                                            line = list(color = 'rgba(83,122,85, 1.0)', width = 1))) %>%
        layout(title = paste("Looking at Marijuana Usage Distribution by Individual Income", input$category, sep = " "), xaxis = list(title = "Individual Income",  showticklabels = FALSE), yaxis = list(title = 'Percent by Group (%)'),
               barmode = 'group', hovermode = "closest")
      
    } else if (input$category == "famincome") {
      data <- famincome
      plot_ly(data, x=~labels,y =~Non_Users, type = 'bar', name = 'Non-Users', text=~labels, marker = list(color = 'rgba(159,190,160, 0.6)', 
                                                                                                           line = list(color = 'rgba(159,190,160, 1.0)', width = 1))) %>%
        add_trace(y = ~Users, name = 'Users', marker = list(color = 'rgba(83,122,85, 0.6)', 
                                                            line = list(color = 'rgba(83,122,85, 1.0)', width = 1))) %>%
        layout(title = paste("Looking at Marijuana Usage Distribution by Family Income", input$category, sep = " "), xaxis = list(title = "Family Income",  showticklabels = FALSE), yaxis = list(title = 'Percent by Group (%)'),
               barmode = 'group', hovermode = "closest")
      
    }
  })
  
  ######### BAO'S TABS #############################
  
  output$questionPlot <- renderPlotly({
    drawPlot(input$type, input$questions, input$unit)
    
  })
  
  output$suicidalPlot <- renderPlotly({
    drawSuicidePlot(input$suicide, input$unit2)
  })
  
  # Attributal Risk Calculations
  output$calc <- renderUI({
    question <- input$suicide
    data <- read.csv(paste0("data/",question,".csv"))
    if (input$suicide == "think") {
      type <- "Suicidal Thinking"
    } else if (input$suicide == "plan") {
      type <- "Suicidal Planning"
    } else {
      type <- "Suicidal Attemp "
    }
    out.exp <- data[1,6]
    out.no.exp <- data[1,7] 
    no.out.exp <- data[2,6] 
    no.out.no.exp <- data[2,7] 
    total.out <- out.exp + out.no.exp 
    total.no.out <- no.out.exp + no.out.no.exp 
    ar <- (out.exp/total.out) - (no.out.exp/total.no.out)
    ar.people <- round(ar*100, 0)
    prop.ar <- round(ar / (out.exp/total.out), 2)
    
    
    HTML(
      paste0("<h2>Attributal Risk Calculations</h3>","<p>*We will assume survey data represents the incidences of drugs use in the US</p>,
      <pre>
Exposed: Exposed to marijuana use
Outcome: Answering 'Yes' to suicidal behavior questions
      
Incidence attributal to exposure in the exposed group (AR) = Incidence in Exposed - Incidence in Unexposed
Proportion of incidence attributal to exposure in the exposed group = AR / Incidence in Exposed
      
Outcome with exposure = <b>", out.exp,"</b>    Outcome without Exposure = <b>", out.no.exp,"</b>
No Outcome with exposure = <b>", no.out.exp,"</b>	   No Outcome without Exposure = <b>", no.out.no.exp,"</b>
Total # of Outcome = <b>", total.out,"</b>    Total # of No Outcome = <b>", total.no.out,"</b>
      
Atrributal Risk Due to using Marijuana = <b>(", out.exp,"/",total.out, ")</b> - <b>(", no.out.exp, "/", total.no.out, ")</b> = <b>", round(ar, 2), "</b> --> <b>", ar.people, " per 100 people</b>
% of Attributal Risk due to using Marijuana = <b>((", out.exp,"/", total.out,") - <b>(", no.out.exp,"/",total.no.out,"))</b> / <b>((",out.exp,"/",total.out,"))</b> = <b>",prop.ar," --> ",round(prop.ar * 100,0),"%</b>

<b>",round(prop.ar * 100,0), "%</b> of the incidence in ", type ," in marijuana users is attributal to marijuana.          
       </pre>"
    ))
  })
  
})