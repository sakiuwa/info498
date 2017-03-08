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
        data <- group_by(slider.data, MJAGE, CATAG6) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJAGE, y=~count, color=~CATAG6, type="bar") %>% layout(barmode='stack', xaxis=list(title="Age first used marijuana"), yaxis= list(title="# of respondents")))
        
      } else if(input$demo == "IRSEX") { # If Sex demographic is selected
        data <- group_by(slider.data, MJAGE, IRSEX) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJAGE, y=~count, color=~IRSEX, type="bar") %>% layout(barmode="stack", xaxis=list(title="Age first used marijuana"), yaxis= list(title="# of respondents")))
        
      } else { # If Race demographic is selected
        data <- group_by(slider.data, MJAGE, NEWRACE2) %>% summarise(count = n()) %>% mutate(perc = round(count/sum(count)*100, 2))
        return(plot_ly(data, x=~MJAGE, y=~count, color=~NEWRACE2, type="bar") %>% layout(barmode="stack", xaxis=list(title="Age first used marijuana"), yaxis= list(title="# of respondents")))
      }
    } else if(input$question == "MJREC") { # If "How long since used?" selected
      if(input$demo == "CATAG6") {
        data <- group_by(users.only, MJREC, CATAG6) %>% summarise(natcount = sum(ANALWT_C)) %>% mutate(natperc = round(natcount/sum(natcount)*100, 2))
        return(plot_ly(data, x=~MJREC, y=~natperc, color=~CATAG6, type="bar") %>% layout(barmode="stack", xaxis=list(title="Time since last used marijuana"), yaxis=list(title="National %")))
      } else if(input$demo == "IRSEX") {
        data <- group_by(users.only, MJREC, IRSEX) %>% summarise(natcount = sum(ANALWT_C)) %>% mutate(natperc = round(natcount/sum(natcount)*100, 2))
        return(plot_ly(data, x=~MJREC, y=~natperc, color=~IRSEX, type="bar") %>% layout(barmode="stack", xaxis=list(title="Time since last used marijuana"), yaxis=list(title="National %")))
      } else {
        data <- group_by(users.only, MJREC, NEWRACE2) %>% summarise(natcount = sum(ANALWT_C)) %>% mutate(natperc = round(natcount/sum(natcount)*100, 2))
        return(plot_ly(data, x=~MJREC, y=~natperc, color=~NEWRACE2, type="bar") %>% layout(barmode="stack", xaxis=list(title="Time since last used marijuana"), yaxis=list(title="National %")))
      }
    } else { # If "Last 30 days?" selected
      if(input$demo == "CATAG6") {
        data <- group_by(users.only, MJDAY30A, CATAG6) %>% summarise(natcount = round(sum(ANALWT_C), 0))
        return(plot_ly(data, x=~MJDAY30A, y=~natcount, color=~CATAG6, type="bar", hoverinfo='text', text= ~paste('Count: ', format(natcount, big.mark=",", scientific=FALSE))) %>% layout(barmode="stack", xaxis=list(title="Days used in last 30 days"), yaxis=list(title="National count, based on weights")))
      } else if(input$demo == "IRSEX") {
        data <- group_by(users.only, MJDAY30A, IRSEX) %>% summarise(natcount = round(sum(ANALWT_C), 0)) 
        return(plot_ly(data, x=~MJDAY30A, y=~natcount, color=~IRSEX, type="bar", hoverinfo='text', text= ~paste('Count: ', format(natcount, big.mark=",", scientific=FALSE))) %>% layout(barmode="stack", xaxis=list(title="Days used in last 30 days"), yaxis=list(title="National count, based on weights")))
      } else {
        data <- group_by(users.only, MJDAY30A, NEWRACE2) %>% summarise(natcount = round(sum(ANALWT_C), 0)) 
        return(plot_ly(data, x=~MJDAY30A, y=~natcount, color=~NEWRACE2, type="bar", hoverinfo='text', text= ~paste('Count: ', format(natcount, big.mark=",", scientific=FALSE))) %>% layout(barmode="stack", xaxis=list(title="Days used in last 30 days"), yaxis=list(title="National count, based on weights")))
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
  
  
  
  output$questionPlot <- renderPlotly({
    drawPlot(input$type, input$questions, input$unit)
    
  })
  
  output$suicidalPlot <- renderPlotly({
    drawSuicidePlot(input$suicide, input$unit2)
  })
  
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
    out.exp <- data[1,6] #1464
    out.no.exp <- data[1,7] #623    
    no.out.exp <- data[2,6] #19541
    no.out.no.exp <- data[2,7] #19800
    total.out <- out.exp + out.no.exp #2087
    total.no.out <- no.out.exp + no.out.no.exp #39341
    ar <- (out.exp/total.out) - (no.out.exp/total.no.out)
    ar.people <- round(ar*100, 0)
    prop.ar <- round(ar / (out.exp/total.out), 2)
    
    
    HTML(
      paste0("<h2>Atrributal Risk Calculations</h3>","<p>*We will assume survey data represents the incidences of drugs use in the US</p>,
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

<b>",round(prop.ar * 100,0), "%</b> of the incidence in ", type ,"in marijuana users is attributal to marijuana.          
       </pre>"
    ))
  })
  
})