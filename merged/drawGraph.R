drawPlot <- function(type, question, unit) {
  
  top_labels <- c("All of the time", "Most of the time", "Some of the time", "A little of the time", "None of the time")
  
  data <- read.csv(paste0("data/",type,".csv"))
  row <- data[2,]
  breakdown <- "Breakdown By Percentage"
  if (question == "nervous30") {
    row <- data[1,]
    data <- data[1,]
  } else if (question == "hopeless30") {
    data <- data[2,]
  } else if (question == "restless30") {
    row <- data[3,]
    data <- data[3,]
  }
  
  if (unit == "weighted") {
    data <- data %>% select("X1" = X6, "X2" = X7, "X3" = X8, "X4" = X9, "X5" = X10, "y" = y)
    breakdown <- "Breakdown By US Population in Millions (Weights Applied)"
  } else if (unit == "total") {
    data <- data %>% select("X1" = X11, "X2" = X12, "X3" = X13, "X4" = X14, "X5" = X15, "y" = y)
    breakdown <- "Breakdown By Survey Respondents"
  }
  
  position <- c(row$X1 / 2,  row$X1 +2  + row$X2 / 2, row$X1 + row$X2 + 7+ row$X3 / 2, row$X1 + row$X2 + row$X3 + 15+ row$X4 / 2,
                row$X1 + row$X2 + row$X3 + row$X4 + 10 + row$X5 / 2)
  
  plot <- plot_ly(data, x =~X1, y =~y, type = 'bar', orientation = 'h', name = "All of the time", 
                  marker = list(color = '537A55', 
                                line = list(color = 'rgb(248, 248, 249)', width = 1))) %>% 
    add_trace(x =~X2, name = "Most of the time", marker = list(color = '7AA27B')) %>%
    add_trace(x =~X3, name = "Some of the time", marker = list(color = '9FBEA0')) %>%
    add_trace(x =~X4, name = "A little of the time", marker = list(color = 'CDE0CE')) %>%
    add_trace(x =~X5, name = "None of the time", marker = list(color = 'E5F1E5')) %>% 
    layout(title = breakdown,
          xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.15, 1)),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           barmode = 'stack',
           paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           margin = list(l = 120, r = 10, t = 140, b = 80),
           showlegend = TRUE,
           hovermode = "closest") %>% 
    add_annotations(xref = 'paper', yref = "y", x = 0.14, y = ~y,
                    xanchor = 'right',
                    text = ~y,
                    font = list(family = 'Arial', size = 12,
                                color = '434343'),
                    showarrow = FALSE, align = 'right') 
  
  if (unit == "percent") {
    plot <- plot %>% add_annotations(xref = 'x', yref = 'y',
                                     x = ~X1 / 2 - 1, y = ~y,
                                     text = paste0(data[,"X1"], '%'),
                                     font = list(family = 'Arial', size = 12,
                                                 color = '434343'),
                                     showarrow = TRUE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 / 2, y = ~y,
                      text = paste0(data[,"X2"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 + X3 / 2, y = ~y,
                      text = paste0(data[,"X3"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 + X3 + X4 / 2, y = ~y,
                      text = paste0(data[,"X4"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 + X3 + X4 + X5 / 2, y = ~y,
                      text = paste0(data[,"X5"], '%'),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE)
  } else if (unit == "weighted") {
    plot <- plot %>% add_annotations(xref = 'x', yref = 'y',
                                     x = ~X1 / 2 - 1, y = ~y,
                                     text = round(data[,"X1"] / 1000000, 1),
                                     font = list(family = 'Arial', size = 12,
                                                 color = '434343'),
                                     showarrow = TRUE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 / 2, y = ~y,
                      text = round(data[,"X2"]/ 1000000, 1),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 + X3 / 2, y = ~y,
                      text = round(data[,"X3"]/ 1000000, 1),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 + X3 + X4 / 2, y = ~y,
                      text = round(data[,"X4"]/ 1000000, 1),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 + X3 + X4 + X5 / 2, y = ~y,
                      text = round(data[,"X5"]/ 1000000, 1),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE)
  } else {
    plot <- plot %>% add_annotations(xref = 'x', yref = 'y',
                                     x = ~X1 / 2 - 1, y = ~y,
                                     text = round(data[,"X1"], 0),
                                     font = list(family = 'Arial', size = 12,
                                                 color = '434343'),
                                     showarrow = TRUE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 / 2, y = ~y,
                      text = round(data[,"X2"], 0),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 + X3 / 2, y = ~y,
                      text = round(data[,"X3"],0),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 + X3 + X4 / 2, y = ~y,
                      text = round(data[,"X4"], 0),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE) %>% 
      add_annotations(xref = 'x', yref = 'y',
                      x = ~X1 + X2 + X3 + X4 + X5 / 2, y = ~y,
                      text = round(data[,"X5"], 0),
                      font = list(family = 'Arial', size = 12,
                                  color = '434343'),
                      showarrow = FALSE)
  }
  return(plot)
}

drawSuicidePlot <- function(question, unit) {
  data <- read.csv(paste0("data/",question,".csv"))
  breakdown <- "Breakdown By Percentage"
  if (unit == "weighted") {
    data <- data %>% select("X1" = X3, "X2" = X4, "y" = y)
    breakdown <- "Breakdown By US Population in Millions (Weights Applied)"
  } else if (unit == "total") {
    data <- data %>% select("X1" = X5, "X2" = X6, "y" = y)
    breakdown <- "Breakdown By Survey Respondents"
  }
  
  plot <- plot_ly(data, x=~X1, y =~y, type = 'bar', name = "User", 
                  marker = list(color = 'rgba(83,122,85, 0.6)', line = list(color = 'rgba(83,122,85, 1.0)', width = 3))) %>%
    add_trace(x = ~X2, name = 'Non-User', marker = list(color = 'rgba(159,190,160, 0.6)', line = list(color = 'rgba(159,190,160, 1.0)', width = 3))) %>%
    layout(title = breakdown,
           xaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE,
                        domain = c(0.15, 1)),
           yaxis = list(title = "",
                        showgrid = FALSE,
                        showline = FALSE,
                        showticklabels = FALSE,
                        zeroline = FALSE),
           barmode = 'stack',
           paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
           margin = list(l = 120, r = 10, t = 140, b = 80),
           showlegend = TRUE,
           hovermode = "closest")
    
    if (unit == "percent") {
      plot <- plot %>% add_annotations(xref = 'paper', yref = "y", x = 0, y = ~y,
                                       xanchor = 'right',
                                       text = ~y,
                                       font = list(family = 'Arial', size = 12,
                                                   color = '434343'),
                                       showarrow = FALSE, align = 'right') %>% 
        add_annotations(xref = 'x', yref = 'y',
                        x = ~X1 / 2, y = ~y,
                        text = paste0(data[,"X1"], '%'),
                        font = list(family = 'Arial', size = 12,
                                    color = '434343'),
                        showarrow = FALSE) %>% 
        add_annotations(xref = 'x', yref = 'y',
                        x = ~X1 + X2 / 2, y = ~y,
                        text = paste0(data[,"X2"], '%'),
                        font = list(family = 'Arial', size = 12,
                                    color = '434343'),
                        showarrow = FALSE)
    } else if (unit == "weighted") {
      plot <- plot %>% add_annotations(xref = 'paper', yref = "y", x = 0, y = ~y,
                                       xanchor = 'right',
                                       text = ~y,
                                       font = list(family = 'Arial', size = 12,
                                                   color = '434343'),
                                       showarrow = FALSE, align = 'right') %>% 
        add_annotations(xref = 'x', yref = 'y',
                        x = ~X1 / 2, y = ~y,
                        text = round(data[,"X1"]/ 1000000, 1),
                        font = list(family = 'Arial', size = 12,
                                    color = '434343'),
                        showarrow = FALSE) %>% 
        add_annotations(xref = 'x', yref = 'y',
                        x = ~X1 + X2 / 2, y = ~y,
                        text = round(data[,"X2"]/ 1000000, 1),
                        font = list(family = 'Arial', size = 12,
                                    color = '434343'),
                        showarrow = FALSE)
    } else {
      plot <- plot %>% add_annotations(xref = 'paper', yref = "y", x = 0, y = ~y,
                                       xanchor = 'right',
                                       text = ~y,
                                       font = list(family = 'Arial', size = 12,
                                                   color = '434343'),
                                       showarrow = FALSE, align = 'right') %>% 
        add_annotations(xref = 'x', yref = 'y',
                        x = ~X1 / 2, y = ~y,
                        text = round(data[,"X1"], 0),
                        font = list(family = 'Arial', size = 12,
                                    color = '434343'),
                        showarrow = FALSE) %>% 
        add_annotations(xref = 'x', yref = 'y',
                        x = ~X1 + X2 / 2, y = ~y,
                        text = round(data[,"X2"], 0),
                        font = list(family = 'Arial', size = 12,
                                    color = '434343'),
                        showarrow = FALSE)
    }
  
    
  return(plot)
}

calcTable <- function(question) {
  data <- read.csv(paste0("data/",question,".csv"))
  
}

# data <- think
# 
# exposed <- c(data[1,6], data[2,7])
# unexposed <- c(data[1,7], data[2,6])
# 
# df <- data.frame(exposed, unexposed)
# row.names(df) <- c("Yes", "No")
# 
# test <- drawSuicidePlot("try", "weighted")






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
