### UI of second tab
### by Landon Young
### 
### This tab of the shiny app will be looking at correlations between 
### marijuana users' self-reported mental health and general health 

library(ggplot2)
library(dplyr)

health <- read.csv("./data/health.csv")
