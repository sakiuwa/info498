# setwd("~/Desktop/ICPSR_36361/")
# load( "./DS0001/36361-0001-Data.rda")
# setwd("~/Desktop/INFO498C/final-project/")

library(dplyr)
library(plotly)
library(reshape2)

# mj.health <- da36361.0001 %>% select(MJEVER, HEALTH2, DSTNRV30, DSTHOP30, DSTRST30, ADDPREV, SUICTHNK, SUICPLAN, SUICTRY, ANALWT_C)

# write.csv(mj.health, file = "data/mjhealth.csv")

mj.health <- read.csv("data/dont need/mjhealth.csv")
# 
# yes.data <- mj.health %>% filter(MJEVER == "(1) Yes")
# no.data <- mj.health %>% filter(MJEVER == "(2) No")
# 
# yes.total <- nrow(yes.data)
# no.total <- nrow(no.data)

# General Health (could have used table function)
# yes.ovr.health <- yes.data %>% filter(!is.na(HEALTH2)) %>% group_by(HEALTH2) %>% summarize(YTotal=n(),YPercent=round(YTotal/(yes.total-5) * 100, 1), YWTTotal=sum(ANALWT_C))
# no.ovr.health <- no.data %>% filter(!is.na(HEALTH2)) %>% group_by(HEALTH2) %>% summarize(NTotal=n(),NPercent=round(NTotal/(no.total-15) * 100, 1), NWTTotal=sum(ANALWT_C))
# both.health <- merge(yes.ovr.health, no.ovr.health, by="HEALTH2")
# write.csv(both.health, file = "data/both_health.csv")
# both.health <- read.csv("data/both_health.csv")

# Nervous last 30 Days (13600 weren't asked)
# yes.nervous30 <- yes.data %>% filter(!is.na(DSTNRV30)) %>% group_by(DSTNRV30) %>% summarize(YTotal=n(), YPercent=round(YTotal/(yes.total - 2437) *100,1), YWTTotal=sum(ANALWT_C))
# no.nervous30 <- no.data %>% filter(!is.na(DSTNRV30)) %>% group_by(DSTNRV30) %>% summarize(NTotal=n(), NPercent=round(NTotal /(no.total - 11400) *100, 1), NWTTotal=sum(ANALWT_C))
# both.nervous30 <- merge(yes.nervous30, no.nervous30, by="DSTNRV30")
# write.csv(both.nervous30, file = "data/both_nervous30.csv")
# both.nervous30 <- read.csv("data/both_nervous30.csv")

# y <- c('During the past 30 days,<br> how often did you feel nervous?', 'During the past 30 days,<br> how often did you feel hopeless?', 'During the past 30 days,<br> how often did you feel restless?')

# Users All Questions
# nervous <- c(both.nervous30$YPercent,both.nervous30$YWTTotal,both.nervous30$YTotal)
# hopeless <- c(both.hopeless30$TPercent,both.hopeless30$YWTTotal,both.hopeless30$YTotal)
# restless <- c(both.restless30$YPercent,both.restless30$YWTTotal,both.restless30$YTotal)
# all.user <- data.frame(t(data.frame(nervous, hopeless, restless)))
# all.user$y <- y
# write.csv(all.user, file = "data/allQuser.csv")
# all.user <- read.csv("data/allQuser.csv")

# Non-users All Questions
# nervous <- c(both.nervous30$NPercent,both.nervous30$NWTTotal,both.nervous30$NTotal)
# hopeless <- c(both.hopeless30$NPercent,both.hopeless30$NWTTotal,both.hopeless30$NTotal)
# restless <- c(both.restless30$NPercent,both.restless30$NWTTotal,both.restless30$NTotal)
# all.user <- data.frame(t(data.frame(nervous, hopeless, restless)))
# all.user$y <- y
# write.csv(all.user, file = "data/allQnonuser.csv")

# Both All Questions
# nervous.sum <- mj.health %>% filter(!is.na(DSTNRV30)) %>% group_by(DSTNRV30) %>% summarize(Total=n(), PopWT = sum(ANALWT_C))
# nervous.sum <- nervous.sum %>% mutate(Percent = round(Total/sum(Total) * 100, 1))
# 
# hopeless.sum <- mj.health %>% filter(!is.na(DSTHOP30)) %>% group_by(DSTHOP30) %>% summarize(Total=n(), PopWT = sum(ANALWT_C))
# hopeless.sum <- hopeless.sum %>% mutate(Percent = round(Total/sum(Total) * 100, 1))
# 
# restless.sum <- mj.health %>% filter(!is.na(DSTRST30)) %>% group_by(DSTRST30) %>% summarize(Total=n(), PopWT = sum(ANALWT_C))
# restless.sum <- restless.sum %>% mutate(Percent = round(Total/sum(Total) * 100, 1))
# 
# nervous <- c(nervous.sum$Percent, nervous.sum$PopWT, nervous.sum$Total)
# hopeless <- c(hopeless.sum$Percent, hopeless.sum$PopWT, hopeless.sum$Total)
# restless <- c(restless.sum$Percent, restless.sum$PopWT, restless.sum$Total)
# all.both <- data.frame(t(data.frame(nervous, hopeless, restless)))
# all.both$y <- y
# write.csv(all.both, file = "data/allQboth.csv")

# Hopeless last 30 Days
# yes.hopeless <- yes.data  %>% filter(!is.na(DSTHOP30)) %>% group_by(DSTHOP30) %>% summarize(YTotal=n(), TPercent=round(YTotal/(yes.total - 2430) *100,1), YWTTotal=sum(ANALWT_C))
# no.hopeless <- no.data  %>% filter(!is.na(DSTHOP30)) %>% group_by(DSTHOP30) %>% summarize(NTotal=n(), NPercent=round(NTotal /(no.total - 11420) *100, 1), NWTTotal=sum(ANALWT_C))
# both.hopeless30 <- merge(yes.hopeless, no.hopeless, by="DSTHOP30")
# write.csv(both.hopeless30, file = "data/both_hopeless30.csv")
# both.hopeless30 <- read.csv("data/both_hopeless30.csv")

# Restless last 30 Days
# yes.restless <- yes.data %>% filter(!is.na(DSTRST30)) %>% group_by(DSTRST30) %>% summarize(YTotal=n(), YPercent=round(YTotal/(yes.total - 2442) *100,1), YWTTotal=sum(ANALWT_C))
# no.restless <- no.data %>% filter(!is.na(DSTRST30)) %>%group_by(DSTRST30) %>% summarize(NTotal=n(), NPercent=round(NTotal /(no.total - 11447) *100, 1), NWTTotal=sum(ANALWT_C))
# both.restless30 <- merge(yes.restless, no.restless, by="DSTRST30")
# write.csv(both.restless30, file = "data/both_restless30.csv")
# both.restless30 <- read.csv("data/both_restless30.csv")


# user <- read.csv("data/user.csv")
# nonuser <- read.csv("data/nonuser.csv")
# both <- read.csv("data/both.csv")

# Suicidal Thoughts 
# suicide.think <- mj.health %>% filter(!is.na(MJEVER) && !is.na(SUICTHNK)) %>% select(MJEVER, SUICTHNK, ANALWT_C) %>% filter(!is.na(SUICTHNK))
# yes.think <- suicide.think %>% filter(SUICTHNK == "(1) Yes") %>%  group_by(MJEVER) %>% summarize(YTotal = n(), YWTTotal=sum(ANALWT_C))
# yes.think <- yes.think %>% filter(!is.na(MJEVER)) %>% mutate(YPercent=round(YTotal/sum(YTotal) * 100,1))
# no.think <- suicide.think %>% filter(SUICTHNK == "(2) No") %>% group_by(MJEVER) %>% summarize(NTotal = n(), NWTTotal=sum(ANALWT_C))
# no.think <- no.think %>% filter(!is.na(MJEVER)) %>% mutate(NPercent=round(NTotal/sum(NTotal) * 100,1))
# think.suicide <- merge(yes.think, no.think, by="MJEVER")
# 
# yes <- c(think.suicide$YPercent, think.suicide$YWTTotal, think.suicide$YTotal)
# no <- c(think.suicide$NPercent, think.suicide$NWTTotal, think.suicide$NTotal)
# think <- data.frame(t(data.frame(yes, no)))
# think$y <- c("Yes", "No")
# write.csv(think, file="data/think.csv")

# Suicidal Plans 
# suicide.plan <- mj.health %>% filter(!is.na(MJEVER) | !is.na(SUICTHNK)) %>% select(MJEVER, SUICPLAN, ANALWT_C) %>% filter(!is.na(SUICPLAN))
# yes.plan <- suicide.plan %>% filter(SUICPLAN == "(1) Yes") %>% group_by(MJEVER) %>% summarize(YTotal = n(), YWTTotal=sum(ANALWT_C))
# yes.plan <- yes.plan %>% filter(!is.na(MJEVER)) %>% mutate(YPercent=round(YTotal/sum(YTotal) * 100,1))
# no.plan <- suicide.plan %>% filter(SUICPLAN == "(2) No") %>% group_by(MJEVER) %>% summarize(NTotal = n(), NWTTotal=sum(ANALWT_C))
# no.plan <- no.plan %>% filter(!is.na(MJEVER)) %>% mutate(NPercent=round(NTotal/sum(NTotal) * 100,1))
# plan.suicide <- merge(yes.plan, no.plan, by="MJEVER")
# 
# yes <- c(plan.suicide$YPercent, plan.suicide$YWTTotal, plan.suicide$YTotal)
# no <- c(plan.suicide$NPercent, plan.suicide$NWTTotal, plan.suicide$NTotal)
# plan <- data.frame(t(data.frame(yes, no)))
# plan$y <- c("Yes", "No")
# write.csv(plan, file="data/plan.csv")

# Suical Actions
suicide.try <- mj.health %>% filter(!is.na(MJEVER) | !is.na(SUICTHNK)) %>% select(MJEVER, SUICTRY, ANALWT_C) %>% filter(!is.na(SUICTRY))
yes.try <- suicide.try %>% filter(SUICTRY == "(1) Yes") %>% group_by(MJEVER) %>% summarize(YTotal = n(), YWTTotal=sum(ANALWT_C))
yes.try <- yes.try %>% filter(!is.na(MJEVER)) %>% mutate(YPercent=round(YTotal/sum(YTotal) * 100,1))
no.try <- suicide.try %>% filter(SUICTRY == "(2) No") %>% group_by(MJEVER) %>% summarize(NTotal = n(), NWTTotal=sum(ANALWT_C))
no.try <- no.try %>% filter(!is.na(MJEVER)) %>% mutate(NPercent=round(NTotal/sum(NTotal) * 100,1))
try.suicide <- merge(yes.try, no.try, by="MJEVER")

yes <- c(try.suicide$YPercent, try.suicide$YWTTotal, try.suicide$YTotal)
no <- c(try.suicide$NPercent, try.suicide$NWTTotal, try.suicide$NTotal)
try <- data.frame(t(data.frame(yes, no)))
try$y <- c("Yes", "No")
write.csv(try, file="data/try.csv")

think <- read.csv("data/think.csv")
plan <- read.csv("data/plan.csv")
try <- read.csv("data/try.csv")




# plot <- plot_ly(think, x=~X1, y =~y, type = 'bar', name = "User", 
#   marker = list(color = 'rgba(83,122,85, 0.6)', line = list(color = 'rgba(83,122,85, 1.0)', width = 3))) %>%
#   add_trace(x = ~X2, name = 'Non-User', marker = list(color = 'rgba(159,190,160, 0.6)', line = list(color = 'rgba(159,190,160, 1.0)', width = 3))) %>%
#   layout(title = "",
#          xaxis = list(title = "",
#                       showgrid = FALSE,
#                       showline = FALSE,
#                       showticklabels = FALSE,
#                       zeroline = FALSE,
#                       domain = c(0.15, 1)),
#          yaxis = list(title = "",
#                       showgrid = FALSE,
#                       showline = FALSE,
#                       showticklabels = FALSE,
#                       zeroline = FALSE),
#          barmode = 'stack',
#          paper_bgcolor = 'rgb(248, 248, 255)', plot_bgcolor = 'rgb(248, 248, 255)',
#          margin = list(l = 120, r = 10, t = 140, b = 80),
#          showlegend = TRUE,
#          hovermode = "closest") %>% 
#   add_annotations(xref = 'paper', yref = "y", x = 0, y = ~y,
#                   xanchor = 'right',
#                   text = ~y,
#                   font = list(family = 'Arial', size = 12,
#                               color = '434343'),
#                   showarrow = FALSE, align = 'right') %>% 
#   add_annotations(xref = 'x', yref = 'y',
#                   x = ~X1 / 2, y = ~y,
#                   text = paste0(think[,"X1"], '%'),
#                   font = list(family = 'Arial', size = 12,
#                               color = '434343'),
#                   showarrow = FALSE) %>% 
#   add_annotations(xref = 'x', yref = 'y',
#                   x = ~X1 + X2 / 2, y = ~y,
#                   text = paste0(think[,"X2"], '%'),
#                   font = list(family = 'Arial', size = 12,
#                               color = '434343'),
#                   showarrow = FALSE)
# 
