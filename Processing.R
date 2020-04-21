library(dplyr)
library(ggplot2)
library(reshape2)
sraw <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"), header = TRUE)
craw <- read.csv(url("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"), header = TRUE)

Dates <- unique(sraw$date)
States <- unique(sraw$state)

state.cases <- data.frame(date)
for(i in c(1:length(States))){
  temp <- filter(sraw, state == States[i])
  temp <- select(temp, date, cases)
  state.cases <- merge(state.cases, temp, by = "date", all = T)
  names(state.cases)[i+1] <- paste(as.character(States[i]))
}
state.cases[is.na(state.cases)] <- 0

state.deaths <- data.frame(date)
for(i in c(1:length(States))){
  temp <- filter(sraw, state == States[i])
  temp <- select(temp, date, deaths)
  state.deaths <- merge(state.deaths, temp, by = "date", all = T)
  names(state.deaths)[i+1] <- paste(as.character(States[i]))
}
state.deaths[is.na(state.deaths)] <- 0



test <- melt(state.deaths, id.vars = "date")
test$variable <- as.character(test$variable)
test <- test %>%
  filter(value > 0)
test$days <- 0

temp <- filter(test, variable == States[1])
temp$days <- c(0:(nrow(temp)-1))
test2 <- select(temp, days, variable, value)
for(i in c(2:length(States))){
  temp <- filter(test, variable == States[i])
  if(nrow(temp) > 0){
    temp$days <- c(0:(nrow(temp)-1))
    temp <- select(temp, days, variable, value)
    test2 <- rbind(test2, temp)
  }
}

test2 %>%
  ggplot( aes(x=days, y=value, group=variable, color=variable)) +
  geom_line()

