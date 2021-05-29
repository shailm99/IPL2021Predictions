# Latest IPL results
library(tidyverse)
actual <- read.table('actual.txt', sep = ',', header = TRUE)
actual[1,2]

teams <- character(8)
names(teams) <- sort(unique(actual$Winner))
teams


model1 <- as.data.frame(t(read.csv('model1_FIX.csv', row.names = 'X')))
model2 <- read.csv('model2_FIX.csv',row.names = 'X')
model3 <- read.csv('Model3FIX.csv', row.names = 'X')



which(abs(as.numeric(model1$WinA) - as.numeric(model1$WinB)) == min(abs(as.numeric(model1$WinA) - as.numeric(model1$WinB))))

which(abs(as.numeric(model2$WinA) - as.numeric(model2$WinB)) == max(abs(as.numeric(model2$WinA) - as.numeric(model2$WinB))))
model2$diff <- abs(as.numeric(model2$WinA) - as.numeric(model2$WinB))
tibble(model2) %>% arrange(desc(diff))
# RCB KKR
# CSK KXIP
# KXIP SRH
# MI CSK

tibble(model1)
tibble(model2)


colnames(model3) <- c("TeamA", "TeamB", "Winner", "WinA", "WinB")
sd(table(model2$Winner) * 2)
sd(table(model3$Winner) * 2)
sd(table(model1$Winner) * 2)


noquote(sort(unique(model1$Winner)))
paste(sort(unique(model1$Winner)), collapse = ',')

teams[] <- c("Chennai Super Kings","Delhi Capitals","Kolkata Knight Riders", "Kings XI Punjab"
, "Mumbai Indians", "Royal Challengers Bangalore", "Rajasthan Royals", "Sunrisers Hyderabad")

actual$TeamA <- vapply(actual$TeamA, function(x) {teams[str_trim(x)]}, FUN.VALUE = character(1))
actual$TeamB <- vapply(actual$TeamB, function(x) {teams[str_trim(x)]}, FUN.VALUE = character(1))
actual$Winner <- vapply(actual$Winner, function(x) {teams[str_trim(x)]}, FUN.VALUE = character(1))

actual

make_predictions <- function(actual, model){
  predictions <- character(length = length(nrow(actual)))
  
  for (i in seq_len(nrow(actual))) {
    d <- actual[i,] 
    predicted <- model[((model$TeamA == d$TeamA) | (model$TeamB == d$TeamA)) & ((model$TeamA == d$TeamB) | (model$TeamB == d$TeamB)),][1,]
    index_to_drop <- which(rownames(model) == rownames(predicted))
    predictions[i] <- predicted$Winner
    model <- model[-index_to_drop,]
  }
  predictions
}

actual$WinnerModel1 <- make_predictions(actual, model1)
actual$WinnerModel2 <- make_predictions(actual, model2)
actual$WinnerModel3 <- make_predictions(actual, model3)

table(make_predictions(actual, model3)$Winner) * 2 # predicting the remainder of the season



# weighted success
model1$WinA <- as.numeric(model1$WinA)  / 1000
model1$WinB <- as.numeric(model1$WinB)  / 1000
model2$WinA <- as.numeric(model2$WinA)  / 1000
model2$WinB <- as.numeric(model2$WinB)  / 1000

model2


weighted_success <- function(actual, model){
  predictions <- numeric(length = length(nrow(actual)))
  
  for (i in seq_len(nrow(actual))) {
    d <- actual[i,] 
    predicted <- model[((model$TeamA == d$TeamA) | (model$TeamB == d$TeamA)) & ((model$TeamA == d$TeamB) | (model$TeamB == d$TeamB)),][1,]
    index_to_drop <- which(rownames(model) == rownames(predicted))
    if (d$Winner == predicted$TeamA){
      predictions[i] <- predicted$WinA
    } else if (d$Winner == predicted$TeamB){
      predictions[i] <- predicted$WinB
    } else {
      print(d$Winner)
      print(predicted$TeamB)
      print(predicted$TeamA)
    }    
    model <- model[-index_to_drop,]
  }
  predictions
}

model1_weights <- weighted_success(actual, model1)
model2_weights <- weighted_success(actual, model2)
model3_weights <- weighted_success(actual, model3)

sum(model1_weights)
sum(model2_weights)
sum(model3_weights)




library(tidyverse)
predictions  <- actual %>% tibble() %>% mutate(model1 = Winner == WinnerModel1, model2 = Winner == WinnerModel2,
                                               model3 = Winner == WinnerModel3)

predictions %>% summarise(acc_model1 = mean(model1), acc_model2 = mean(model2), acc_model3 = mean(model3))







table(predictions$Winner) * 2
sort(table(predictions$WinnerModel1) * 2, decreasing = TRUE)
sort(table(predictions$WinnerModel2) * 2, decreasing = TRUE)
sort(table(predictions$WinnerModel3) * 2, decreasing = TRUE)
league_table <- data.frame(team = teams, Actual = as.numeric(table(predictions$Winner) * 2),
           Model1 = as.numeric(table(predictions$WinnerModel1) * 2), 
           Model2 = as.numeric(c(table(predictions$WinnerModel2) * 2, 0)),
                               Model3 = as.numeric(c(table(predictions$WinnerModel3) * 2,)))
league_table %>% tibble() %>% mutate(devmodel1 = abs(Actual - Model1),
                                     devmodel2 = abs(Actual - Model2),
                                     devmodel3 = abs(Actual - Model3),
                                     actual_rank = rank(Actual), 
                                     model1_rank = rank(Model1), 
                                     model2_rank = rank(Model2),  model3_rank = rank(Model3)) %>% mutate(
                                       devmodel1_rank = abs(actual_rank - model1_rank), 
                                       devmodel2_rank = abs(actual_rank - model2_rank), 
                                       devmodel3_rank = abs(actual_rank - model3_rank)
                                     ) %>% summarise(avgdev1 = mean(devmodel1),
                                                     avgdev2 = mean(devmodel2), avgdev3 = mean(devmodel3)
                                                    , avgdev1_rank = mean(devmodel1_rank),
                                                    avgdev2_rank = mean(devmodel2_rank),
                                                    avgdev3_rank = mean(devmodel3_rank)
                                                    )
league_table$team <- rownames(league_table)
pivot_wider(league_table)
pivot_longer(league_table, cols = "Actual" : "Model3", names_to = "Type") %>% 
  ggplot(aes(x = team, y = value, fill = Type)) + geom_bar(position = 'dodge', stat = 'identity') + labs(
    y = "Predicted Points", title = "Comparing our models' league table predictions with reality (as of 04/25/2021)"
  )




library(readr)
library(rvest)

team <- read_html('https://www.firstpost.com/firstcricket/teams/chennai-super-kings-team-players/series/ipl-2021.html')
team %>% html_nodes('.team-player-name') %>% html_text(trim = TRUE)

sess <- html_session('https://www.firstpost.com/firstcricket/teams/chennai-super-kings-team-players/series/ipl-2021.html')

sess %>% read_html() %>% html_nodes('.team-player-name') %>% html_text(trim = TRUE)


teams <- c("Chennai Super Kings","Delhi Capitals","Kolkata Knight Riders", "Punjab Kings"
             , "Mumbai Indians", "Royal Challengers Bangalore", "Rajasthan Royals", "Sunrisers Hyderabad")

teams <- tolower(teams)
teams <- gsub(' ', '-', teams)

urls <- paste('https://www.firstpost.com/firstcricket/teams/', teams,'-team-players/series/ipl-2021.html', sep = '')

teams <- c("Chennai Super Kings","Delhi Capitals","Kolkata Knight Riders", "Punjab Kings"
           , "Mumbai Indians", "Royal Challengers Bangalore", "Rajasthan Royals", "Sunrisers Hyderabad")

squads <- data.frame(team = c(), player = c())
for (i in seq_along(urls)){
  players <- read_html(urls[i]) %>% html_nodes('.team-player-name') %>% html_text(trim = TRUE)
  team <- rep(teams[i], length(players))
  squads <- rbind(squads, data.frame(team = team, player = players))
}  

write_csv(squads, 'squad.csv')


  