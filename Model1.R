# Model 1: Simple Model 

matches <- read.csv('IPL Matches 2008-2020.csv')
deliv <- read.csv("IPL Ball-by-Ball 2008-2020.csv")
# Assume the number of runs a team scores is normally distributed 
# with mean and standard deviation determined by their historical record vs. the opposition

# Randomly sample scores from both teams and determine the winner based upon who scores more 

# cleaning
library(lubridate)
library(tidyverse)
matches08 <- matches[year(matches$date) >= 2018,]

matchs20ids <- matches08[year(matches08$date) >= 2020,]$id 

innings_summary <- deliv %>% tibble() %>% group_by(id, inning, batting_team, bowling_team) %>% 
  summarise(runs_scored = sum(total_runs))

# normality assumption met for overall runs scored 
qqnorm(innings_summary$runs_scored, ylab = "Runs scored in an innings")
qqline(innings_summary$runs_scored)

# we want the 2020 innings to be weighted twice as much as the 2019 and 2018 ones since its the most recent
in2020 <- innings_summary[innings_summary$id %in% matchs20ids,]

# we only want matches played in 2018 and after
innings08 <- rbind(innings_summary[innings_summary$id %in% matches08$id,], in2020)

# need to change delhi daredevils to delhi capitals 
innings08[innings08$batting_team == "Delhi Daredevils",]$batting_team = "Delhi Capitals" 
innings08[innings08$bowling_team == "Delhi Daredevils",]$bowling_team = "Delhi Capitals" 


length(unique(innings08$batting_team)) # correct number of teams 

# generate all the 56 league fixtures for the simulation 
fixtures <- cbind(combn(unique(innings08$batting_team), 2), combn(unique(innings08$batting_team), 2))
fixtures <- rbind(fixtures, NA, NA, NA, NA)
simMatch <- function(teamA, teamB){
  historyA <- innings08 %>% filter(batting_team == teamA, bowling_team == teamB)
  historyA <- c(mean(historyA$runs_scored), sd(historyA$runs_scored))
  print(historyA)
  runsA <- round(rnorm(1, historyA[1], historyA[2]))
  
  historyB <- innings08 %>% filter(batting_team == teamB, bowling_team == teamA)
  historyB <- c(mean(historyB$runs_scored), sd(historyB$runs_scored))
  print(historyB)
  runsB <- round(rnorm(1, historyA[1], historyA[2]))
  
  if (runsA > runsB) {
    return(teamA)
  } else if (runsB > runsA) {
    return(teamB)
  } else {
    return("Tie")
  }
}

simMatch("Chennai Super Kings", "Mumbai Indians")
set.seed(240340)
for (i in seq_len(ncol(fixtures))) {
  match <- fixtures[,i]
  team1 <- match[1]
  team2 <- match[2]
  result <- table(replicate(1000, {
    simMatch(teamA = team1, teamB = team2)
  }))
  if (length(result) == 3){
    fixtures[3,i] <- result[team1]
    fixtures[4,i] <- result[team2]
    fixtures[5,i] <- result["Tie"]
  }
}

rownames(fixtures) <- c("TeamA", "TeamB", "WinA", "WinB", "Tie", "Winner")
fixtures <- as.data.frame(t(fixtures))
fixtures$WinA<- as.numeric(fixtures$WinA)
fixtures$WinB <- as.numeric(fixtures$WinB)
fixtures$Tie <- as.numeric(fixtures$Tie)



fixtures$Winner <- fixtures$WinA > fixtures$WinB
fixtures[fixtures$Winner == TRUE,]$Winner <- fixtures[fixtures$Winner == TRUE,]$TeamA
fixtures[fixtures$Winner == FALSE,]$Winner <- fixtures[fixtures$Winner == FALSE,]$TeamB



points <- table(fixtures['Winner']) * 2
points

league_table <- sort(points, decreasing = TRUE)
write.csv(data.frame(team = names(league_table), point = league_table)[, c(1,3)], "model1_LT.csv")

write.csv(t(fixtures), "model1_FIX.csv")

write.csv(t(fixtures), 'fixtures.csv')
