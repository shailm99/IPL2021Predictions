# Model 2: Simulating a game 


# Since we have delivery by delivery level data for every team we can take each team’s delivery distribution based upon historical data against an opposition
# Then simulate by randomly sampling from it then simulating the entire match and pick a winner from there (this might be hard and may take a lot of time to run but it could be interesting)


matches <- read.csv('IPL Matches 2008-2020.csv')
deliv <- read.csv("IPL Ball-by-Ball 2008-2020.csv")

library(lubridate)
library(tidyverse)

matcheid <- matches[year(matches$date) >= 2018,]$id

deliv18 <- deliv %>% tibble() %>% filter(id %in% matcheid) %>% arrange(id, over)

deliv18[deliv18$batting_team == "Delhi Daredevils",]$batting_team = "Delhi Capitals" 
deliv18[deliv18$bowling_team == "Delhi Daredevils",]$bowling_team = "Delhi Capitals" 

# break the game into 3 phases: powerplay, middle overs, death bowling 
unique(deliv18$batting_team)
cskMI <- deliv18[deliv18$batting_team == "Chennai Super Kings" & deliv18$bowling_team == "Mumbai Indians",]

cskMI
pp <- cskMI %>% filter(over <= 5)
mo <- cskMI %>% filter(over >= 6, over <= 14)
death <-cskMI %>% filter(over >= 15)


pp_index <- sample(seq_along(pp$total_runs), 6, replace = TRUE)
pp_sampled <- pp[pp_index,]
sum(pp_sampled$total_runs) 
sum(pp_sampled$is_wicket)

total_runs <- 0
total_wickets <- 0

simulatePhase <- function(df, balls, current_runs = 0, current_wickets = 0) {
  index <- sample(nrow(df), balls, replace = TRUE)
  sampled <- df[index,]
  total_runs <- current_runs
  total_wickets <- current_wickets
  
  if (total_wickets >= 10) {
    return(c(total_runs, total_wickets))
  }
  
  for (i in seq_len(nrow(sampled))) {
    ball <- sampled[i,]
    total_runs <- total_runs + ball$total_runs
    total_wickets <- total_wickets + ball$is_wicket
    extras <- !is.na(ball$extras_type)
    extra_type <- ball$extras_type
    if (total_wickets >= 10) {
      return(c(total_runs, total_wickets))
    } 
    while(extras & (extra_type %in% c("noballs", "wides"))){
      extra_sample <- pp[sample(seq_along(pp$total_runs), 1, replace = TRUE),]
      total_runs <- extra_sample$total_runs + total_runs
      
      if (extra_type == 'wides') {
        total_wickets <- total_wickets + extra_sample$is_wicket
        if (total_wickets >= 10) {
          return(c(total_runs, total_wickets))
        } 
      }
      extras <- !is.na(extra_sample$extras_type)
      extra_type <- extra_sample$extras_type
    } 
  }
  return(c(total_runs, total_wickets))
}

simInnings <- function(df) {
  pp <- df %>% filter(over <= 5)
  mo <- df %>% filter(over >= 6, over <= 14)
  death <-df %>% filter(over >= 15)
  
  total <- c(0, 0)
  
  total <- simulatePhase(pp, 36, current_runs = total[1], current_wickets = total[2])

  total <- simulatePhase(mo, 54, current_runs = total[1], current_wickets = total[2])

  total <- simulatePhase(death, 30, current_runs = total[1], current_wickets = total[2])

  total[1]
}

simSuperOver <- function(df){
  death <-df %>% filter(over >= 15)
  total <- simulatePhase(death, 6, 0, 0)
  total[1]
}
simMatch <- function(teamA, teamB) {
  batA <- deliv18[deliv18$batting_team == teamA & deliv18$bowling_team == teamB,]
  runsA <- simInnings(batA)
  
  batB <- deliv18[deliv18$batting_team == teamB & deliv18$bowling_team == teamA,]
  runsB <- simInnings(batB)
  
  if (runsA > runsB) {
    return(teamA)
  } else if (runsB > runsA) {
    return(teamB)
  } else {
    tie <- TRUE
    while(tie){
      runsA <- simSuperOver(batA)
      runsB <- simSuperOver(batB)
      if (runsA > runsB) {
        return(teamA)
      } else if (runsB > runsA) {
        return(teamB)
      }
    }
  }
}

teamNames <- unique(deliv18$batting_team)


fixtures <- cbind(combn(teamNames, 2), combn(teamNames, 2))
fixtures <- rbind(fixtures, NA, NA, NA)

set.seed(12345)
for (i in seq_len(ncol(fixtures))) {
  match <- fixtures[,i]
  team1 <- match[1]
  team2 <- match[2]
  result <- replicate(1000, simMatch(teamA = team1, teamB = team2))
  result <- table(result)
  fixtures[3,i] <- result[team1]
  fixtures[4,i] <- result[team2]
}

fixtures
rownames(fixtures) <- c("TeamA", "TeamB", "WinA", "WinB", "Winner")
fixtures <- as.data.frame(t(fixtures))
fixtures$Winner <- fixtures$WinA > fixtures$WinB
fixtures[fixtures$Winner == TRUE,]$Winner <- fixtures[fixtures$Winner == TRUE,]$TeamA
fixtures[fixtures$Winner == FALSE,]$Winner <- fixtures[fixtures$Winner == FALSE,]$TeamB

fixtures

points <- table(fixtures['Winner']) * 2
points
league_table <- sort(points, decreasing = TRUE)
write.csv(data.frame(team = names(league_table), point = league_table)[, c(1,3)], "model2_LT.csv")

write.csv(fixtures, "model2_FIX.csv")
