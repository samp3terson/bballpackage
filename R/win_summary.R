#'This function will summarize all the data gained from the website and get it ready
#'for any team that you select from the website.
#'
#'@param team This is the team that you want to get information about
#'@param data This is where you would draw information from
#'
#'@return This returns the data you will use to make the other graphs
#'
win_summary <- function(team, data) {
  sumstats <- data %>% 
    filter(`Home Team` == team | `Away Team` == team) %>% 
    mutate(
      result = case_when(
        `Home Team` == team & `Home Score` > `Away Score` ~ "Win",
        `Away Team` == team & `Away Score` > `Home Score` ~ "Win",
        TRUE ~ "Loss"
      )) %>% 
    summarize(
      Team = team,
      Total_Games = n(),
      Wins = sum(result == "Win"),
      Losses = sum(result == "Loss"),
      Win_Percentage = mean(result == "Win", na.rm = TRUE),
      Date = first(Date),
      Score_Diff = first(`Score Difference`)
    )
  return(sumstats)
}