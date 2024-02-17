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
    filter(Team == team) %>%
    summarize(
      Team = team,
      Total_Games = first(Total_Games),
      Wins = first(Wins),
      Losses = first(Losses),
      Win_Percentage = first(Win_Percentage),
      Date = first(Date),
      Score_Diff = first(Score_Diff)
    )
  return(sumstats)
}
}
