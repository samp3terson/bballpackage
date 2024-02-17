#'This will get the win amounts and compare them to each other on a bar plot.
#'
#'@param team This is the team you want information about and needs to be 'team'
#'@param data This is where you're pulling your info from (summary_tibble.csv)
#'
#'@return This returns a bar plot
#'
#'@import
#' ggplot2
#'
#'@export
show_percentage_wins <- function(team,data){
  team_data <- data %>%
    filter(Team == team) %>%
    pivot_longer(cols = c(Wins, Losses), names_to = "Outcome", values_to = "Count")
  win_percentage <- data$Win_Percentage[which(data$Team == team)]
  ggplot(team_data, aes(x = Team, y = Count, fill = Outcome)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste("Wins and Losses for", team),
         x = "Team",
         y = "Number of Games") +
    scale_fill_manual(name = "Outcome", values = c("Wins" = "blue", "Losses" = "red")) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}