#'This will get the win amounts and compare them to each other on a bar plot in a
#'single column.
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
show_wins <- function(team,data){
  data %>% 
    filter(Team == team) %>% 
    pivot_longer(cols = c(Wins,Losses), names_to = "Outcome",values_to = "Count") %>% 
    ggplot(aes(x = Team, y = Count, fill = Outcome))+
    geom_bar(stat = "identity", position = "stack")
}