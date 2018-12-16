add_draftkings_fantasy_points <- function(player_box){
  player_box %>% 
    mutate(pass_pts = 
             4*pass_td + 
             0.04*pass_yds + 
             -1*int +
             3*as.numeric(pass_yds > 300),
           rush_pts =  0.1*rush_yds + 
             6*rush_td + 
             3*as.numeric(rush_yds > 100),
           rec_pts =  0.1*rec_yds +
             receptions +
             6*rec_td +
             3*as.numeric(rec_yds > 100)) %>% 
    mutate(fantasy_points = rec_pts + rush_pts + pass_pts) %>% 
    return()
}

add_game_week_id <- function(player_box){
  player_box %>% 
     mutate(game_week_id = paste(season, "week", week, sep = "_")) %>% 
    return()
}
