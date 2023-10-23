library("rvest")
library("ggplot2")
library("tidyverse")
library("useful")
library("stringr")


### Reading in Website

UVA_roster = c("Kihei Clark", "Reece Beekman", "Armaan Franklin", "Jayden Gardner", 
               "Ben Vander Plas", "Kadin Shedrick", "Francisco Caffaro", "Ryan Dunn",
               "Isaac McKneely", "Taine Murray", "Chase Coleman", "Tristan How", "Cavaliers")
website1 = "https://www.foxsports.com/college-basketball/north-carolina-central-eagles-vs-virginia-cavaliers-nov-07-2022-game-boxscore-239938?tab=playbyplay"
starting_lineup1 = "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner"
abbrev1 = "NCCU"
opp1 = "vsNCCU"
game = tibble(Time = "", Description = "", Event = "", Token = "", words = strsplit("place holder", split = ' '), Player = "", 
               UVA_score = 0, Opp_score = 0, Time_in_sec = 0, Opps = "")

game_pbp = function(website,starting_lineup,abbrev,opp){
  
  fox = read_html(website)
  first_half<-fox%>%
    html_nodes(".flex-col:nth-child(1)")%>%
    html_text()
  second_half<-fox%>%
    html_nodes(".final+ .final")%>%
    html_text()
  
  fh_array = unlist(strsplit(first_half,split = '\n'))
  sh_array = unlist(strsplit(second_half,split = '\n'))
  fh_array = sapply(fh_array,str_trim,side="both")
  sh_array = sapply(sh_array,str_trim,side="both")
  fh_array = fh_array[fh_array != ""]
  sh_array = sh_array[sh_array != ""]
  fh_array = fh_array[-grep("UVA",fh_array)]
  sh_array = sh_array[-grep("UVA",sh_array)]
  
  fh_table = as.data.frame(matrix(c(fh_array,""),ncol=2,byrow=T))
  sh_table = as.data.frame(matrix(c(sh_array,""),ncol=2,byrow=T))

  game = rbind(fh_table,sh_table)
  game = na.omit(game)
  
  ### Cleaning Dataset
  colnames(game) = c("Description","Time")
  
  starters = paste("Cavaliers lineup change (", starting_lineup, sep = '')
  game = add_row(game, Time = "20:00", Description = paste(starters, ")", sep = ''),.before = 1)
  game = game[,c(2,1)]
  
  ## Creating an Event Column
  game$Event = ""
  
  game[grepl("makes three", game$Description, fixed = TRUE),]$Event = "Made Three"
  game[grepl("makes two", game$Description, fixed = TRUE),]$Event = "Made Two"
  game[grepl("makes", game$Description, fixed = TRUE) & grepl("free throw", 
                                                              game$Description, fixed = TRUE),]$Event = "Made FT"
  
  game[grepl("misses three", game$Description, fixed = TRUE),]$Event = "Missed Three"
  game[grepl("misses two", game$Description, fixed = TRUE),]$Event = "Missed Two"
  game[grepl("misses", game$Description, fixed = TRUE) &
         grepl("free throw", game$Description, fixed = TRUE),]$Event = "Missed FT"
  game[grepl("turnover", game$Description, fixed = TRUE),]$Event = "Turnover"
  game[grepl("block", game$Description, fixed = TRUE),]$Event = "Block"
  game[grepl("offensive rebound", game$Description, fixed = TRUE),]$Event = "Off Rebound"
  game[grepl("defensive rebound", game$Description, fixed = TRUE),]$Event = "Def Rebound"
  if (length(game$Description[grepl("second timeout", game$Description, fixed = TRUE)]) >0){
    game[grepl("second timeout", game$Description, fixed = TRUE),]$Event = "Timeout"
  }
  if (length(game$Description[grepl("Jump ball", game$Description, fixed = TRUE)]) >0){
    game[grepl("Jump ball", game$Description, fixed = TRUE),]$Event = "Jump Ball" 
  }
  
  
  game[grepl("lineup", game$Description, fixed = TRUE),]$Event = "Opp Sub"
  game[grepl("Cavaliers lineup", game$Description, fixed = TRUE),]$Event = "Cav Sub"
  
  game[grepl("shooting foul", game$Description, fixed = TRUE) | 
         grepl("personal foul", game$Description, fixed = TRUE),]$Event = "Foul"
  
  
  #game[grepl("flagrant", game$Description, fixed = TRUE),]$Event = "Flagrant"
  
  ## Creating a Player Column
  
  # Create a token to split the strings on
  game$Token = " "
  
  game[grepl("makes", game$Description, fixed = TRUE),]$Token = "makes"
  game[grepl("misses", game$Description, fixed = TRUE),]$Token = "misses"
  game[grepl("turnover", game$Description, fixed = TRUE),]$Token = "turnover"
  game[grepl("block", game$Description, fixed = TRUE),]$Token = "block"
  
  game[grepl("offensive rebound", game$Description, fixed = TRUE),]$Token = "offensive rebound"
  game[grepl("defensive rebound", game$Description, fixed = TRUE),]$Token = "defensive rebound"
  if (length(game$Description[grepl("second timeout", game$Description, fixed = TRUE)]) >0){
    game[grepl("second timeout", game$Description, fixed = TRUE),]$Token = "second timeout"
  }
  
  
  game[grepl("shooting foul", game$Description, fixed = TRUE),]$Token = "shooting foul"
  game[grepl("personal foul", game$Description, fixed = TRUE),]$Token = "personal foul"
  
  # split on the token, take the first element (player doing action)
  game$words = strsplit(game$Description, game$Token)
  game$Player = lapply(game$words,"[[",1) 
  game$Player = str_trim(game$Player, "both")
  
  game$UVA_score = 0
  game$Opp_score = 0
  
  for( i in 2:nrow(game)) {
    
    scoreDif = 0
    oppScoreDif = 0
    
    if (game$Token[i] == "makes") {
      
      if(game$Player[i] %in% UVA_roster) {
        
        if(game$Event[i] == "Made FT") {
          scoreDif = 1
        } else if (game$Event[i] == "Made Two") {
          scoreDif = 2
        } else if (game$Event[i] == "Made Three") {
          scoreDif = 3
        }
        
        
      } else {
        
        if(game$Event[i] == "Made FT") {
          oppScoreDif = 1
        } else if (game$Event[i] == "Made Two") {
          oppScoreDif = 2
        } else if (game$Event[i] == "Made Three") {
          oppScoreDif = 3
        }
        
      }
      
    }
    game$UVA_score[i] = game$UVA_score[i-1] + scoreDif
    game$Opp_score[i] = game$Opp_score[i-1] + oppScoreDif
  }
  
  ### Cleaning for Time and usable Time in seconds
  
  game$Time_in_sec = 0 
  
  for( i in 1:nrow(game)) {
    extra = c('1STHALF','2NDHALF',paste(abbrev,as.character(game$Opp_score[i-1]), sep = ''), paste('UVA',as.character(game$UVA_score[i-1]), sep = ''))
    for (e in extra) {
      game$Time[i] = gsub(e,'',game$Time[i])
    }
    min_sec = as.character(game$Time[i])
    if (str_length(min_sec) == 4) {
      min_sec = paste('0',min_sec, sep = '')
    }
    game$Time_in_sec[i] = as.numeric(substr(min_sec, 1, 2))*60 + as.numeric(substr(min_sec, 4, 5))
  }
  game = na.omit(game)
  game$Opps = opp
  
  game
}

game1 = game_pbp(website1,starting_lineup1,abbrev1,opp1)

### end of playbyplay data for all games







### Stats for each Lineup while they are on the court together

Lineup_stats = tibble(On_court_time = 0,Possessions = 0, Pts= 0, Pts_against = 0, Tnovers = 0, FG_made = 0, FG_att = 0, Three_made =0, Three_att = 0,Rebounds = 0,Defensive_plays = 0)
starting = lapply(strsplit("Reece Beekman,Kadin Shedrick,Kihei Clark,Armaan Franklin,Jayden Gardner", split = ","),sort)
Lineup_stats = add_column(Lineup_stats,Lineup = starting,.before = "On_court_time")

Lineup_time = 0
i_lineup = 1
for (i in 2:nrow(games)) {
  if (games$Time_in_sec[i-1]>=games$Time_in_sec[i]){
    Lineup_stats$On_court_time[i_lineup] = Lineup_stats$On_court_time[i_lineup] + (games$Time_in_sec[i-1] - games$Time_in_sec[i])
    Lineup_stats$Pts[i_lineup] = Lineup_stats$Pts[i_lineup] + (games$UVA_score[i] - games$UVA_score[i-1])
    Lineup_stats$Pts_against[i_lineup] = Lineup_stats$Pts_against[i_lineup] + (games$Opp_score[i] - games$Opp_score[i-1])
    Lineup_stats$Possessions[i_lineup] = Lineup_stats$Possessions[i_lineup] + (games$`Possession Number`[i] - games$`Possession Number`[i-1])
  } 
  if (games$Event[i] == "Cav Sub"){
    Players = lapply(strsplit(gsub("\\)","",gsub("Cavaliers lineup change \\(", "", games$Description[i])), split = ", "),sort)
    i_lineup = 0
    for (s in 1:nrow(Lineup_stats)){
      if(length(Lineup_stats$Lineup[s]) == length(Players)){
        if (compare.list(Lineup_stats$Lineup[s],Players)){
          i_lineup = s
        }
      } 
    }
    if (i_lineup == 0){
      Lineup_stats <- Lineup_stats %>% 
        add_row(Lineup = Players,On_court_time = 0,Possessions = 0,Pts= 0,Pts_against = 0,Tnovers = 0,FG_made = 0,FG_att = 0,Three_made =0,Three_att = 0,Rebounds = 0,Defensive_plays = 0)
      i_lineup = nrow(Lineup_stats)
    }
  } else if (games$Player[i] %in% UVA_roster) {
    if (games$Event[i] == "Block"){
      Lineup_stats$Defensive_plays[i_lineup] = Lineup_stats$Defensive_plays[i_lineup] + 1
    } else if(games$Event[i] == "Off Rebound" || games$Event[i] == "Def Rebound"){
      Lineup_stats$Rebounds[i_lineup] = Lineup_stats$Rebounds[i_lineup] + 1
    } else if(games$Event[i] == "Made Two"){
      Lineup_stats$FG_made[i_lineup] = Lineup_stats$FG_made[i_lineup] + 1
      Lineup_stats$FG_att[i_lineup] = Lineup_stats$FG_att[i_lineup] + 1
    } else if(games$Event[i] == "Made Three"){
      Lineup_stats$FG_made[i_lineup] = Lineup_stats$FG_made[i_lineup] + 1
      Lineup_stats$FG_att[i_lineup] = Lineup_stats$FG_att[i_lineup] + 1
      Lineup_stats$Three_made[i_lineup] = Lineup_stats$Three_made[i_lineup] + 1
      Lineup_stats$Three_att[i_lineup] = Lineup_stats$Three_att[i_lineup] + 1
    } else if(games$Event[i] == "Missed Two"){
      Lineup_stats$FG_att[i_lineup] = Lineup_stats$FG_att[i_lineup] + 1
    } else if(games$Event[i] == "Missed Three"){
      Lineup_stats$FG_att[i_lineup] = Lineup_stats$FG_att[i_lineup] + 1
      Lineup_stats$Three_att[i_lineup] = Lineup_stats$Three_att[i_lineup] + 1
    } else if(games$Event[i] == "Turnover"){
      Lineup_stats$Tnovers[i_lineup] = Lineup_stats$Tnovers[i_lineup] + 1
    } 
  } else if (games$Event[i] == "Turnover") {
    Lineup_stats$Defensive_plays[i_lineup] = Lineup_stats$Defensive_plays[i_lineup] + 1
  }
}

# Next, Create Table for individual players using the Lineup_stats table

Player_stats = tibble(Player = "", On_court_time = 0, Possessions = 0, Pts= 0, Pts_against = 0, Tnovers = 0, FG_made = 0, FG_att = 0, Three_made =0, Three_att = 0,Rebounds = 0,Defensive_plays = 0)
for (i in 1:length(UVA_roster)) {
  lineups_withp = grepl(UVA_roster[i],Lineup_stats$Lineup)
  Player_stats <- Player_stats %>%
    add_row(Player = UVA_roster[i],
            On_court_time = sum(Lineup_stats$On_court_time[lineups_withp]),Possessions = sum(Lineup_stats$Possessions[lineups_withp]),
            Pts= sum(Lineup_stats$Pts[lineups_withp]),Pts_against = sum(Lineup_stats$Pts_against[lineups_withp]),
            Tnovers = sum(Lineup_stats$Tnovers[lineups_withp]),
            FG_made = sum(Lineup_stats$FG_made[lineups_withp]), FG_att = sum(Lineup_stats$FG_att[lineups_withp]),
            Three_made = sum(Lineup_stats$Three_made[lineups_withp]), Three_att = sum(Lineup_stats$Three_att[lineups_withp]),
            Rebounds = sum(Lineup_stats$Rebounds[lineups_withp]), Defensive_plays = sum(Lineup_stats$Defensive_plays[lineups_withp]))
  Player_stats <- Player_stats %>%
    add_row(Player = paste("Without",UVA_roster[i], sep = " "),
            On_court_time = sum(Lineup_stats$On_court_time[!lineups_withp]),Possessions = sum(Lineup_stats$Possessions[!lineups_withp]),
            Pts= sum(Lineup_stats$Pts[!lineups_withp]),Pts_against = sum(Lineup_stats$Pts_against[!lineups_withp]),
            Tnovers = sum(Lineup_stats$Tnovers[!lineups_withp]),
            FG_made = sum(Lineup_stats$FG_made[!lineups_withp]), FG_att = sum(Lineup_stats$FG_att[!lineups_withp]),
            Three_made = sum(Lineup_stats$Three_made[!lineups_withp]), Three_att = sum(Lineup_stats$Three_att[!lineups_withp]),
            Rebounds = sum(Lineup_stats$Rebounds[!lineups_withp]), Defensive_plays = sum(Lineup_stats$Defensive_plays[!lineups_withp]))
}
Player_stats = Player_stats[-1,]

Player_stats = add_column(Player_stats, Pt_diff_permin = (Player_stats$Pts-Player_stats$Pts_against)/(Player_stats$On_court_time/60), .before = "Pts")
Lineup_stats = add_column(Lineup_stats, Pt_diff_permin = (Lineup_stats$Pts-Lineup_stats$Pts_against)/(Lineup_stats$On_court_time/60), .before = "Pts")
