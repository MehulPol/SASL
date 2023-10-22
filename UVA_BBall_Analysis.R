
library("tidyverse")
library("useful")

## Obtain data from games ##

games = read.csv("/Users/mehulpol/SASL/games.csv")

UVA_roster = c("Kihei Clark", "Reece Beekman", "Armaan Franklin", "Jayden Gardner", 
               "Ben Vander Plas", "Kadin Shedrick", "Francisco Caffaro", "Ryan Dunn",
               "Isaac McKneely", "Taine Murray", "Chase Coleman", "Tristan How", "Cavaliers")
### Stats for each Lineup while they are on the court together

Lineup_stats = tibble(On_court_time = 0, Pts= 0, Pts_against = 0, Tnovers = 0, FG_made = 0, FG_att = 0, Three_made =0, Three_att = 0,Rebounds = 0,Defensive_plays = 0)
starting = lapply(strsplit("Reece Beekman,Kadin Shedrick,Kihei Clark,Armaan Franklin,Jayden Gardner", split = ","),sort)
Lineup_stats = add_column(Lineup_stats,Lineup = starting,.before = "On_court_time")
games = add_column(games, Lineup = "",.after = "line")

Lineup_time = 0
i_lineup = 1
for (i in 2:nrow(games)) {
  if (games$Time_in_sec[i-1]>=games$Time_in_sec[i]){
    Lineup_stats$On_court_time[i_lineup] = Lineup_stats$On_court_time[i_lineup] + (games$Time_in_sec[i-1] - games$Time_in_sec[i])
    Lineup_stats$Pts[i_lineup] = Lineup_stats$Pts[i_lineup] + (games$UVA_score[i] - games$UVA_score[i-1])
    Lineup_stats$Pts_against[i_lineup] = Lineup_stats$Pts_against[i_lineup] + (games$Opp_score[i] - games$Opp_score[i-1])
  } 
  if (games$Event[i] == "Cav Sub"){
    Players = lapply(strsplit(gsub("\\)","",gsub("Cavaliers lineup change \\(", "", games$Description[i])), split = ", "),sort)
    if (typeof(Players) != typeof(strsplit("This is a list", split = " "))){
      Players = lapply(strsplit(gsub("\\)","",gsub("Cavaliers lineup change \\(", "", games$Description[i])), split = ","),sort)
    }
    games$Lineup[i] = Players
    #if (-grepl("\\(",Players)){
    #  Players = strsplit(paste(Players, sep = " "), split = ",")
    #}
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
        add_row(Lineup = Players,On_court_time = 0,Pts= 0,Pts_against = 0,Tnovers = 0,FG_made = 0,FG_att = 0,Three_made =0,Three_att = 0,Rebounds = 0,Defensive_plays = 0)
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

Player_stats = tibble(Player = "", On_court_time = 0, Pts= 0, Pts_against = 0, Tnovers = 0, FG_made = 0, FG_att = 0, Three_made =0, Three_att = 0,Rebounds = 0,Defensive_plays = 0)
for (i in 1:length(UVA_roster)) {
  lineups_withp = grepl(UVA_roster[i],Lineup_stats$Lineup)
  Player_stats <- Player_stats %>%
    add_row(Player = UVA_roster[i],
            On_court_time = sum(Lineup_stats$On_court_time[lineups_withp]),
            Pts= sum(Lineup_stats$Pts[lineups_withp]),Pts_against = sum(Lineup_stats$Pts_against[lineups_withp]),
            Tnovers = sum(Lineup_stats$Tnovers[lineups_withp]),
            FG_made = sum(Lineup_stats$FG_made[lineups_withp]), FG_att = sum(Lineup_stats$FG_att[lineups_withp]),
            Three_made = sum(Lineup_stats$Three_made[lineups_withp]), Three_att = sum(Lineup_stats$Three_att[lineups_withp]),
            Rebounds = sum(Lineup_stats$Rebounds[lineups_withp]), Defensive_plays = sum(Lineup_stats$Defensive_plays[lineups_withp]))
  Player_stats <- Player_stats %>%
    add_row(Player = paste("Without",UVA_roster[i], sep = " "),
            On_court_time = sum(Lineup_stats$On_court_time[!lineups_withp]),
            Pts= sum(Lineup_stats$Pts[!lineups_withp]),Pts_against = sum(Lineup_stats$Pts_against[!lineups_withp]),
            Tnovers = sum(Lineup_stats$Tnovers[!lineups_withp]),
            FG_made = sum(Lineup_stats$FG_made[!lineups_withp]), FG_att = sum(Lineup_stats$FG_att[!lineups_withp]),
            Three_made = sum(Lineup_stats$Three_made[!lineups_withp]), Three_att = sum(Lineup_stats$Three_att[!lineups_withp]),
            Rebounds = sum(Lineup_stats$Rebounds[!lineups_withp]), Defensive_plays = sum(Lineup_stats$Defensive_plays[!lineups_withp]))
}
Player_stats = Player_stats[-1,]

Player_stats = add_column(Player_stats, Pt_diff_permin = (Player_stats$Pts-Player_stats$Pts_against)/(Player_stats$On_court_time/60), .before = "Pts")
Lineup_stats = add_column(Lineup_stats, Pt_diff_permin = (Lineup_stats$Pts-Lineup_stats$Pts_against)/(Lineup_stats$On_court_time/60), .before = "Pts")
Lineup_stats = Lineup_stats %>%
  mutate(efficiency = (Pts+ Rebounds + Defensive_plays - (FG_att - FG_made) - Tnovers) / (On_court_time/60), .before = "Pt_diff_permin")


# Subset for Games and Players for adjusted plus/minus

for (game in unique(games$opp)){
  
}

# Subset for longer time on court
x = Lineup_stats[which(Lineup_stats$On_court_time>600),]

Player_stats = Player_stats %>%
  mutate(efficiency = (Pts+ Rebounds + Defensive_plays - (FG_att - FG_made) - Tnovers) / (On_court_time/60), .before = "Pt_diff_permin")

y = Player_stats[which(Player_stats$On_court_time>600),]



