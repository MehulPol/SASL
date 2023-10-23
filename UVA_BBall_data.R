
library("rvest")
library("ggplot2")
library("tidyverse")
library("useful")
library("stringr")

UVA_roster = c("Kihei Clark", "Reece Beekman", "Armaan Franklin", "Jayden Gardner", 
               "Ben Vander Plas", "Kadin Shedrick", "Francisco Caffaro", "Ryan Dunn",
               "Isaac McKneely", "Taine Murray", "Chase Coleman", "Tristan How", "Cavaliers")

### Reading in Website

websites = c("https://www.foxsports.com/college-basketball/north-carolina-central-eagles-vs-virginia-cavaliers-nov-07-2022-game-boxscore-239938?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/monmouth-hawks-vs-virginia-cavaliers-nov-11-2022-game-boxscore-239939?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/continental-tire-main-event-baylor-bears-vs-virginia-cavaliers-nov-18-2022-game-boxscore-239941?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/continental-tire-main-event-virginia-cavaliers-vs-illinois-fighting-illini-nov-20-2022-game-boxscore-244972?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/maryland-eastern-shore-hawks-vs-virginia-cavaliers-nov-25-2022-game-boxscore-239942?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-michigan-wolverines-nov-29-2022-game-boxscore-240892?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/florida-state-seminoles-vs-virginia-cavaliers-dec-03-2022-game-boxscore-239943?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/james-madison-dukes-vs-virginia-cavaliers-dec-06-2022-game-boxscore-239944?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/houston-cougars-vs-virginia-cavaliers-dec-17-2022-game-boxscore-239945?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-miami-fl-hurricanes-dec-20-2022-game-boxscore-242282?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/albany-great-danes-vs-virginia-cavaliers-dec-28-2022-game-boxscore-239946?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-georgia-tech-yellow-jackets-dec-31-2022-game-boxscore-242427?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-pittsburgh-panthers-jan-03-2023-game-boxscore-240102?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/syracuse-orange-vs-virginia-cavaliers-jan-07-2023-game-boxscore-239947?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/north-carolina-tar-heels-vs-virginia-cavaliers-jan-10-2023-game-boxscore-239948?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-florida-state-seminoles-jan-14-2023-game-boxscore-241060?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-tech-hokies-vs-virginia-cavaliers-jan-18-2023-game-boxscore-239949?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-wake-forest-demon-deacons-jan-21-2023-game-boxscore-242875?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/boston-college-eagles-vs-virginia-cavaliers-jan-28-2023-game-boxscore-239950?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-syracuse-orange-jan-30-2023-game-boxscore-239641?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-virginia-tech-hokies-feb-04-2023-game-boxscore-242255?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/north-carolina-state-wolfpack-vs-virginia-cavaliers-feb-07-2023-game-boxscore-239951?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/duke-blue-devils-vs-virginia-cavaliers-feb-11-2023-game-boxscore-239952?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-louisville-cardinals-feb-15-2023-game-boxscore-241968?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/notre-dame-fighting-irish-vs-virginia-cavaliers-feb-18-2023-game-boxscore-239953?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-boston-college-eagles-feb-22-2023-game-boxscore-239995?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/virginia-cavaliers-vs-north-carolina-tar-heels-feb-25-2023-game-boxscore-243540?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/clemson-tigers-vs-virginia-cavaliers-feb-28-2023-game-boxscore-239954?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/louisville-cardinals-vs-virginia-cavaliers-mar-04-2023-game-boxscore-239955?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/conference-tournament-north-carolina-tar-heels-vs-virginia-cavaliers-mar-09-2023-game-boxscore-245316?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/conference-tournament-clemson-tigers-vs-virginia-cavaliers-mar-10-2023-game-boxscore-245319?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/conference-tournament-duke-blue-devils-vs-virginia-cavaliers-mar-11-2023-game-boxscore-245320?tab=playbyplay",
             "https://www.foxsports.com/college-basketball/ncaa-tournament-first-round-furman-paladins-vs-virginia-cavaliers-mar-16-2023-game-boxscore-245433?tab=playbyplay")
starting_lineups = c("Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Jayden Gardner,Kihei Clark,Isaac McKneely,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Kadin Shedrick,Reece Beekman,Jayden Gardner",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Ben Vander Plas",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Francisco Caffaro",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Francisco Caffaro",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Gardner,Francisco Caffaro",
                     "Armaan Franklin,Kihei Clark,Reece Beekman,Jayden Garner,Kadin Shedrick")
abbrevs = c("NCCU",
            "MONM",
            "BAY",
            "ILL",
            "UMES",
            "MICH",
            "FSU",
            "JMU",
            "HOU",
            "MIA",
            "ALBY",
            "GT",
            "PITT",
            "SYR",
            "UNC",
            "FSU",
            "VT",
            "WAKE",
            "BC",
            "SYR",
            "VT",
            "NCST",
            "DUKE",
            "LOU",
            "ND",
            "BC",
            "UNC",
            "CLEM",
            "LOU",
            "UNC",
            "CLEM",
            "DUKE",
            "FUR")
opps = c("vsNCCU","vsMONM","lvBAY","lvILL","vsUMES","atMICH","vsFSU","vsJMU","vsHOU","atMIA","vsALBY","atGT","
         atPITT","vsSYR","vsUNC","atFSU","vsVT","atWAKE","vsBC","atSYR","atVT","vsNCST","vsDUKE","atLOU","vsND","
         atBC","atUNC","vsCLEM","vsLOU","accUNC","accCLEM","accDUKE","mmFUR")
games = tibble(Time = "", Description = "", Event = "", Token = "", words = strsplit("place holder", split = ' '), Player = "", 
               UVA_score = 0, Opp_score = 0, Time_in_sec = 0, Opps = "")

for (g in 1:length(websites)){

fox = read_html(websites[g])
Sys.sleep(30)
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
if (length(grep("OVERTIME",sh_array))!=0){
  sh_array = sh_array[-grep("OVERTIME",sh_array)]
}


fh_table = as.data.frame(matrix(c(fh_array,""),ncol=2,byrow=T))
sh_table = as.data.frame(matrix(c(sh_array,""),ncol=2,byrow=T))

game = rbind(fh_table,sh_table)
game = na.omit(game)

### Cleaning Dataset
colnames(game) = c("Description","Time")

starters = paste("Cavaliers lineup change (", starting_lineups[g], sep = '')
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
game$words = c(strsplit(game$Description, game$Token))
game$Player = lapply(game$words,function(x){ifelse(length(x)>1,x[[1]],x)})
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
  min_sec = as.character(game$Time[i])
  if (str_length(min_sec) == 4) {
    min_sec = paste('0',min_sec, sep = '')
  }
  game$Time_in_sec[i] = as.numeric(substr(min_sec, 1, 2))*60 + as.numeric(substr(min_sec, 4, 5))
}
game = na.omit(game)

game$Opps = opps[g]

games = rbind(games,game)
}

games = games[-1,]

### end of playbyplay data for all games


#Possession Tracking
change_poss = c('Turnover','Def Rebound')
made_shot = c('Made Two','Made Three')
poss_vec = c()
poss = ""
for (i in 1:nrow(games)){
  if (grepl("vs.",games$Description[i])){
    if (unlist(strsplit(unlist(strsplit(games$Description[i],"[(]"))[2]," gains poss"))[1] %in% UVA_roster){
      poss = "Cavs"
    }
    else{poss = "Opp"}
  }
  else if(games$Event[i] %in% change_poss){
    if(poss == "Cavs"){poss = "Opp"}
    else{poss = "Cavs"}
  }
  else if(games$Event[i] %in% made_shot){
    if(i<nrow(games)){
      if(games$Token[i+1] == "shooting foul"){
        if((games$Player[i] %in% UVA_roster) == (games$Player[i+1] %in% UVA_roster)){
          if(poss == "Cavs"){poss = "Opp"}
          else{poss = "Cavs"}
        }
      }
      else{
        if(poss == "Cavs"){poss = "Opp"}
        else{poss = "Cavs"}
      }
    }
  }
  else if(grepl("inbound",games$Description[i])){
    if (unlist(strsplit(games$Description[i],"inbound "))[2] == "Cavaliers"){poss = "Cavs"}
    else{poss = "Opp"}
  }
  else if(games$Event[i] == "Made FT"){
    fts = as.integer(unlist(strsplit(unlist(strsplit(games$Description[i],"free throw "))[2]," of ")))
    if(fts[1] == fts[2]){
      if(poss == "Cavs"){poss = "Opp"}
      else{poss = "Cavs"}
    }
  }
  poss_vec = c(poss_vec,poss)
} 
games[,"Possession"] = poss_vec
games[1,"Possession"] = "Cavs"

num_poss = c()
game_poss = 1
for (i in 1:(nrow(games)-1)){
  num_poss =c(num_poss,game_poss)
  if (games$Opps[i]==games$Opps[i+1]){
    if (games$Possession[i]!=games$Possession[i+1]){game_poss = game_poss + 1}
  }
  else{
    game_poss = 0
  }
}
num_poss =c(num_poss,game_poss)
games[,"Possession Number"] = num_poss

games = games%>%select(-words)
#write.csv(games, "games.csv", row.names=FALSE)



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

