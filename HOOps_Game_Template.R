library("plyr")
library("rvest")
library("ggplot2")
library("tidyverse")
library("useful")
library("stringr")


### Reading in Website

UVA_roster = c("Blake Buchanan","Dante Harris","Reece Beekman","Andrew Rohde",
                "Desmond Roberts","Taine Murray","Isaac McKneely","Elijah Gertrude",
                "Ryan Dunn","Anthony Robinson","Jordan Minor","Tristan How", "Bryce Walker",
                "Christian Bliss","Jacob Groves","Leon Bond III","Cavaliers")


#UVA_roster = c("Kihei Clark", "Reece Beekman", "Armaan Franklin", "Jayden Gardner", 
#               "Ben Vander Plas", "Kadin Shedrick", "Francisco Caffaro", "Ryan Dunn",
#               "Isaac McKneely", "Taine Murray", "Chase Coleman", "Tristan How", "Cavaliers")

# ACC playbyplay for their lineup/substitution data
# acc_web = "https://theacc.com/boxscore.aspx?id=JcLFfL9RUu0H5ystoOMbjQ7A58QMysWHEEqi7hufVoQXO5nVxbctjHh%2F6ifARsoKUNz2Cn1GpJx0eQgL%2FI8uNugW6%2F65ooJ9xE6%2BliOEFRnJFwDEdzr8vBabydal0a6c%2FfJQROX%2BPC8WpklHbwVMoM%2FD9S6dIYosu6nSxEimSD6X%2BRBu%2FokVAmbam%2BXCIyUz&path=mbball#play-by-play"
# acc = read_html(acc_web)
# # all_plays<-acc%>%
# #   html_nodes(".text-right , th , .text-bold.hide-on-medium-down~ .hide-on-medium-down+ .hide-on-medium-down")%>%
# #   html_text()
# # 
# # all_plays = all_plays[-c(1:92)]
# # all_plays = all_plays[all_plays != ""]
# # all_plays = all_plays[-c(860:901)]
# # all_plays = all_plays[-c(405:413)]
# # 
# # all_plays = as.data.frame(matrix(all_plays,ncol=2,byrow=T))
# 
# 
# acc_subs <-acc%>%
#   html_nodes("th , .text-bold.hide-on-medium-down~ .hide-on-medium-down+ .hide-on-medium-down")%>%
#   html_text()
# acc_subs = acc_subs[-c(1:84)]
# acc_subs = acc_subs[-c(860:901,405:413)]
# acc_subs = as.data.frame(matrix(acc_subs,ncol=2,byrow=T))
# acc_subs$Half = 1
# 
# # Make times consistent
# for(i in 2:nrow(acc_subs)) {
#   if(acc_subs$V1[i] == "--"){
#     acc_subs$V1[i] = acc_subs$V1[i-1]
#   }
# }
# 
# for(i in 2:nrow(acc_subs)) {
#   if(acc_subs$V1[i] > acc_subs$V1[i-1]) {
#     acc_subs$Half[i] <- 2
#   }
# }
# 
# ## Make it so everything after is 2nd half
# acc_subs$Half <- cummax(acc_subs$Half)
# 
# acc_subs = subset(acc_subs, grepl("SUB", V2))
# acc_subs <- acc_subs %>%
#   separate(V2, into = c("Action", "Player"), sep = " by ")
# 
# # Based on ACC naming convention
# name_mapping <- setNames(
#   UVA_roster,
#   c("BUCHANAN,BLAKE", "HARRIS,DANTE", "BEEKMAN,REECE", "ROHDE,ANDREW",
#     "ROBERTS,DESMOND", "MURRAY,TAINE", "MCKNEELY,ISAAC", "GERTRUDE,ELIJAH",
#     "DUNN,RYAN", "ROBINSON,ANTHONY", "MINOR,JORDAN", "HOW,TRISTAN", "WALKER,BRYCE",
#     "BLISS,CHRISTIAN", "GROVES,JACOB", "BOND III,LEON", "CAVALIERS")
# )
# 
# acc_subs = acc_subs %>%
#   mutate(Player = mapvalues(Player, from = names(name_mapping), to = name_mapping))
# 

starting_lineup <- c("Isaac McKneely", "Reece Beekman", "Andrew Rohde", "Ryan Dunn", "Jacob Groves")
lineup_changes <- data.frame(Half = 1, Time = "20:00", Lineup = I(list(starting_lineup)), stringsAsFactors = FALSE)

# lineup_changes = add_row(lineup_changes,Half = 1,Time = '14:44',Lineup = I(list(c("Isaac McKneely", "Dante Harris", "Andrew Rohde", "Ryan Dunn", "Jacob Groves"))))
# lineup_changes = add_row(lineup_changes,Half = 1,Time = '12:14',Lineup = I(list(c("Reece Beekman", "Dante Harris", "Taine Murray", "Leon Bond III", "Jacob Groves"))))
# lineup_changes = add_row(lineup_changes,Half = 1,Time = '8:31',Lineup = I(list(c("Reece Beekman", "Dante Harris", "Taine Murray", "Leon Bond III", "Jordan Minor"))))
# lineup_changes = add_row(lineup_changes,Half = 1,Time = '7:33',Lineup = I(list(c("Reece Beekman", "Isaac McKneely", "Taine Murray", "Ryan Dunn", "Jordan Minor"))))
# lineup_changes = add_row(lineup_changes,Half = 1,Time = '6:25',Lineup = I(list(c("Reece Beekman", "Isaac McKneely", "Andrew Rohde", "Ryan Dunn", "Jordan Minor"))))
# lineup_changes = add_row(lineup_changes,Half = 1,Time = '5:19',Lineup = I(list(c("Dante Harris", "Isaac McKneely", "Andrew Rohde", "Ryan Dunn", "Blake Buchanan"))))
# lineup_changes = add_row(lineup_changes,Half = 1,Time = '3:59',Lineup = I(list(c("Dante Harris", "Isaac McKneely", "Andrew Rohde", "Leon Bond III", "Blake Buchanan"))))
# lineup_changes = add_row(lineup_changes,Half = 1,Time = '3:05',Lineup = I(list(c("Dante Harris", "Isaac McKneely", "Jacob Groves", "Leon Bond III", "Blake Buchanan"))))
# lineup_changes = add_row(lineup_changes,Half = 1,Time = '1:35',Lineup = I(list(c("Dante Harris", "Isaac McKneely", "Jacob Groves", "Leon Bond III", "Jordan Minor"))))
# lineup_changes = add_row(lineup_changes,Half = 1,Time = '0:51',Lineup = I(list(c("Dante Harris", "Taine Murray", "Jacob Groves", "Leon Bond III", "Jordan Minor"))))
# lineup_changes = add_row(lineup_changes,Half = 2,Time = '20:00',Lineup = I(list(c("Dante Harris", "Reece Beekman", "Ryan Dunn", "Andrew Rohde", "Blake Buchanan"))))
# lineup_changes = add_row(lineup_changes,Half = 2,Time = '15:55',Lineup = I(list(c("Dante Harris", "Reece Beekman", "Leon Bond III", "Taine Murray", "Blake Buchanan"))))
# lineup_changes = add_row(lineup_changes,Half = 2,Time = '15:30',Lineup = I(list(c("Dante Harris", "Andrew Rohde", "Leon Bond III", "Taine Murray", "Blake Buchanan"))))
# lineup_changes = add_row(lineup_changes,Half = 2,Time = '13:53',Lineup = I(list(c("Reece Beekman", "Andrew Rohde", "Leon Bond III", "Taine Murray", "Jacob Groves"))))
# lineup_changes = add_row(lineup_changes,Half = 2,Time = '11:47',Lineup = I(list(c("Reece Beekman", "Andrew Rohde", "Ryan Dunn", "Taine Murray", "Jacob Groves"))))
# lineup_changes = add_row(lineup_changes,Half = 2,Time = '9:42',Lineup = I(list(c("Dante Harris", "Andrew Rohde", "Ryan Dunn", "Taine Murray", "Jacob Groves"))))
# lineup_changes = add_row(lineup_changes,Half = 2,Time = '7:28',Lineup = I(list(c("Dante Harris", "Jordan Minor", "Ryan Dunn", "Taine Murray", "Jacob Groves"))))
# lineup_changes = add_row(lineup_changes,Half = 2,Time = '5:59',Lineup = I(list(c("Dante Harris", "Jordan Minor", "Leon Bond III", "Taine Murray", "Jacob Groves"))))
# lineup_changes = add_row(lineup_changes,Half = 2,Time = '3:10',Lineup = I(list(c("Bryce Walker", "Jordan Minor", "Leon Bond III", "Taine Murray", "Jacob Groves"))))
# lineup_changes = add_row(lineup_changes,Half = 2,Time = '2:25',Lineup = I(list(c("Bryce Walker", "Jordan Minor", "Leon Bond III", "Taine Murray", "Tristan How"))))


# unique_times <- unique(paste(acc_subs$Half, acc_subs$V1))
# lineup <- starting_lineup
# for(time in unique_times) {
#   half <- as.numeric(strsplit(time, " ")[[1]][1])
#   timestamp <- strsplit(time, " ")[[1]][2]
# 
#   current_subs <- acc_subs[acc_subs$Half == half & acc_subs$V1 == timestamp,]
# 
#   for(player_out in current_subs$Player[current_subs$Action == "SUB OUT"]) {
#     lineup <- lineup[lineup != player_out]
#   }
# 
#   for(player_in in current_subs$Player[current_subs$Action == "SUB IN"]) {
#     lineup <- c(lineup, player_in)
#   }
# 
#   lineup_changes <- rbind(lineup_changes, data.frame(Half = half,Time = timestamp, Lineup = I(list(lineup))))
# }
# 
# # Turn to string so it's more readable
lineup_changes$Lineup <- sapply(lineup_changes$Lineup, function(l) paste(paste("Cavaliers lineup change (",paste(l, collapse = ","),sep=""),")",sep="")  )
colnames(lineup_changes)[colnames(lineup_changes) == 'Lineup'] <- 'Description'




## Fox Sports Scraping and Cleaning
website1 = "https://www.foxsports.com/college-basketball/texas-am-aggies-vs-virginia-cavaliers-nov-29-2023-game-boxscore-246173?tab=playbyplay"
starting_lineup1 = "Isaac McKneely,Reece Beekman,Andrew Rohde,Ryan Dunn,Jacob Groves"
abbrev1 = "TXAM"
opp1 = "vsTXAM"

game = tibble(Half = 1, Time = "", Description = "", Event = "", Token = "", words = strsplit("place holder", split = ' '), Player = "", 
               UVA_score = 0, Opp_score = 0, Time_in_sec = 0, Opps = "",`Possession Number` = 0)

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
  game = game%>%mutate(Half=1)%>% relocate(Half,.before = Time)
  game$Half[which(game$Time == '0:00')[1]:nrow(game)] = 2
  
  game = full_join(game,lineup_changes,by=c('Half','Time','Description'))
  
  game$Time_in_sec = 0 
  for(i in 1:nrow(game)){
    min_sec = as.character(game$Time[i])
    if (str_length(min_sec) == 4) {
      min_sec = paste('0',min_sec, sep = '')
    }
    game$Time_in_sec[i] = as.numeric(substr(min_sec, 1, 2))*60 + as.numeric(substr(min_sec, 4, 5))
  }
  
  game = game %>% arrange(Half, desc(Time_in_sec))
  
  #starters = paste("Cavaliers lineup change (", starting_lineup, sep = '')
  #game = add_row(game, Half = 1,Time = "20:00", Description = paste(starters, ")", sep = ''),.before = 1)
  game = game[,c(2,3,1,4)]
  
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
  
  ### Cleaning for Possessions
  
  change_poss = c('Turnover','Def Rebound')
  made_shot = c('Made Two','Made Three')
  poss_vec = c()
  poss = ""
  for( i in 1:nrow(game)) {
    #Possesion Tracking
    if (grepl("vs.",game$Description[i])){
      if (unlist(strsplit(unlist(strsplit(game$Description[i],"[(]"))[2]," gains poss"))[1] %in% UVA_roster){
        poss = "Cavs"
      }
      else{poss = "Opp"}
    }
    else if(game$Event[i] %in% change_poss){
      if(poss == "Cavs"){poss = "Opp"}
      else{poss = "Cavs"}
    }
    else if(game$Event[i] %in% made_shot){
      if(i<nrow(game)){
        if(game$Token[i+1] == "shooting foul"){
          if((game$Player[i] %in% UVA_roster) == (game$Player[i+1] %in% UVA_roster)){
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
    else if(grepl("inbound",game$Description[i])){
      if (unlist(strsplit(game$Description[i],"inbound "))[2] == "Cavaliers"){poss = "Cavs"}
      else{poss = "Opp"}
    }
    else if(game$Event[i] == "Made FT"){
      fts = as.integer(unlist(strsplit(unlist(strsplit(game$Description[i],"free throw "))[2]," of ")))
      if(fts[1] == fts[2]){
        if(poss == "Cavs"){poss = "Opp"}
        else{poss = "Cavs"}
      }
    }
    poss_vec = c(poss_vec,poss)
  }
  game[,"Possession"] = poss_vec
  game[1,"Possession"] = "Cavs"
  num_poss = c()
  game_poss = 1
  game$Opps = opp
  for (i in 1:(nrow(game)-1)){
    num_poss =c(num_poss,game_poss)
    if (game$Opps[i]==game$Opps[i+1]){
      if (game$Possession[i]!=game$Possession[i+1]){game_poss = game_poss + 1}
    }
    else{
      game_poss = 0
    }
  }
  num_poss = c(num_poss,game_poss)
  game[,"Possession Number"] = num_poss
  
  game = na.omit(game)
  game
}

game1 = game_pbp(website1,starting_lineup1,abbrev1,opp1)
## end of playbyplay data for all games

# game1 = game1%>%select(-words)
# write.csv(game1, "TXAM_game.csv", row.names=FALSE)






### Stats for each Lineup while they are on the court together

Lineup_stats = tibble(On_court_time = 0,Possessions = 0, Pts= 0, Pts_against = 0, Tnovers = 0, FG_made = 0, FG_att = 0, Three_made =0, Three_att = 0,Rebounds = 0,Defensive_plays = 0)
starting = lapply(strsplit(starting_lineup1, split = ","),sort)
Lineup_stats = add_column(Lineup_stats,Lineup = starting,.before = "On_court_time")

Lineup_time = 0
i_lineup = 1
for (i in 2:nrow(game1)) {
  if (game1$Time_in_sec[i-1]>=game1$Time_in_sec[i]){
    Lineup_stats$On_court_time[i_lineup] = Lineup_stats$On_court_time[i_lineup] + (game1$Time_in_sec[i-1] - game1$Time_in_sec[i])
    Lineup_stats$Pts[i_lineup] = Lineup_stats$Pts[i_lineup] + (game1$UVA_score[i] - game1$UVA_score[i-1])
    Lineup_stats$Pts_against[i_lineup] = Lineup_stats$Pts_against[i_lineup] + (game1$Opp_score[i] - game1$Opp_score[i-1])
    Lineup_stats$Possessions[i_lineup] = Lineup_stats$Possessions[i_lineup] + (game1$`Possession Number`[i] - game1$`Possession Number`[i-1])
  } 
  if (game1$Event[i] == "Cav Sub"){
    Players = lapply(strsplit(gsub("\\)","",gsub("Cavaliers lineup change \\(", "", game1$Description[i])), split = ", "),sort)
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
  } else if (game1$Player[i] %in% UVA_roster) {
    if (game1$Event[i] == "Block"){
      Lineup_stats$Defensive_plays[i_lineup] = Lineup_stats$Defensive_plays[i_lineup] + 1
    } else if(game1$Event[i] == "Off Rebound" || game1$Event[i] == "Def Rebound"){
      Lineup_stats$Rebounds[i_lineup] = Lineup_stats$Rebounds[i_lineup] + 1
    } else if(game1$Event[i] == "Made Two"){
      Lineup_stats$FG_made[i_lineup] = Lineup_stats$FG_made[i_lineup] + 1
      Lineup_stats$FG_att[i_lineup] = Lineup_stats$FG_att[i_lineup] + 1
    } else if(game1$Event[i] == "Made Three"){
      Lineup_stats$FG_made[i_lineup] = Lineup_stats$FG_made[i_lineup] + 1
      Lineup_stats$FG_att[i_lineup] = Lineup_stats$FG_att[i_lineup] + 1
      Lineup_stats$Three_made[i_lineup] = Lineup_stats$Three_made[i_lineup] + 1
      Lineup_stats$Three_att[i_lineup] = Lineup_stats$Three_att[i_lineup] + 1
    } else if(game1$Event[i] == "Missed Two"){
      Lineup_stats$FG_att[i_lineup] = Lineup_stats$FG_att[i_lineup] + 1
    } else if(game1$Event[i] == "Missed Three"){
      Lineup_stats$FG_att[i_lineup] = Lineup_stats$FG_att[i_lineup] + 1
      Lineup_stats$Three_att[i_lineup] = Lineup_stats$Three_att[i_lineup] + 1
    } else if(game1$Event[i] == "Turnover"){
      Lineup_stats$Tnovers[i_lineup] = Lineup_stats$Tnovers[i_lineup] + 1
    } 
  } else if (game1$Event[i] == "Turnover") {
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


# Individual Box Scores
#games = read.csv("/Users/mehulpol/SASL/games.csv")

Box_Score = data.frame(Player = "", Pts= 0, Assists = 0, Tnovers = 0, 
                       FT_made = 0, FT_att = 0, FG_made = 0, FG_att = 0, 
                       Three_made = 0, Three_att = 0, Off_Rebounds = 0, Def_Rebounds = 0,
                       Steals = 0, Blocks = 0, fouls = 0)
for (i_lineup in 1:length(UVA_roster[-length(UVA_roster)])) {
  Box_Score = Box_Score %>%
    add_row(Player = UVA_roster[i_lineup], Pts= 0, Assists = 0,Tnovers = 0, 
            FT_made = 0, FT_att = 0, FG_made = 0, FG_att = 0, 
            Three_made =0, Three_att = 0, Off_Rebounds = 0, Def_Rebounds = 0,
            Steals = 0, Blocks = 0, fouls = 0)
  p_plays = subset(game1,game1$Player == UVA_roster[i_lineup])
  if (nrow(p_plays)>0){
    for (i in 1:nrow(p_plays)){
    if (p_plays$Event[i] == "Block"){
      Box_Score$Blocks[i_lineup + 1] = Box_Score$Blocks[i_lineup + 1] + 1
    } else if(p_plays$Event[i] == "Off Rebound"){
      Box_Score$Off_Rebounds[i_lineup + 1] = Box_Score$Off_Rebounds[i_lineup + 1] + 1
    } else if(p_plays$Event[i] == "Def Rebound"){
      Box_Score$Def_Rebounds[i_lineup + 1] = Box_Score$Def_Rebounds[i_lineup + 1] + 1
    } else if(p_plays$Event[i] == "Made FT"){
      Box_Score$FT_made[i_lineup + 1] = Box_Score$FT_made[i_lineup + 1] + 1
      Box_Score$FT_att[i_lineup + 1] = Box_Score$FT_att[i_lineup + 1] + 1
      Box_Score$Pts[i_lineup + 1] = Box_Score$Pts[i_lineup + 1] + 1
    } else if(p_plays$Event[i] == "Made Two"){
      Box_Score$FG_made[i_lineup + 1] = Box_Score$FG_made[i_lineup + 1] + 1
      Box_Score$FG_att[i_lineup + 1] = Box_Score$FG_att[i_lineup + 1] + 1
      Box_Score$Pts[i_lineup + 1] = Box_Score$Pts[i_lineup + 1] + 2
    } else if(p_plays$Event[i] == "Made Three"){
      Box_Score$FG_made[i_lineup + 1] = Box_Score$FG_made[i_lineup + 1] + 1
      Box_Score$FG_att[i_lineup + 1] = Box_Score$FG_att[i_lineup + 1] + 1
      Box_Score$Three_made[i_lineup + 1] = Box_Score$Three_made[i_lineup + 1] + 1
      Box_Score$Three_att[i_lineup + 1] = Box_Score$Three_att[i_lineup + 1] + 1
      Box_Score$Pts[i_lineup + 1] = Box_Score$Pts[i_lineup + 1] + 3
    } else if(p_plays$Event[i] == "Missed FT"){
      Box_Score$FT_att[i_lineup + 1] = Box_Score$FT_att[i_lineup + 1] + 1
    } else if(p_plays$Event[i] == "Missed Two"){
      Box_Score$FG_att[i_lineup + 1] = Box_Score$FG_att[i_lineup + 1] + 1
    } else if(p_plays$Event[i] == "Missed Three"){
      Box_Score$FG_att[i_lineup + 1] = Box_Score$FG_att[i_lineup + 1] + 1
      Box_Score$Three_att[i_lineup + 1] = Box_Score$Three_att[i_lineup + 1] + 1
    } else if(p_plays$Event[i] == "Turnover"){
      Box_Score$Tnovers[i_lineup + 1] = Box_Score$Tnovers[i_lineup + 1] + 1
    } else if(p_plays$Event[i] == "Foul"){
      Box_Score$fouls[i_lineup + 1] = Box_Score$fouls[i_lineup + 1] + 1
    } 
  }
  }
  assist_line = paste(UVA_roster[i_lineup],'assists', sep = ' ')
  Box_Score$Assists[i_lineup + 1] = nrow(game1[grepl(assist_line,game1$Description),])
  steal_line = paste(UVA_roster[i_lineup],'steals', sep = ' ')
  Box_Score$Steals[i_lineup + 1] = nrow(game1[grepl(steal_line,game1$Description),])
}

Box_Score = Box_Score[-1,]
Box_Score = Box_Score%>%
  mutate(PER = FG_made*85.910 + Steals*53.897 + Three_made*51.757 +
           FT_made*46.845 + Blocks*39.190 + Def_Rebounds*14.707 + Off_Rebounds*39.190 +
           Assists*34.677 - fouls*17.174 - (FT_att-FT_made)*20.091 - 
           (FG_att-FG_made)*39.190 - Tnovers*53.897,.before = Pts)

# Things not put into box score: offensive fouls
# Extra stats to add = Adjusted Plus/minus + PER + VOR + VA + EWA

All_Stats = merge(Player_stats, Box_Score, by = "Player", all.x = TRUE) %>%
  mutate(Pt_diff_perposs = (Pts.x-Pts_against)/(Possessions), .before = Pt_diff_permin)%>%
  mutate(poss_permin = Possessions/(On_court_time/60), .before = Pt_diff_perposs)%>%
  mutate(efficiency = (Pts.x + Def_Rebounds + Off_Rebounds + Defensive_plays - (FG_att.x - FG_made.x) - Tnovers.x) / (On_court_time/60), .before = Pt_diff_perposs)%>%
  mutate(def_eff = 2*Defensive_plays/Possessions, .after = Pt_diff_permin)%>%
  mutate(PER = PER/(On_court_time/60))

# write.csv(All_Stats, "TXAM_stats.csv", row.names=FALSE)

## Visualizations



