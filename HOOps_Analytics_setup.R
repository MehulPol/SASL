
library(tidyverse)
games = read.csv("/Users/mehulpol/SASL/games.csv")

# UVA_roster = c("Blake Buchanan","Dante Harris","Reece Beekman","Andrew Rohde",
#                "Desmond Roberts","Taine Murray","Isaac Mckneely","Elijah Gertrude",
#                "Ryan Dunn","Anthony Robinson","Jordan Minor","Tristan How",
#                "Christian Bliss","Jake Groves","Leon Bond III","Cavaliers")

UVA_roster = c("Kihei Clark", "Reece Beekman", "Armaan Franklin", "Jayden Gardner", 
               "Ben Vander Plas", "Kadin Shedrick", "Francisco Caffaro", "Ryan Dunn",
               "Isaac McKneely", "Taine Murray", "Chase Coleman", "Tristan How", "Cavaliers")
# Possession tracking

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
#write.csv(games, "games.csv", row.names=FALSE)




# Individual Box Scores
games = read.csv("/Users/mehulpol/SASL/games.csv")

Box_Score = data.frame(Player = "", Pts= 0, Tnovers = 0, 
                      FT_made = 0, FT_att = 0, FG_made = 0, FG_att = 0, 
                      Three_made = 0, Three_att = 0, Rebounds = 0, 
                      Steals = 0, Blocks = 0, fouls = 0)
for (i_lineup in 1:length(UVA_roster[-length(UVA_roster)])) {
  Box_Score = Box_Score %>%
    add_row(Player = UVA_roster[i_lineup], Pts= 0, Tnovers = 0, 
            FT_made = 0, FT_att = 0, FG_made = 0, FG_att = 0, 
            Three_made =0, Three_att = 0, Rebounds = 0, 
            Steals = 0, Blocks = 0, fouls = 0)
  p_plays = subset(games,games$Player == UVA_roster[i_lineup])
  for (i in 1:nrow(p_plays)){
    if (p_plays$Event[i] == "Block"){
      Box_Score$Blocks[i_lineup + 1] = Box_Score$Blocks[i_lineup + 1] + 1
    } else if(p_plays$Event[i] == "Off Rebound" || p_plays$Event[i] == "Def Rebound"){
      Box_Score$Rebounds[i_lineup + 1] = Box_Score$Rebounds[i_lineup + 1] + 1
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

Box_Score = Box_Score[-1,]

# Things not put into box score: offensive fouls + steals

