
library(tidyverse)
games = read.csv("/Users/mehulpol/SASL/g.csv")

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

write.csv(games, "games.csv", row.names=FALSE)

# individual box scores
games = read.csv("/Users/mehulpol/SASL/games.csv")
