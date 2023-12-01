
library(tidyverse)
library(useful)

UVA_roster = c("Blake Buchanan","Dante Harris","Reece Beekman","Andrew Rohde",
               "Desmond Roberts","Taine Murray","Isaac McKneely","Elijah Gertrude",
               "Ryan Dunn","Anthony Robinson","Jordan Minor","Tristan How", "Bryce Walker",
               "Christian Bliss","Jacob Groves","Leon Bond III","Cavaliers")

TARL_stats = read.csv('TARL_stats.csv')
TARL_stats$Player[TARL_stats$Player == "Jake Groves"] = "Jacob Groves"
TARL_stats$Player[TARL_stats$Player == "Without Jake Groves"] = "Without Jacob Groves"
FLA_stats = read.csv('FLA_stats.csv')
NCAT_stats = read.csv('NCAT_stats.csv')
TXSO_stats = read.csv('TXSO_stats.csv')
WIS_stats = read.csv('WIS_stats.csv')
WVU_stats = read.csv('WVU_stats.csv')
TXAM_stats = read.csv('TXAM_stats.csv')

szn_stats = TARL_stats %>% 
  full_join(FLA_stats)%>%
  full_join(NCAT_stats)%>%
  full_join(TXSO_stats)%>%
  full_join(WIS_stats)%>%
  full_join(WVU_stats)%>%
  full_join(TXAM_stats)%>%
  group_by(Player)%>%
  summarise_all(sum)

szn_stats = szn_stats%>%
  mutate(Minutes = round((On_court_time/60),2))%>%
  mutate(PER = (FG_made.y*85.910 + Steals*53.897 + Three_made.y*51.757 +
           FT_made*46.845 + Blocks*39.190 + Def_Rebounds*14.707 + Off_Rebounds*39.190 +
           Assists*34.677 - fouls*17.174 - (FT_att-FT_made)*20.091 - 
           (FG_att.y-FG_made.y)*39.190 - Tnovers.y*53.897)/(On_court_time/60))%>%
  mutate(Pt_diff_perposs = (Pts.x-Pts_against)/(Possessions))%>%
  mutate(Pt_diff_permin = (Pts.x-Pts_against)/(On_court_time/60))%>%
  mutate(poss_permin = Possessions/(On_court_time/60))%>%
  mutate(efficiency = (Pts.x + Def_Rebounds + Off_Rebounds + Defensive_plays - (FG_att.x - FG_made.x) - Tnovers.x) / (On_court_time/60))%>%
  mutate(def_eff = 2*Defensive_plays/Possessions)
szn_stats$Pt_dif <- szn_stats$Pts.x - szn_stats$Pts_against

plusminus <- data.frame(Player = szn_stats$Player, Pt_dif = szn_stats$Pt_dif)

without <- plusminus[grepl("Without", plusminus$Player), ]
with <- plusminus[!grepl("Without", plusminus$Player), ]
without$Player <- gsub("Without ", "", without$Player)

with$adjusted_plusminus <- NA

for (i in 1:nrow(with)) {
  player_name <- with$Player[i]
  without_row <- which(without$Player == player_name)
  with$adjusted_plusminus[i] <- with$Pt_dif[i] - without$Pt_dif[without_row]
}

szn_stats <- merge(szn_stats, with[, c("Player", "adjusted_plusminus")], by = "Player", all.x = TRUE)
# write.csv(szn_stats, "Nov_playerstats.csv", row.names=FALSE)

## Stats for each Lineup while they are on the court together

FLA_game = read.csv('FLA_game.csv')
NCAT_game = read.csv('NCAT_game.csv')
TXSO_game = read.csv('TXSO_game.csv')
WIS_game = read.csv('WIS_game.csv')
WVU_game = read.csv('WVU_game.csv')
TXAM_game = read.csv('TXAM_game.csv')

game1 = TARL_game %>% 
  full_join(FLA_game)%>%
  full_join(NCAT_game)%>%
  full_join(TXSO_game)%>%
  full_join(WIS_game)%>%
  full_join(WVU_game)%>%
  full_join(TXAM_game)
Lineup_stats = tibble(On_court_time = 0,Possessions = 0, Pts= 0, Pts_against = 0, Tnovers = 0, FG_made = 0, FG_att = 0, Three_made =0, Three_att = 0,Rebounds = 0,Defensive_plays = 0)
starting_lineup1 = "Isaac McKneely,Reece Beekman,Andrew Rohde,Ryan Dunn,Jacob Groves"
starting = lapply(strsplit(starting_lineup1, split = ","),sort)
Lineup_stats = add_column(Lineup_stats,Lineup = starting,.before = "On_court_time")

Lineup_time = 0
i_lineup = 1
for (i in 2:nrow(game1)) {
  if (game1$Time_in_sec[i-1]>=game1$Time_in_sec[i]){
    Lineup_stats$On_court_time[i_lineup] = Lineup_stats$On_court_time[i_lineup] + (game1$Time_in_sec[i-1] - game1$Time_in_sec[i])
    Lineup_stats$Pts[i_lineup] = Lineup_stats$Pts[i_lineup] + (game1$UVA_score[i] - game1$UVA_score[i-1])
    Lineup_stats$Pts_against[i_lineup] = Lineup_stats$Pts_against[i_lineup] + (game1$Opp_score[i] - game1$Opp_score[i-1])
    Lineup_stats$Possessions[i_lineup] = Lineup_stats$Possessions[i_lineup] + (game1$Possession.Number[i] - game1$Possession.Number[i-1])
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
  } else if (game1$Event[i] == "Off Rebound"){
    Lineup_stats$Opp_OReb[i_lineup] = Lineup_stats$Opp_OReb[i_lineup] + 1
  } else if (game1$Event[i] == "Def Rebound"){
    Lineup_stats$Opp_DReb[i_lineup] = Lineup_stats$Opp_DReb[i_lineup] + 1
  } else if (game1$Event[i] == "Turnover") {
    Lineup_stats$Defensive_plays[i_lineup] = Lineup_stats$Defensive_plays[i_lineup] + 1
  }
}
# Combine alike lineups
for (i in 1:nrow(Lineup_stats)){
  if (length(Lineup_stats$Lineup[i])==1){
    lu = strsplit(Lineup_stats$Lineup[i][[1]], ",")
  } else {lu = Lineup_stats$Lineup[i]}
  lu = lapply(lu,sort)
  Lineup_stats$Lineup[i] = paste(unlist(lu),collapse=",")
}
Lineup_stats$Lineup = as.character(Lineup_stats$Lineup)
for (i in 1:nrow(Lineup_stats)){
  Lineup_stats$Lineup[i] = gsub("Keenly","Kneely",Lineup_stats$Lineup[i])
  Lineup_stats$Lineup[i] = gsub("Jake","Jacob",Lineup_stats$Lineup[i])
}
Lineup_stats = Lineup_stats%>%
  group_by(Lineup)%>%
  summarise_all(sum)
 

# Template for sub-lineups

Beek_lineups = Lineup_stats[grepl("Beekman", Lineup_stats$Lineup),]
BeekXDunn = Beek_lineups[grepl("Dunn", Beek_lineups$Lineup), -1] %>%
  summarize_all(sum)
WithoutBeekXDunn = Lineup_stats[-grepl("Beekman", Lineup_stats$Lineup),]
WithoutBeekXDunn = WithoutBeekXDunn[-grepl("Dunn", WithoutBeekXDunn$Lineup),-1] %>%
  summarize_all(sum)

BeekXDunn = mutate(BeekXDunn, Lineup = "With Beekman and Dunn", .before=On_court_time)
WithoutBeekXDunn = mutate(WithoutBeekXDunn, Lineup = "Without Beekman and Dunn", .before=On_court_time)
BeekXDunn = rbind(BeekXDunn, WithoutBeekXDunn)
BeekXDunn = BeekXDunn %>%
  mutate(Pt_diff_perposs = (Pts-Pts_against)/(Possessions),.before = Pts)%>%
  mutate(Pt_diff_permin = (Pts-Pts_against)/(On_court_time),.before = Pts)%>%
  mutate(poss_permin = Possessions/(On_court_time/60),.before = Pts)%>%
  mutate(efficiency = (Pts + Rebounds + Defensive_plays - (FG_att - FG_made) - Tnovers) / (On_court_time/60),.before = Pts)%>%
  mutate(def_eff = 2*Defensive_plays/Possessions,.before = Pts)



Lineup_stats = Lineup_stats%>%
  mutate(Minutes = round((On_court_time/60),2))%>%
  mutate(Pt_diff_perposs = (Pts-Pts_against)/(Possessions),.before = Pts)%>%
  mutate(Pt_diff_permin = (Pts-Pts_against)/(On_court_time),.before = Pts)%>%
  mutate(poss_permin = Possessions/(On_court_time/60),.before = Pts)%>%
  mutate(efficiency = (Pts + Rebounds + Defensive_plays - (FG_att - FG_made) - Tnovers) / (On_court_time/60),.before = Pts)%>%
  mutate(def_eff = 2*Defensive_plays/Possessions,.before = Pts)

# write.csv(szn_stats, "Nov_stats.csv", row.names=FALSE)
# write.csv(Lineup_stats, "First_4Lineup_stats.csv", row.names=FALSE)
# Lineup_Stats = read.csv("/Users/mehulpol/SASL/First_4Lineup_stats.csv")
# combine with new if new

Tm_minutes = max(szn_stats$Minutes)
Tm_Reb = max(szn_stats$Rebounds)
Opp_Reb = max(szn_stats$Opp_DReb) + max(szn_stats$Opp_OReb)

szn_stats$Reb_Rate = (szn_stats$Rebounds * (Tm_minutes / 5)) / (szn_stats$Minutes * (Tm_Reb + Opp_Reb))
#Visualizations

szn_stats %>%
  filter(Player %in% UVA_roster)%>%
  filter(Pt_dif>20)%>%
  ggplot(aes(x=reorder(Player, +Pt_dif),y=Pt_dif)) + geom_col() + geom_text(aes(label = Minutes), vjust = -0.2) +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) +
  labs(title = "Top Point Differentials Amongst Players Labeled with Minutes on Court",y="Point Differential While on the Court", x="Player")

szn_stats %>%
  filter(Player %in% UVA_roster)%>%
  filter(Minutes>100)%>%
  ggplot(aes(x=reorder(Player, +Rebounds/Minutes),y=Rebounds/Minutes)) + geom_col() +
  theme(axis.text.x = element_text(angle=60, hjust = 1)) +
  labs(title = "Most Team Rebounds per Minute when on the Court",y="Rebounds per Minute", x="Player")
