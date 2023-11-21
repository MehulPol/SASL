
library(tidyverse)

TARL_stats = read.csv("/Users/mehulpol/SASL/TARL_stats.csv")
FLA_stats = read.csv("/Users/mehulpol/SASL/FLA_stats.csv")
NCAT_stats = read.csv("/Users/mehulpol/SASL/NCAT_stats.csv")
TXSO_stats = read.csv("/Users/mehulpol/SASL/TXSO_stats.csv")

szn_stats = TARL_stats %>% 
  full_join(FLA_stats)%>%
  full_join(NCAT_stats)%>%
  full_join(TXSO_stats)%>%
  group_by(Player)%>%
  summarise_all(sum)
#Only weird thing is the Jacob Groves is also called Jake Groves in some places

szn_stats = szn_stats%>%
  mutate(PER = (FG_made.y*85.910 + Steals*53.897 + Three_made.y*51.757 +
           FT_made*46.845 + Blocks*39.190 + Def_Rebounds*14.707 + Off_Rebounds*39.190 +
           Assists*34.677 - fouls*17.174 - (FT_att-FT_made)*20.091 - 
           (FG_att.y-FG_made.y)*39.190 - Tnovers.y*53.897)/(On_court_time/60))%>%
  mutate(Pt_diff_perposs = (Pts.x-Pts_against)/(Possessions))%>%
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
