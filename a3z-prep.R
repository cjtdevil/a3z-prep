library(readxl);library(tidyverse)
df <- read_excel("Player Master List (1).xlsx")

fun.teams = function(teams){
  if(length(teams)>1){
    allteams = unique(teams)
    out = paste(allteams,collapse = '/')
  }else{
    out = teams
  }
  return(out)
}

tableau <- df %>%
  mutate(carryagainst60 = 60*Carries...24/`5v5 TOI`,
         EntriesAllowed = Targets - Denials,
         Pos. = trimws(Pos.)) %>%
  select(Player = Players,
         Team,
         Pos=Pos.,
         TOI = `5v5 TOI`,
         Entries = `Zone Entries`,
         PEntries = Carries...15,
         Exits = `Zone Exits`,
         PExits = `Exits w/ Possession`,
         EntriesAllowed = EntriesAllowed,
         PEntriesAllowed = Carries...24,
         iSF = `Shots`,
         isA = Passes) %>%
  group_by(Player) %>%
  mutate(Team = fun.teams(Team),
         Pos= fun.teams(Pos)) %>%
  group_by(Player,Team,Pos) %>%
  summarise_all(sum) %>%
  mutate(PE60 = 60*PEntries/TOI,
         PX60 = 60*PExits/TOI,
         PEA60 = 60*PEntriesAllowed/TOI,
         iSF60 = 60*iSF/TOI,
         isA60 = 60*isA/TOI,
         PEperc = 100*PEntries/Entries,
         PXperc = 100*PExits/Exits,
         PEAperc = 100*PEntriesAllowed/EntriesAllowed,
         PEA60 = ifelse(Pos=="D",PEA60,NA),
         PEAperc = ifelse(Pos=="D",PEAperc,NA)) %>% ungroup %>%
  filter(TOI>100) %>% ungroup %>% group_by(Pos) %>%
  mutate_at(vars(PE60:PEAperc), .funs = list(ptile = ~100*pnorm(scale(.)))) %>% ungroup %>%
  mutate(PEAperc_ptile = 100-PEAperc_ptile,
         PEA60_ptile = 100-PEA60_ptile) %>%
  select(Player:TOI,PE60:PEAperc_ptile) %>%
  pivot_longer(cols=PE60:PEAperc_ptile) %>%
  mutate(type = ifelse(grepl("ptile",name),"percentile","raw_value"),
         type_name = gsub("_ptile","",name)) %>%
  pivot_wider(id_cols = c(Player:TOI,type_name),names_from = type,values_from = value) %>%
  mutate(percentile = round(percentile,1),
         raw_value = round(raw_value,1),
         TOI = round(TOI),
         category = ifelse(type_name %in% c("PE60","PEperc","PE60_ptile","PEperc_ptile"),"Entries",
                           ifelse(type_name %in% c("PX60","PXperc","PX60_ptile","PXperc_ptile"),"Exits",
                                  ifelse(type_name %in% c("PEA60","PEAperc","PEA60_ptile","PEAperc_ptile"),"Entries Allowed",
                                         "Shot Contributions")))) %>%
  filter(!is.na(raw_value),!is.na(percentile))


write.csv(tableau,"tableau.csv")
