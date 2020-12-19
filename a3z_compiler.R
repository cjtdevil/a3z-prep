# Library loading
library(tidyverse);library(readxl);library(utils)

# Create "TM1/TM2" Format
fun.teams = function(teams){
  if(length(teams)>1){
    allteams = unique(teams)
    out = paste(allteams,collapse = '/')
  }else{
    out = teams
  }
  return(out)
}

# Read in Sznajder data 
raw <- read_excel("All Three Zones Master Sheet.xlsx") # via https://www.patreon.com/CSznajder/posts

# Clean Data
clean <- raw %>%
  select(player=Players,team=Team,pos=Pos.,season=Year,
         toi = `5v5 TOI`,
         iSF=Shots,sA=Passes,
         entries = `Zone Entries`,cEntries=Carries...16,
         exits = `Exit Attempts`,cExits=`Exits w/ Possession`,
         targets = Targets, cEA = Carries...25,) %>%
  mutate(player = toupper(player),
         season = as.numeric(substr(season,1,4))) %>%
  mutate(scode = 10^(season - 2016))  # Make seasons 4-char binary, one for each included season

# Get season codes for loop
scodes <- clean$scode %>% unique

# Sum player performance through all combinations of seasons
for(i in 1:length(scodes)){
  temp_scodes <- combn(scodes,i)  %>% data.frame()
  for(j in 1:ncol(temp_scodes)){
    
    scodes_temp <- temp_scodes[,j]
    
    temp <- clean %>%
      filter(scode %in% scodes_temp) %>%
      group_by(player) %>%
      mutate(team = fun.teams(team)) %>%
      group_by(player,pos,team) %>%
      summarise_at(vars(toi:cEA),funs(sum)) %>%
      mutate(scode = sum(scodes_temp))
    
    
    if(j==1 & i==1){
      out <- temp
    }else{
      out <- out %>%
        bind_rows(temp)
    }
  }
}

# Calculate per 60s
df_final <- out %>% ungroup %>% 
  mutate(cEA = ifelse(pos=="F",NA,cEA)) %>%
  mutate(cE_perc = cEntries/entries,
         cEA_perc = cEA/targets,
         cEx_perc = cExits/exits) %>%
  mutate_at(vars(iSF:cEA),list(~60*./toi)) %>%
  select(-entries,-exits,-targets) %>%
  
# Make data "long" form for tableau  
  pivot_longer(cols = c(iSF:cEA,cE_perc:cEx_perc,),names_to = 'metric',values_to = 'value') %>%

# Calculate percentiles by position
  group_by(metric,scode,pos) %>%
  mutate(zscore = pnorm(scale(value)),
         zscore = ifelse(metric %in% c('cEA','cEA_perc'),1-zscore,zscore)) %>%
  mutate(cat = ifelse(metric %in% c('iSF','sA'),'shot contr',
                      ifelse(metric %in% c('cEntries','cE_perc'),'entries',
                             ifelse(metric %in% c('cExits','cEx_perc'),'exits',
                                    'entry def')))) %>%
  arrange(player,scode) %>%
  filter(!is.na(metric)) %>%
  mutate(player=gsub("BRANDON GAUNCE","BRENDAN GAUNCE",player),
         player=gsub("BRANDON PERLINI","BRENDAN PERLINI",player),
         player=gsub("COLLIN MILLER","COLIN MILLLER",player),
         player=gsub("FREDERIK GAUDREAU","FREDERICK GAUDREAU",player),
         player=gsub("ISAC LUNDERSTROM","ISAC LUNDESTROM",player),
         player=gsub("JAROME IGINLIA","JAROME IGINLA",player),
         player=gsub("JESSE PULUJARVI","JESSE PULJUJARVI",player),
         player=gsub("JOAKIM NORSTROM","JOAKIM NORDSTROM",player),
         player=gsub("JOSH MORRSSEY","JOSH MORRISSEY",player),
         player=gsub("KEVIN LABANC","KEVIN LEBANC",player),
         player=gsub("KORBIANIAN HOLZER","KORBINIAN HOLZER",player),
         player=gsub("MARCUS PETERSSON","MARCUS PETTERSSON",player),
         player=gsub("MARCUS SORENSON","MARCUS SORENSEN",player),
         player=gsub("PETER CEHLARIIK","PETER CEHLARIK",player),
         player=gsub("PHILIP DANAULT","PHILLIP DANAULT",player),
         player=gsub("ROBERT HAAG","ROBERT HAGG",player),
         player=gsub("ROSS JOHNSOTN","ROSS JOHNSTON",player),
         player=gsub("STEFAN NOENSEN","STEFAN NOESEN",player),
         player=gsub("TOBIAS ENSTROM","TOBY ENSTROM",player),
         player=gsub("TROY STETCHER","TROY STECHER",player),
         player=gsub("URHO VAAKANINEN","URHO VAAKANAINEN",player),
         player=gsub("VLADISLAV KAMANEV","VLADISLAV KAMENEV",player))




  

# Output to CSV
write.csv(df_final,file="a3z.csv")
