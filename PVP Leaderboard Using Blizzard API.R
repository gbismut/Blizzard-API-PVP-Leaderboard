library(httr)
library(tidyverse)

#Function to call Blizzard API
blizz <- function(endpoint, locale = "en_US", namespace = NULL, json = FALSE) {
  x <- httr::GET(
    url = glue::glue("https://us.api.blizzard.com{endpoint}"),
    query = list(
      namespace = namespace,
      locale = locale,
      access_token = Sys.getenv("BLIZZARD_AUTH_TOKEN")
    )
  )
  
  if(httr::http_error(x))
    stop(httr::http_status(x)[["message"]])
  
  x <- httr::content(x) # grab the request content
  
  if(json) {
    jsonlite::toJSON(x, pretty = TRUE)
  } else {
    x <- jsonlite::toJSON(x)
    jsonlite::fromJSON(x)
  }
}

#
Specs = blizz("/data/wow/playable-specialization/index", "en_US", "static-us", json = FALSE)
Specs = as.data.frame(Specs[2])

PlayableSpecs = data.frame()

for(i in unlist(unique(Specs$character_specializations.id))){
  SpecID = unlist(Specs$character_specializations.id[Specs$character_specializations.id == i])
  print(SpecID)
  pull = blizz(paste0("/data/wow/playable-specialization/",SpecID), "en_US", "static-us", json = FALSE)
  newData = data.frame(Class = pull[3]$playable_class$name, Spec = pull[4]$name)
  PlayableSpecs=bind_rows(PlayableSpecs, newData)
}

#Concatenate class and spec, then remove white space
PlayableSpecs$Concatenate = paste0(tolower(PlayableSpecs$Class),"-",tolower(PlayableSpecs$Spec))
PlayableSpecs$Concatenate = str_replace_all(PlayableSpecs$Concatenate, fixed(" "), "")

#Pull full soloshuffle ladder (currently slow)
tmp_df = data.frame()
SoloShuffle = data.frame()

for(i in PlayableSpecs$Concatenate){
  print(i)
  tmp_df = blizz(paste0("/data/wow/pvp-season/34/pvp-leaderboard/shuffle-",i), 
                 "en_US", "dynamic-us", json = FALSE)
  tmp_df = as.data.frame(tmp_df)
  SoloShuffle = bind_rows(SoloShuffle, tmp_df)
}

#Interesting idea
#Daily Batch, check if total played games has changed 
#Can check if total played games hasnt changed over a week
ShuffleStats = SoloShuffle
ShuffleStats$entries.season_match_statistics$won = as.numeric(ShuffleStats$entries.season_match_statistics$won)
ShuffleStats$entries.season_match_statistics$played = as.numeric(ShuffleStats$entries.season_match_statistics$played)
ShuffleStats$WinRate = (ShuffleStats$entries.season_match_statistics$won/ShuffleStats$entries.season_match_statistics$played)*100
ShuffleStats$entries.rating = as.numeric(ShuffleStats$entries.rating)

#See Win Rate by Spec
WinRateSpec = ShuffleStats %>% 
  group_by(name) %>% 
  summarise(
    AvgWinRate = mean(WinRate),
    MedianWinRate = median(WinRate),
    CountOfPlayers = n(),
    NumberOfMatchesPlayed = sum(entries.season_match_statistics$played),
    AvgRating = mean(entries.rating),
    MinRating = min(entries.rating),
    maxRating = max(entries.rating)
  )

colnames(SoloShuffle)

View(WinRateSpec)
WinRateSpec$name = gsub('shuffle-','', WinRateSpec$name)
unique(WinRateSpec$name)
ggplot(data=WinRateSpec, aes(x=name, y=AvgWinRate)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#3v3
threes = blizz("/data/wow/pvp-season/34/pvp-leaderboard/3v3", "en_US", "dynamic-us", json = FALSE)
threes = as.data.frame(threes)

ArmsWarrior = blizz("/data/wow/pvp-season/34/pvp-leaderboard/shuffle-warrior-arms", "en_US", "dynamic-us", json=FALSE)

View(ArmsWarrior)
View(as.data.frame(ArmsWarrior))
