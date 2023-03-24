rm(list = ls())
setwd("~/GitHub/Blizzard API")

library(httr)
library(tidyverse)
library(writexl)
library(dbplyr)
library(dplyr)



#Load Database
slb <- DBI::dbConnect(RSQLite::SQLite(), "SoloShuffleLeaderboard.db") #slb = shuffle leaderboard
src_dbi(slb)
tbl(slb, sql("SELECT * FROM `SoloShuffle2023-03-22`"))
my_db_file = "SoloShuffleLeaderboard.db"
my_db <- src_sqlite(my_db_file)



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

#Convert to numeric
ShuffleStats = SoloShuffle
ShuffleStats$entries.season_match_statistics$won = as.numeric(ShuffleStats$entries.season_match_statistics$won)
ShuffleStats$entries.season_match_statistics$played = as.numeric(ShuffleStats$entries.season_match_statistics$played)
ShuffleStats$WinRate = (ShuffleStats$entries.season_match_statistics$won/ShuffleStats$entries.season_match_statistics$played)*100
ShuffleStats$entries.rating = as.numeric(ShuffleStats$entries.rating)

#Unlist CharID to merge later
ShuffleStats$CharID = unlist(ShuffleStats$entries.character$id)
ShuffleStats$ShufflePlayed = unlist(ShuffleStats$entries.season_match_statistics$played)
ShuffleStats$ShuffleWon = unlist(ShuffleStats$entries.season_match_statistics$won)
ShuffleStats$ShuffleLost = unlist(ShuffleStats$entries.season_match_statistics$lost)


#See Win Rate by Spec
WinRateSpec = ShuffleStats %>% 
  group_by(name) %>% 
  dplyr::summarise(
    AvgWinRate = mean(WinRate),
    MedianWinRate = median(WinRate),
    CountOfPlayers = n(),
    NumberOfMatchesPlayed = sum(entries.season_match_statistics$played),
    AvgRating = mean(entries.rating),
    MinRating = min(entries.rating),
    MaxRating = max(entries.rating)
  )


WinRateSpec$name = gsub('shuffle-','', WinRateSpec$name)

ggplot(data=WinRateSpec, aes(x=name, y=AvgWinRate)) +
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  aes(x = reorder(name, AvgWinRate),decreasing = TRUE)


#3v3
ThreesAPI = blizz("/data/wow/pvp-season/34/pvp-leaderboard/3v3", "en_US", "dynamic-us", json = FALSE)
ThreesAPI <-  as.data.frame(do.call(cbind, ThreesAPI))
ThreesChar = as.data.frame(ThreesAPI$entries.character)
ThreesMatchStats = as.data.frame(ThreesAPI$entries.season_match_statistics)
ThreesCleaned = data.frame(CharName = unlist(ThreesChar$name),
                         CharID = unlist(ThreesChar$id),
                         RealmID = unlist(ThreesChar$realm$id),
                         RealmName = unlist(ThreesChar$realm$slug),
                         Faction = unlist(ThreesAPI$entries.faction$type),
                         Rank = unlist(ThreesAPI$entries.rank),
                         ThreesRating = unlist(ThreesAPI$entries.rating),
                         ThreesPlayed = unlist(ThreesMatchStats$played),
                         ThreesWon = unlist(ThreesMatchStats$won),
                         ThreesLost = unlist(ThreesMatchStats$lost)
)

ThreesClassSpec = merge(x=ThreesCleaned,y=ShuffleStats, by="CharID")
ThreesStats = ThreesClassSpec %>% 
              group_by(name) %>% 
              dplyr::summarise(
                AvgWinRate = mean(WinRate),
                MedianWinRate = median(WinRate),
                CountOfPlayers = n(),
                NumberOfMatchesPlayed = sum(entries.season_match_statistics$played),
                AvgRating = mean(entries.rating),
                MinRating = min(entries.rating),
                MaxRating = max(entries.rating)
              )

#2v2
TwosAPI = blizz("/data/wow/pvp-season/34/pvp-leaderboard/2v2", "en_US", "dynamic-us", json = FALSE)
TwosAPI <-  as.data.frame(do.call(cbind, TwosAPI))
TwosChar = as.data.frame(TwosAPI$entries.character)
TwosMatchStats = as.data.frame(TwosAPI$entries.season_match_statistics)
TwosCleaned = data.frame(CharName = unlist(TwosChar$name),
                  CharID = unlist(TwosChar$id),
                  RealmID = unlist(TwosChar$realm$id),
                  RealmName = unlist(TwosChar$realm$slug),
                  Faction = unlist(TwosAPI$entries.faction),
                  Rank = unlist(TwosAPI$entries.rank),
                  TwosRating = unlist(TwosAPI$entries.rating),
                  TwosPlayed = unlist(TwosMatchStats$played),
                  TwosWon = unlist(TwosMatchStats$won),
                  TwosLost = unlist(TwosMatchStats$lost)
                  )

TwosClassSpec = merge(x=TwosCleaned,y=ShuffleStats, by="CharID")

TwosStats = TwosClassSpec %>% 
  group_by(name) %>% 
  dplyr::summarise(
    AvgWinRate = mean(WinRate),
    MedianWinRate = median(WinRate),
    CountOfPlayers = n(),
    NumberOfMatchesPlayed = sum(entries.season_match_statistics$played),
    AvgRating = mean(entries.rating),
    MinRating = min(entries.rating),
    MaxRating = max(entries.rating)
  )

#Write to db/excel
copy_to(my_db, WinRateSpec, name = paste0("SoloShuffle",Sys.Date()))
copy_to(my_db, ThreesStats, name = paste0("Threes",Sys.Date()))
copy_to(my_db, TwosStats, name = paste0("Twos",Sys.Date()))

write_xlsx(as.data.frame(WinRateSpec), path = paste0("SoloShuffle",Sys.Date(),".xlsx"))
write_xlsx(as.data.frame(ThreesClassSpec), path = paste0("ThreesRowLevel",Sys.Date(),".xlsx"))
write_xlsx(as.data.frame(ThreesStats), path = paste0("ThreesStats",Sys.Date(),".xlsx"))
write_xlsx(as.data.frame(TwosClassSpec), path = paste0("TwosRowLevel",Sys.Date(),".xlsx"))
write_xlsx(as.data.frame(TwosStats), path = paste0("TwosStats",Sys.Date(),".xlsx"))

