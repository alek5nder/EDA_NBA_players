## forbes athletes:
athletes_forbes <- 
  read.csv("/top50AthletesForbes.csv",
           sep = ";",
           header = TRUE, 
           stringsAsFactors = FALSE)

athletes_forbes[] <- lapply(athletes_forbes, 
                            function(x) {if (is.character(x)) {
                              iconv(x, from = "latin1", to = "UTF-8", sub = "")
                            } else {x}
                            })

athletes_forbes <- athletes_forbes[, -1]

athletes_forbes <- athletes_forbes %>% select(2, 1, everything())


athletes_forbes_summary <- athletes_forbes %>%
  group_by(SPORT) %>%
  summarise(
    liczba_sportowcow = n()
  )
## nba players:
nba_players <- read.csv("/all_seasons.csv")
nba_players$draft_number[(nba_players$draft_number==0)] <- NA
nba_players$draft_number[(nba_players$draft_number=="Undrafted")] <- NA

nba_players %>%
  summarise_all(~ sum(is.na(.))) %>%
  gather(key = "Kolumna", value = "MissingCount") %>%
  filter(MissingCount > 0)

nba_players$draft_number[is.na(nba_players$draft_number)] <- 0
vis_dat(nba_players)
