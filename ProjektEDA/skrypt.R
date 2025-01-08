jokic_seasons <- nba_players %>%
  filter(player_name == "Nikola Jokic") %>%
  pull(season)

filtered_data <- nba_players %>%
  filter(season %in% jokic_seasons) %>%
  mutate(group = ifelse(player_name == "Nikola Jokic", "Nikola Jokic", "Pozostali zawodnicy")) %>%
  group_by(group) %>%
  summarise(
    avg_pts = mean(pts),
    avg_reb = mean(reb),
    avg_ast = mean(ast),
    avg_ts_pct = mean(ts_pct)
  ) %>%
  as.data.frame()

radar_data <- rbind(
  max = c(35, 15, 10, 0.7),
  min = c(0, 0, 0, 0),
  filtered_data[, -1]
)

rownames(radar_data) <- c("Max", "Min", filtered_data$group)

radarchart(radar_data,
           axistype = 1,
           pcol = c("dodgerblue", "goldenrod"), 
           pfcol = c(rgb(0, 0, 1, 0.3), rgb(0, 1, 0, 0.3)),
           cglcol = "grey", cglwd = 0.8,
           vlcex = 0.8,
           title = "Nikola Jokic vs Pozostali zawodnicy")

legend("topright", legend = rownames(radar_data)[3:4], 
       fill = c(rgb(1, 0, 0, 0.3), rgb(0, 0, 1, 0.3)))