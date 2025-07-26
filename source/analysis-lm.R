#parametry r. lewandowskiego:
lewandowski <- data.frame(player_name = "Robert Lewandowski", player_height = 185, player_weight = 79)

#analiza lm:

model_pts <- lm(pts ~ player_height * player_weight, data = nba_players %>% filter(decade == "2016-2023"))
model_reb <- lm(reb ~ player_height * player_weight, data = nba_players %>% filter(decade == "2016-2023"))
model_ast <- lm(ast ~ player_height * player_weight, data = nba_players %>% filter(decade == "2016-2023"))

predicted_pts <- max(predict(model_pts, newdata = lewandowski), 0)
predicted_reb <- max(predict(model_reb, newdata = lewandowski), 0)
predicted_ast <- max(predict(model_ast, newdata = lewandowski), 0)

## sochan vs lewandowski:
lewandowski_stats <- c(predicted_pts, predicted_reb, predicted_ast)

sochan_stats <- nba_players %>%
  filter(player_name == "Jeremy Sochan") %>%
  summarise(pts = mean(pts), reb = mean(reb), ast = mean(ast)) %>%
  unlist()

data <- rbind(
  lewandowski_stats, 
  sochan_stats
)

colnames(data) <- c("Punkty", "Zbórki", "Asysty")

radar_data <- as.data.frame(data)
radar_data <- rbind(
  max = rep(30, 3), 
  min = rep(0, 3), 
  radar_data
)

radarchart(radar_data, 
           axistype = 1, 
           pcol = c("dodgerblue", "goldenrod"), 
           pfcol = c(rgb(0, 0, 1, 0.2), rgb(0, 1, 0, 0.2)),
           cglcol = "grey", cglty = 1,
           caxislabels = seq(0, 30, 5), 
           vlcex = 1.2)

legend("topright", legend = c("Lewandowski", "Jeremy Sochan"), 
       fill = c(rgb(0, 0, 1, 0.3), rgb(0, 1, 0, 0.3)), 
       bty = "n", border = "black")