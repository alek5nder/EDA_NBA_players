nba_players$height_category <- cut(nba_players$player_height,
                                   breaks = c(-Inf, 180, 190, 200, 210, Inf),
                                   labels = c("<180", "180-190", "190-200", "200-210", ">210"))

average_stats <- aggregate(cbind(pts, reb, ast) ~ height_category, 
                           data = nba_players, 
                           FUN = mean, na.rm = TRUE)

average_stats_long <- average_stats %>%
  pivot_longer(cols = c("pts", "reb", "ast"), names_to = "stat", values_to = "value") %>%
  mutate(stat = factor(stat, levels = c("pts", "reb", "ast")),
         stat_order = case_when(
           stat == "pts" ~ 1,
           height_category %in% c("<180", "180-190") & stat == "reb" ~ 2,
           TRUE ~ 3
         ))

ggplot(average_stats_long, aes(x = height_category, y = value, fill = stat)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = c("pts" = "dodgerblue", "reb" = "goldenrod", "ast" = "forestgreen"),
                    labels = c("pts" = "Punkty", "reb" = "Zbiórki", "ast" = "Asysty")) +
  labs(title = "Średnie statystyki graczy NBA w zależności od wzrostu",
       x = "Kategoria wzrostu",
       y = "Średnia wartość",
       fill = "Statystyka") +
  theme_minimal(base_size = 15) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


nba_players <- nba_players %>%
  mutate(
    decade = case_when(
      season >= 1996 & season <= 2006 ~ "1996-2006",
      season >= 2006 & season <= 2016 ~ "2006-2016",
      season >= 2016 & season <= 2023 ~ "2016-2023"
    )
  )

ggplot(nba_players, aes(x = decade, y = player_height, fill = decade)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(
    title = "Rozkład wzrostu zawodników NBA w dekadach",
    x = "Dekada",
    y = "Wzrost (cm)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = c("1996-2006" = "dodgerblue", "2006-2016" =
                                 "forestgreen", "2016-2023" = "goldenrod"))



nba_players <- nba_players %>%
  mutate(draft_group = case_when(
    draft_number >= 1 & draft_number <= 10 ~ "Top 10",
    draft_number <= 30 ~ "11-30",
    TRUE ~ "31+"
  ))

ggplot(nba_players, aes(x = net_rating, fill = draft_group)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(values = c("Top 10" = "dodgerblue", "11-30" = "forestgreen", "31+" = "goldenrod")) +
  xlim(-50, 50) +
  labs(
    title = "Rozkład Net Rating w zależności od grupy draftu",
    x = "Net Rating",
    y = "Gęstość",
    fill = "Grupa draftu"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 14),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    legend.position = "top"
  )

late_pick_stars <- nba_players %>%
  filter(draft_number > 30,
         oreb_pct > quantile(oreb_pct, 0.75),
         dreb_pct > quantile(dreb_pct, 0.75),
         ast_pct > quantile(ast_pct, 0.75)) %>%
  arrange(desc(ts_pct))
kable(late_pick_stars[, c("player_name", "draft_number", "season","oreb_pct", "dreb_pct", "usg_pct", "ts_pct", "ast_pct")])

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
           cglcol = "grey", cglty = 1,
           caxislabels = seq(0, 30, 5), 
           vlcex = 1.2)

legend("topright", legend = c("Nikola Jokic", "Pozostali zawodnicy"), 
       cex = 1.1, fill = c(rgb(0, 0, 1, 0.3), rgb(0, 1, 0, 0.3)))

