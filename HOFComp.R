library(tidyverse)
library(nflfastR)
library(nflreadr)

pbp <- load_pbp(seasons=1999:2023)

pbp %>%
  select(receiver_player_name, season) %>% 
  mutate(receiver_player_name_cleaned = str_replace(receiver_player_name, "(\\w)\\.(\\s*)", "\\1")) %>%
  mutate(receiver_player_name_cleaned = trimws(receiver_player_name_cleaned)) %>%
  group_by(receiver_player_name_cleaned) %>%
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  View()

pbp %>%
  mutate(TE_Name = str_replace(receiver_player_name, "(\\w)\\.(\\s*)", "\\1")) %>%
  mutate(TE_Name = trimws(TE_Name)) %>%
  group_by(TE_Name) %>%
  mutate(row_id = row_number()) %>%
  ungroup() %>%
  group_by(season, TE_Name) %>%
  filter(TE_Name=='AGates' | TE_Name=='TGonzalez'
         | TE_Name=='TKelce' | TE_Name=='RGronkowski') %>%
  summarise(mean_epa = mean(epa)) %>%
  ggplot(aes(x=season, y = mean_epa, color = TE_Name)) + geom_point() + geom_smooth(method = "lm", se = FALSE) +ggtitle("Mean EPA by Season for First Ballot Caliber TEs") + labs(y= "Mean EPA", x= "Season", color = "TE Name")



  