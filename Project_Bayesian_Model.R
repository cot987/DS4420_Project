# ===============================
# BAYESIAN HOT HAND MODEL
# ===============================

# libraries
library(dplyr)
library(mvnfast)

# load and combine data

folder_path <- "NBA Shot Data"

files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

df_list <- lapply(files, read.csv)
df <- bind_rows(df_list)

cat("Total rows:", nrow(df), "\n")

# clean and prep data

# convert shot outcome
df$SHOT_MADE_BINARY <- ifelse(df$SHOT_MADE %in% c(TRUE, 1, "TRUE"), 1, 0)

# fix date format
df$GAME_DATE <- as.Date(df$GAME_DATE, format = "%m-%d-%Y")

# filter last ~15 years
df_recent <- df %>%
  filter(SEASON_2 >= "2009-10")

# create lag variable
df_recent <- df_recent %>%
  arrange(PLAYER_NAME, GAME_DATE, GAME_ID) %>%
  group_by(PLAYER_NAME) %>%
  mutate(SHOT_LAG1 = lag(SHOT_MADE_BINARY)) %>%
  ungroup()

df_recent <- df_recent %>% filter(!is.na(SHOT_LAG1))