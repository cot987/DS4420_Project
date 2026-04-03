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

# build design matrix

# sample subset 
set.seed(1)
df_sample <- df_recent %>% sample_n(200000)

# X matrix (bias + lag)
X <- cbind(1, df_sample$SHOT_LAG1)

# y vector
y <- df_sample$SHOT_MADE_BINARY

n <- nrow(X)
p <- ncol(X)

# logistic function

sigmoid <- function(z) {
  1 / (1 + exp(-z))
}

# approximate mle

# use simple optimization

log_likelihood <- function(w) {
  p <- sigmoid(X %*% w)
  sum(y * log(p + 1e-8) + (1 - y) * log(1 - p + 1e-8))
}

# optimize
opt <- optim(rep(0, p), fn = function(w) -log_likelihood(w))

w_hat <- opt$par
w_hat

# Bayesian sampling (normal)

# vector of weights (NO matrix)
p_hat <- sigmoid(X %*% w_hat)
w_vec <- as.vector(p_hat * (1 - p_hat))

# efficient computation 
WX <- X * w_vec 
Sigma_hat <- solve(t(X) %*% WX)

# posterior draws
n_samples <- 5000

w_tilde <- rmvn(n_samples, mu = w_hat, sigma = Sigma_hat)

# posterior analysis

# posterior summaries
colMeans(w_tilde)

# histograms
hist(w_tilde[,1], main = "Posterior of Intercept")
hist(w_tilde[,2], main = "Posterior of Lag Effect (Hot Hand)")











