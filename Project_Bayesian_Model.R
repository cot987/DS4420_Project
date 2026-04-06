# installation/setup
packages <- c("cmdstanr", "brms", "dplyr")
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) install.packages(pkg)
}

library(cmdstanr)
library(brms)
library(dplyr)

Sys.setenv(
  CPATH = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include",
  CPLUS_INCLUDE_PATH = "/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include/c++/v1"
)

# check CmdStan installation
if (is.null(cmdstanr::cmdstan_path())) {
  cmdstanr::install_cmdstan()
}
cmdstanr::set_cmdstan_path(cmdstanr::cmdstan_path())
cat("Using CmdStan at:", cmdstanr::cmdstan_path(), "\n")

options(mc.cores = parallel::detectCores())

# load/prep data

folder_path <- "NBA Shot Data"  # path to your CSV files
files <- list.files(path = folder_path, pattern = "*.csv", full.names = TRUE)

df <- bind_rows(lapply(files, read.csv))
cat("Total rows loaded:", nrow(df), "\n")

# clean variables
df$SHOT_MADE_BINARY <- ifelse(df$SHOT_MADE %in% c(TRUE, 1, "TRUE"), 1, 0)
df$GAME_DATE <- as.Date(df$GAME_DATE, format = "%m-%d-%Y")

# filter recent seasons & create lag variable
df_recent <- df %>%
  filter(SEASON_2 >= "2009-10") %>%
  arrange(PLAYER_NAME, GAME_DATE, GAME_ID) %>%
  group_by(PLAYER_NAME) %>%
  mutate(SHOT_LAG1 = lag(SHOT_MADE_BINARY)) %>%
  ungroup() %>%
  filter(!is.na(SHOT_LAG1))

# sample subset for speed
set.seed(1)
df_sample <- df_recent %>% sample_n(200000)

# train/test-split

set.seed(1)
train_idx <- sample(1:nrow(df_sample), size = 0.8 * nrow(df_sample))

train_data <- df_sample[train_idx, ]
test_data  <- df_sample[-train_idx, ]

# priors

priors <- c(
  prior(normal(0, 5), class = "Intercept"),
  prior(normal(0, 5), class = "b")
)

# fit model

model_brm <- brm(
  formula = SHOT_MADE_BINARY ~ SHOT_LAG1,
  data = train_data,
  family = bernoulli("logit"),
  prior = priors,
  chains = 4,
  iter = 1000,
  warmup = 200,
  cores = parallel::detectCores(),
  backend = "cmdstanr"  # ensures faster compilation and execution
)

# make predictions

# predictions on test set
preds <- posterior_predict(model_brm, newdata = test_data)

# convert posterior predictions to mean probability
pred_probs <- colMeans(preds)

# convert to binary classification
pred_binary <- ifelse(pred_probs >= 0.5, 1, 0)

# accuracy
accuracy <- mean(pred_binary == test_data$SHOT_MADE_BINARY)
cat("Test set accuracy:", accuracy, "\n")

# summary
summary(model_brm)




# exhibits
library(ggplot2)
library(bayesplot)

# posterior distribution of lag coefficient
posterior <- as.data.frame(model_brm)
ggplot(posterior, aes(x = b_SHOT_LAG1)) +
  geom_histogram(bins = 50, fill = "skyblue", color = "black") +
  geom_vline(xintercept = 0, color = "red", linetype = "dashed") +
  labs(
    title = "Posterior Distribution of Lag Coefficient",
    x = "Lag Coefficient (SHOT_LAG1)",
    y = "Frequency"
  )


# histogram of predicted probabilities
ggplot(data.frame(pred_probs), aes(x = pred_probs)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(
    title = "Histogram of Predicted Probabilities",
    x = "Predicted Probability of Making Shot",
    y = "Count"
  )

