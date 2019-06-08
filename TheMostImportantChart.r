source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")
library(tidyr)
library(dplyr)
library(tibble)
library(stringr)
library(ggplot2)
library(forcats)

options(scipen = 1e9)

data <- full_data %>%
  # Change index_real to index to get the nominal returns
  select(dates, index_real) %>%
  na.omit()

# Calculate index for each year
index <- seq(1:nrow(data))

# Make a tibble for containing the years and returns
leaded <- as_tibble(matrix(NA,
                           ncol = length(index),
                           nrow = nrow(data)))
# Rename columns
colnames(leaded) <- paste0("m", 1:length(index))

# Loop future i month returns
for (i in index) {
  leaded[, i] <- lead(data$index, i) / data$index
}

# Calculate prediction intervals
quantiles <- cbind(sapply(leaded %>% select(m1:m600),
                          function(x) quantile(x, na.rm = T, 0.05)),
                   sapply(leaded %>% select(m1:m600),
                          function(x) quantile(x, na.rm = T, 0.95)))

# Format data
quantiles <- quantiles %>%
  as.data.frame() %>%
  rownames_to_column(var = "key") %>%
  rename(min = V1,
         max = V2) %>%
  mutate(key = str_remove(key, ".5%"))

# Gather & attach prediction intervals
to_plot <- leaded %>%
  select(m1:m600) %>%
  gather() %>%
  left_join(quantiles)

# Format years, rename and plot
to_plot %>%
  mutate(key = as.numeric(str_remove(key, "m"))) %>%
  rename(Year = key,
         Multiple = value) %>%
  mutate(Year = as.numeric(fct_reorder(as.character(Year), sort(Year))) / 12) %>%
  ggplot(aes(x = Year,
             y = Multiple)) +
  stat_smooth(color = "black") +
  geom_point(color = "#00BFC4", alpha = 0.01) +
  scale_y_continuous(breaks = c(0.5, 1, 2, 4, 8, 16, 32, 64, 128),
                     trans = "log") +
  ggtitle("Historical inflation-adjusted S&P 500 returns") +
  xlab("Years invested") +
  geom_line(aes(y = min), color = "black") +
  geom_line(aes(y = max), color = "black") +
  theme(panel.grid.major = element_line(color = "#969696", size = 0.25))
