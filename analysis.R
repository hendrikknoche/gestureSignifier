library(xlsx)
library(tidyverse)
source("functions.R")

df <- read.csv("data/Test2of2.csv")
dfDetails <- df %>%
  separate_rows(GestureTriedB4, convert = TRUE) %>%
  drop_na()

dfDetails <- df %>%
  mutate(col2 = strsplit(as.character(GestureTriedB4), ",")) %>%
  unnest(col2)
dfDetails$col2 <- as.factor(camel(trimws(dfDetails$col2)))
dfDetails <- dfDetails %>%
  group_by(participantID, taskOrder) %>%
  mutate(numOfGestures = 1:n())

summary(aov(log(taskTimeCom) ~ sigPlace * gesture, df))
qqnorm(log(df$taskTimeCom))
qqline(log(df$taskTimeCom))

forPlotting <- df %>%
  group_by(sigPlace, gesture) %>%
  summarise(
    smean = mean(taskTimeCom, na.rm = TRUE),
    ssd = sd(taskTimeCom, na.rm = TRUE),
    count = n()
  ) %>%
  mutate(
    se = ssd / sqrt(count),
    lower_ci = lower_ci(smean, se, count),
    upper_ci = upper_ci(smean, se, count)
  )

ggplot(forPlotting, aes(x = factor(sigPlace), y = smean, group = gesture, colour = gesture)) +
  geom_line() +
  geom_point() +
  # geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci)) +
  theme_bw()
