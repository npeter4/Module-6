library(rpart)
library(rpart.plot)
library(tidyverse)
library(forecast)
rm(list=ls())
library(readr)
Basketball <- read_delim("2022-2023 NBA Player Stats - Playoffs (1).csv", delim = ";")
Basketball <- Basketball %>%select( -Age, -Tm,-Pos, -G, -GS, -FG, -FGA,-`3P`, -`3P%`, -`2P`, -`2PA`, -`2P%`, -`eFG%`,-FTA, -`FT%`, -ORB, -DRB, -STL, -BLK, -TOV, -PF)
set.seed(1)
Train <- Basketball %>% sample_frac(0.8) 
Testing <- Basketball %>% anti_join(Train, by="Rk")
Tree <- rpart(PTS ~. -Rk-Player, data=Train, method="anova")
rpart.plot(Tree)
Train.Predictions <- predict(Tree)
Testing$Player <- factor(Testing$Player, levels = levels(Train$Player))
Testing.Predictions <-predict(Tree,newdata=Testing)
Testing$residual <- abs(Testing.Predictions - Testing$PTS)
Testing$predicted <- Testing.Predictions

worst_cases <- Testing %>%
  arrange(desc(residual)) %>%
  select(PTS, predicted, residual) %>%
  head(5)

print(worst_cases)
