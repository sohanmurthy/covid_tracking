library(tidyverse)
source("utils/analytics.R")
source("utils/getPostgresData.R")

#test chart theme
mtcars %>%
  ggplot(aes(x = mpg, y = disp, color = as.factor(cyl))) +
  geom_point()


#get data from postgres server (obviously, replace with a real query)
test.df <- getPostgresData("select * from table limit 100;")

#or read a SQL script from the "queries" folder and do the same
test.query <- read_file("queries/test.sql")
test.df <- getPostgresData(test.query)