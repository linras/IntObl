library(tidyverse)
# set working directory
path_loc <- "C:/repo/IntObl/ZadaniaIrysy03"
setwd(path_loc)
# reading in the data
df <- read_csv("iris_with_errors.csv") ; df
str(df)

# taking a quick look
glimpse(df)

# retype to numeric
df$sepal.length <- as.numeric(df$sepal.length)
df$sepal.width <- as.numeric(df$sepal.width)
df$petal.width <- as.numeric(df$petal.width)
df$petal.length <- as.numeric(df$petal.length)


# 1.a) show which numeric records have no values
glimpse(df %>%
  filter(is.na(df$petal.width) | is.na(df$petal.length) | is.na(df$sepal.length) | is.na(df$sepal.width)))

#df$sepal.length[[1]]>=df$sepal.length[[2]]

# 1.b) search for incorrect data and set default value

df <- df %>%
  mutate(sepal.length = replace(sepal.length,
                                  is.na(sepal.length) | sepal.length<=0 | sepal.length >= 15,
                                  median(sepal.length, na.rm = TRUE)))

df <- df %>%
  mutate(sepal.width = replace(sepal.width,
                                is.na(sepal.width) | sepal.width<=0 | sepal.width >= 15,
                                median(sepal.width, na.rm = TRUE)))

df <- df %>%
  mutate(petal.length = replace(petal.length,
                               is.na(petal.length) | petal.length<=0 | petal.length >= 15,
                               median(petal.length, na.rm = TRUE)))

df <- df %>%
  mutate(petal.width = replace(petal.width,
                                is.na(petal.width) | petal.width<=0 | petal.width >= 15,
                                median(petal.width, na.rm = TRUE)))

# 1.c)
#show which variety has wrong format
glimpse(df %>%
  filter(variety != 'Virginica' & variety != 'Setosa' & variety != 'Versicolor'))

firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  substr(x, 2, 2) <- toupper(substr(x, 2, 2))
  x
}
# fix wrong strings
df <- df %>%
  mutate(variety = replace(variety,
                           variety == 'Versicolour' | variety == 'versicolor',
                           'Versicolor'))

df <- df %>%
  mutate(variety = replace(variety,
                           variety == 'virginica',
                           'Virginica'))

df <- df %>%
  mutate(variety = replace(variety,
                           variety == 'setosa',
                           'Setosa'))

