library(tidyverse)

# set working directory
path_loc <- "C:/repo/IntObl/ZadaniaAsocjacjaGrupowanie06"
setwd(path_loc)
# reading in the data
titanic <- read_csv("test.csv") ; titanic

# taking a quick look
glimpse(titanic)

titanic$Pclass <- as.numeric(titanic$Pclass)
titanic$SibSp <- as.numeric(titanic$SibSp)
titanic$Age <- as.numeric(titanic$Age)

library(arules)
# find association rules with default settings
rules <- apriori(titanic)
inspect(rules)


