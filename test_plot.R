# Test Plots
# Test plots from Shakespeare data

library(dplyr)
library(ggplot2)

# HOME        <- "/data/anet11.2/tatf/work/uca78"
HOME        <- "c:/R"
# HOME        <- "h:/R"
PROJECT_DIR <- paste0(HOME,"/Shakespeare")
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

load(file=paste0(DATA_DIR,"/works.RData"))

summary(works)
table(works$Play)      # list of plays
table(works$Character) # list of characters

type_count      <- aggregate(list(lines =works$lines,words=works$words),by=list(Type=works$Type),sum)
play_count      <- aggregate(list(lines =works$lines,words=works$words),by=list(Type=works$Type,Play=works$Play),sum)
character_count <- aggregate(list(lines =works$lines,words=works$words),
                             by=list(Type=works$Type,Play=works$Play,Character=works$Character),sum)

# Biggest parts
head(character_count[order(character_count$words,decreasing = T),],20)

# Bar Charts
ggplot(type_count, aes(x = Type, y=words,fill=factor(Type)) ) + 
  geom_bar(width = 1,stat="identity")

# Order by number of words
play_count$Play <- factor(play_count$Play, levels=play_count$Play[order(play_count$words)])

ggplot(play_count, aes(x = Play, y=words,fill=factor(Type)) ) + 
  geom_bar(width = 1,stat="identity") +
  labs(fill="Play Type", y="Number of Words") +
  coord_flip()

ggplot(play_count, aes(x = Play, y=words) ) + 
  geom_bar(width = 1,stat="identity") +
  labs(fill="Play Type", y="Number of Words") +
  facet_grid(Type~. , scales="free")+
  coord_flip()

