# Read the Shakespeare text file and perform basic data cleaning.
# file obtained from Kaggle:
# https://www.kaggle.com/kingburrito666/shakespeare-plays

library(dplyr)
library(stringr)
library(ggplot2)

# Set directory and environment vaiables

# HOME        <- "/data/anet11.2/tatf/work/uca78"
HOME        <- "c:/R"
PROJECT_DIR <- paste0(HOME,"/Shakespeare")
DATA_DIR    <- paste0(PROJECT_DIR,"/data")

# Read the csv file

works <- read.csv(paste0(DATA_DIR,"/Shakespeare_data.csv"),stringsAsFactors = FALSE)
# 111396 lines 

# Data Cleaning

works <- works %>% filter(ActSceneLine != "")  # Remove records which are not lines
# 105153 lines

works$Player <- tools::toTitleCase(tolower(works$Player))  # Player name in title case (initial capital)
works$Play   <- tools::toTitleCase(works$Play)    # Play name in title case (initial capital)

# Set play type

comedies <- c('A Comedy of Errors',
              'A Midsummer nights dream',
              'A Winters Tale',
              'Alls well that ends well',
              'As you like it',
              'Cymbeline',
              'Loves Labours Lost',
              'Measure for measure',
              'Merchant of Venice',
              'Merry Wives of Windsor',
              'Much Ado about nothing',
              'Pericles',
              'Taming of the Shrew',
              'The Tempest',
              'Twelfth Night',
              'Two Gentlemen of Verona')

histories <- c('Henry IV',
               'Henry V',
               'Henry VI Part 1',
               'Henry VI Part 2',
               'Henry VI Part 3',
               'Henry VIII',
               'King John',
               'Richard II',
               'Richard III')

tragedies <- c('Antony and Cleopatra',
               'Coriolanus',
               'Hamlet',
               'Julius Caesar',
               'King Lear',
               'macbeth',
               'Othello',
               'Romeo and Juliet',
               'Timon of Athens',
               'Titus Andronicus',
               'Troilus and Cressida')

works$Type[works$Play %in% tools::toTitleCase(comedies)]  <- 'Comedy'
works$Type[works$Play %in% tools::toTitleCase(tragedies)] <- 'Tragedy'
works$Type[works$Play %in% tools::toTitleCase(histories)] <- 'History'

works$Type <- as.factor(works$Type)

# Split out Act, scene and line number

works$Act   <- as.numeric(word(works$ActSceneLine,1,sep=fixed('.')))
works$Scene <- as.numeric(word(works$ActSceneLine,2,sep=fixed('.')))
works$Line  <- as.numeric(word(works$ActSceneLine,3,sep=fixed('.')))

# Add number of lines and number of words

works$lines <- 1
works$words <- str_count(works$PlayerLine, '\\w+')

summary(works)
table(works$Play)   # list of plays
table(works$Player) # list of characters

type_count   <- aggregate(list(lines =works$lines,words=works$words),by=list(Type=works$Type),sum)
play_count   <- aggregate(list(lines =works$lines,words=works$words),by=list(Type=works$Type,Play=works$Play),sum)
player_count <- aggregate(list(lines =works$lines,words=works$words),
                          by=list(Type=works$Type,Play=works$Play,Player=works$Player),sum)

# Biggest parts
head(player_count[order(player_count$words,decreasing = T),],20)

# Bar Charts
ggplot(type_count, aes(x = Type, y=words,fill=factor(Type)) ) + 
  geom_bar(width = 1,stat="identity")

ggplot(play_count, aes(x = Play, y=words,fill=factor(Type)) ) + 
  geom_bar(width = 1,stat="identity") +
  coord_flip()

save(works,file=paste0(DATA_DIR,"/works.RData"))
