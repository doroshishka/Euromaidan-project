# Euromaidan-project
Code for extracting @-mentions and #tags from tweets

Sys.setlocale("LC_CTYPE", "russian")
options(scipen=999)

getwd()
setwd("directory")
data = read.csv("directory",encoding = "UTF-8", header = TRUE, sep = ",")

####extracting hashtags######better run without any encoding and then use 1251: Cyrrilic(Windows) to correctly open an output##### 
text_corpus <- data.frame(id = as.character(data2$tweet.id), user = as.character(data2$user.handle), 
                          text = as.character(data2$tweet)) #creates new data frame with the data we want

token_full <- lapply(as.character(text_corpus$text), tokenize_words_mod) %>%
  cbind(id = as.character(text_corpus$id)) %>% cbind(user = as.character(text_corpus$user)) %>%
  as.data.frame() #tokenizes the tweets
names(token_full)[1] <- "wordy"

tokenlist <- unnest(token_full, .preserve = c(id, user)) %>% setDT() #unlists each tweet

tokenlist$num <- seq.int(nrow(tokenlist)) #ads a numeric index for each token

wordid <- subset(tokenlist, wordy == "#", select = num) %>%
  mutate(num = num + 1) %>% setDT()

tokenlist[wordid, on = .(num), ':=' (atyes = TRUE)] #tags tokens with #

hashtags.list <- subset(tokenlist, atyes == TRUE, select = c(id, user, wordy)) #subsets hashtags

hashtags.list <- apply(hashtags.list,2,as.character)

write.csv(hashtags.list, 'hashtags.csv')
####encoding for this file is 1251: Cyrilic(Windows) ####
