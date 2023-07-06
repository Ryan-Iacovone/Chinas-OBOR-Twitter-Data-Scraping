library(tidyverse)
library(readxl)
library(tidytext)
library(ggthemes)
library(udpipe) #library for lemmatization 
library(cowplot) 

tweets_master <- read_excel("C:/Users/Ryan/Coding Projects/Twitter Data Scraping/master_tweets_2018.xlsx")

twe2019 <- read_excel("C:/Users/Ryan/Coding Projects/Twitter Data Scraping/master_tweets_2019.xlsx")

#starting to combine tweet datasets for later use
combined1819 <- rbind(tweets_master, twe2019)


rm(tweets_master, twe2019)

#changing the format of the ID string to display all numbers 
combined1819$id <- format(as.character(combined1819$id), width = 20, scientific = FALSE)

#Changing the date variable to a date format because I don't care about the time of tweet's time of day 
combined1819$date <- as.Date(combined1819$date, format = '%b %d, %Y')

#grabbing the month and year (12-18 for december 2018) of the tweet tweeted for later analysis to group by month and year 
combined1819$month <- format(combined1819$date, "%m-%y") 

#grabbing just the year of the tweet tweeted for later analysis to group by year 
combined1819$year <- format(combined1819$date, "%Y") 

#Grabbing only the tweets that the python program recognized as english
tweets_EN <- combined1819 %>% filter(language == "en") 


#Note that the unnest_tokens function tries to convert tokens into words and strips characters important to twitter such as # and @. A token in twitter is not the same as in regular English. For this reason, instead of using the default token, words, we define a regex that captures twitter character. The pattern appears complex but all we are defining is a patter that starts with @, # or neither and is followed by any combination of letters or digits:

pattern <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"

#Some exploration of the resulting words (not show here) reveals a couple of unwanted characteristics in our tokens. First, some of our tokens are just numbers (years for example). We want to remove these and we can find them using the regex ^\d+$. Second, some of our tokens come from a quote and they start with '. We want to remove the ' when it's at the start of a word, so we will use str_replace(). We add these two lines to the code above to generate our final table:
#We use only our English tweets for creating the tweet_words_china dataframe

tweet_words_china <- tweets_EN %>% 
  mutate(content = str_replace_all(content, "https://t.co/[A-Za-z\\d]+|&amp;", ""))  %>%
  unnest_tokens(word, content, token = "regex", pattern = pattern) %>%
  filter(!word %in% stop_words$word &
           !str_detect(word, "^\\d+$")) %>%
  mutate(word = str_replace(word, "^'", ""))


file_term <- "lemmatization"
directory <- "C:/Users/Ryan/Coding Projects/Twitter Data Scraping"

# Loop through all files in the directory to search for an older version of this document (file_name)
# If it exists, load the file
for (filename in list.files(directory)) {
  
  #Searches through all the filenames trying to match our file_term to the filename and once it does we then load that file path  
  if (grepl(file_term, filename)) {
    
    #Combines the directory and name of file we've matched to create a file path so we can load in the RDA file 
    file_pathy <- file.path(directory, filename)
    
    #loads the lemitization dataframe we created previously 
    load(file_pathy)
    
    #exits the loop once we've found the file with "lemmatization" in it 
    break
  }  
}  

#creates a new character vector based on 2 variables that contain words before and after they've gone through lemmitization   
word <- lemmatization_core$sentence #the original vector of characters as they appeared in the tweets 
lemma <- lemmatization_core$lemma #The lemmitized version of each word now ready for sentiment analysis  

#Create a dictionary as the combined inputs of the two vectors above "word" and "lemma" so their values are now connected via the key of the original word
lemma_dict <- setNames(lemma, word)

#Code to search through the lemma_dict for a specific word to be lemmitized  
word_to_lookup <- "increased"

ifelse(word_to_lookup %in% names(lemma_dict), lemma_dict[[word_to_lookup]], NA)


#Going through the list of our original words then using the match function to connect them with their lemmitized version 

# create a vector of original words to lemmatize, ONLY RUN THIS CODE ONCE OTHERWISE IT CREATES MORE VARIABLES!!!!!!!
orig_words <- tweet_words_china$word

# use the match function to replace the original words with their lemmas
lemmatized_words <- lemma_dict[match(orig_words, names(lemma_dict))]

# replace any missing lemmas with the original words, idk if I want to do this
#lemmatized_words[is.na(lemmatized_words)] <- orig_words[is.na(lemmatized_words)]

# add the lemmatized words to the dataframe, ONLY RUN THIS CODE ONCE OTHERWISE IT CREATES MORE VARIABLES!!!!!!!
tweet_words_china$word_lems <- lemmatized_words

# rename the columns
names(tweet_words_china)[names(tweet_words_china) == "word"] <- "orig_word"
names(tweet_words_china)[names(tweet_words_china) == "word_lems"] <- "word"