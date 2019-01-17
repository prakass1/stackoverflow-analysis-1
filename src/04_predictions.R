library(caret)
library(dplyr)
library(tidytext)


## Method performs initial operations to process text data apply tf-idf and fetch top 50 terms
# and create a document Term Matrix
perfComputationForLDA <- function(data){
  
  #Unest Tokens by making them to lower, removing numeric and punctuations
  posts_words <- data %>%
    unnest_tokens(to_lower = T, strip_numeric=T,strip_punct=T,word, Title) %>%
    count(Tags, word, sort = TRUE) %>%
    ungroup()
  
  #Total Words
  total_words <- posts_words %>% 
    group_by(Tags) %>% 
    summarize(total = sum(n))
  
  #Apply a Left join to retain posts and total words for the post
  tag_words <- left_join(posts_words, total_words)
  

  
  ###### Applying a custom stopword technique and then apply tfidf #########
  mystopwords = data.frame(word = c("5.7","5.6","2.7","1064","3.6","using",
                                    "i", "me", "my", "myself", "we", 
                                    "our", "ours", "ourselves", "you", "your", "yours", 
                                    "yourself", "yourselves", "he", "him", 
                                    "his", "himself", "she", 
                                    "her", "hers", "herself", "it", "its", "itself", "they", "them", "their", 
                                    "theirs", "themselves", "what", 
                                    "which", "who", "whom", "this", "that", "these",
                                    "those", "am", "is", "are", "was", "were", "be", 
                                    "been", "being", "have", "has", "had", "having",
                                    "do", "does", "did", 
                                    "doing", "a", "an", "the", "and", "but", "if", "or", 
                                    "because", "as", "until", "while", "of", "at", "by", "for", 
                                    "with", "about", "against", "between", "into", 
                                    "through", "during", "before", "after", "above", "below", 
                                    "to", "from", "up", "down", "in", "out", "on",
                                    "off", "over", "under", "again", "further", "then", "once", "here", 
                                    "there", "when", "where", "why", "how", "all", 
                                    "any", "both", "each", "few", "more", 
                                    "most", "other", "some", "such", "no", "nor", "not", "only", "own", "same", 
                                    "so", "than", "too", "very", "s", "t", 
                                    "can", "will", "just", "don", "should", "now"))
  
  #antijoin() to remove stopwords
  tag_words <- anti_join(tag_words, mystopwords, by = "word")
  
  #Apply tfidf
  tag_words <- tag_words %>%
  bind_tf_idf(word,Tags,n)
  
  
  # A filter to not leave out tags themselves
  tag_words_no_idf <- tag_words %>% filter(idf == 0) %>% filter(word == Tags)
  
  #############Cast DTM #############
  title_dtm <- tag_words %>%
    arrange(desc(tf_idf)) %>%
    top_n(150) %>%
   rbind(tag_words_no_idf) %>%
    group_by(Tags) %>%
    cast_dtm(Tags, word, n)

  return(title_dtm)
}


#############Plots ################
library(ggplot2)

#Number of words 'n' occuring in total tags distribution plot #########
ggplot(tag_words, aes(n/total, fill = Tags)) +
  geom_histogram(show.legend = FALSE) +
  xlim(NA, 0.0009) +
  facet_wrap(~Tags, ncol = 2, scales = "free_y")


####Zip's frequency is inversly proportional to rank
freq_by_rank <- tag_words %>% 
  group_by(Tags) %>% 
  mutate(rank = row_number(), 
         `term_frequency` = n/total)


#####Plot Zip's Law showing inverse proportion relationship #########
ggplot(data = freq_by_rank,aes(x=rank,y=term_frequency,color=Tags)) + 
  geom_line(size = 0.4, alpha = 0.8, show.legend = T) + scale_x_log10() + scale_y_log10()


##Select top words of tf_idf ###########
tag_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  group_by(Tags) %>% 
  top_n(15) %>% 
  ungroup %>%
  ggplot(aes(word, tf_idf, fill = Tags)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf top 15 words") +
  facet_wrap(~Tags, ncol = 5, scales = "free") +
  coord_flip()



####### LDA Topic Modelling ###############################

library(topicmodels)
library(stats)
library(stringr)
library(ggplot2)
library(dplyr)

#samples_q <- questions[sample(nrow(questions), 80000), ]
trainNum <- createDataPartition(as.factor(questions$Tags),p = .95,list=F)

trainSample <- questions[trainNum,]
validateSample <- questions[-trainNum,]
intrain <- perfComputationForLDA(trainSample)
intest <- perfComputationForLDA(validateSample)

LDA_Tags <- LDA(intrain, k = 15, control = list(seed = 111),method = "VEM")

### Tags belonging to topic ########
ldaStats <- posterior(LDA_Tags)


### A beta or gamma matrix to get title and topic###
title_topics <- tidy(LDA_Tags, matrix = "beta")

#### LDA Topic_Term Plot ########################
top_terms <- title_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()



############ Make Predictions #####################
test.topics <- posterior(LDA_Tags,intest)

test.topics
(test.topics <- apply(test.topics$topics,1, which.max))

#using tidy with gamma gets document probabilities into topic
#but you only have document, topic and gamma
colnames(trainSample)[10] <- "document"

#colnames(validateSample)[10] <- "document"
source_topic_relationship <- tidy(LDA_Tags, matrix = "gamma") %>%
  #join to orig tidy data by document to get the source
  inner_join(trainSample, by = "document") %>%
  dplyr::select(document, topic, gamma) %>%
  group_by(document, topic) %>%
  #get the avg doc gamma value per source/topic
  mutate(avg = mean(gamma)) %>%
  #remove the gamma value
  dplyr::select(-gamma) %>%
  #take all non duplicates
  distinct()

##View(source_topic_relationship)

#relabel topics to include the word Topic
source_topic_relationship$topic = paste("Topic", source_topic_relationship$topic, sep = " ")

circos.clear() #very important! Reset the circular layout parameters
#assign colors to the outside bars around the circle
grid.col = c("android" = "#FF5733",
             "c" = "#DAF7A6",
             "c#" = "#FFC300",
             "c++" = "#C70039",
             "html" = "#581845",
             "css" = "#33FFE5",
             "ios" = "#A0FF33",
             "java" = "#4486D2",
             "javascript" = "#1B324C",
             "jquery" = "#B695D0",
             "mysql" = "#250D39",
             "php" = "#09A6F7",
             "python" = "#064115",
             "r" = "#FF0909",
             "sql" = "#996B97",
             "T1" ="grey",
             "T2" ="grey",
             "T3" ="grey",
             "T4" ="grey",
             "T5" ="grey",
             "T6" ="grey",
             "T7" ="grey",
             "T8" ="grey",
             "T9" ="grey",
             "T10" ="grey",
             "T11" ="grey",
             "T12" ="grey",
             "T13" ="grey",
             "T14" ="grey",
             "T15" ="grey")


library(tidytext) #text mining, unnesting
library(topicmodels) #the LDA algorithm
library(tidyr)
library(dplyr) 
library(ggplot2)
library(kableExtra) 
library(knitr) 
library(ggrepel)
library(gridExtra)
library(formattable) 
library(tm) 
library(circlize) 
library(plotly)

# set the global parameters for the circular layout. Specifically the gap size (15)
#this also determines that topic goes on top half and source on bottom half

#circos.par(gap.after = c(rep(5, length(unique(source_topic_relationship[[1]])) - 1), 15,
                         #rep(5, length(unique(source_topic_relationship[[2]])) - 1), 15))

#main function that draws the diagram. transparancy goes from 0-1
chordDiagram(source_topic_relationship, transparency = 0.5,
             preAllocateTracks = list(track.height = 0.09), grid.col = grid.col)
title("Relationship Between Topic and Source")
#Clear the circos plot
circos.clear()

####### A simple test for topic assignment ############
#Questions:
# Reading data from CSV into dataframe with multiple delimiters efficiently
# The prediction seems to be good however it is dependent with the titles seen so far.
# A lda model trained on a very large data with diverse topics could yeld even more better result
text = "Django - getting a list of groups of a loggedin user. No data"
pData = data.frame("Title"=c(text),"Tags"=c("doc1"),stringsAsFactors = FALSE)
predee = perfComputationForLDA(pData)
test.topics <- posterior(LDA_Tags,predee)
(test.topics <- apply(test.topics$topics, 1, which.max))
title_topics %>% filter(topic == test.topics[[1]]) %>% filter(beta <= max(beta)) %>% top_n(2)

#################
# Save multiple objects
save(LDA_Tags ,file = "LDA.RData")
save(tag_words,perfComputationForLDA,LDA_Tags,source_topic_relationship, file = "lda_app.RData")

##############################################################################################


#################################################
#
#An attempt to visualize LDA Vis using TSNE, 
# commented as the work is not complete
#
#################################################
# library(tm)
# library(text2vec)
# library(lda)
# titles <- VCorpus(VectorSource(questions$Title))
# 
# 
# titles <- tm_map(titles,removePunctuation)  
# 
# 
# for (j in seq(titles)) {
#   titles[[j]] <- gsub("-", " ", titles[[j]])
#   titles[[j]] <- gsub("\u2028", " ", titles[[j]])
# }
# 
# titles <- tm_map(titles, PlainTextDocument)
# 
# titles <- tm_map(titles, tolower)   
# titles <- tm_map(titles,removeWords,stop_words)
# titles <- tm_map(titles, PlainTextDocument)
# 
# titles <- tm_map(titles, stripWhitespace)
# 
# titles <- tm_map(titles, PlainTextDocument)
# 
# titles_dtm <- DocumentTermMatrix(titles)   
# #inspect(titles_dtm)   
# 
# 
# topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
#   # Required packages
#   library(topicmodels)
#   library(dplyr)
#   library(stringi)
#   library(tm)
#   library(LDAvis)
#   
#   # Find required quantities
#   phi <- posterior(fitted)$terms %>% as.matrix
#   theta <- posterior(fitted)$topics %>% as.matrix
#   vocab <- colnames(phi)
#   doc_length <- vector()
#   for (i in 1:NROW(corpus)) {
#     temp <- paste(corpus$Title[i], collapse = ' ')
#     doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
#   }
#   temp_frequency <- inspect(doc_term)
#   freq_matrix <- data.frame(ST = colnames(doc_term),
#                             Freq = colSums(as.matrix(doc_term)))
#   rm(temp_frequency)
#   
#   # Convert to json
#   json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
#                                  vocab = vocab,
#                                  doc.length = doc_length,
#                                  term.frequency = freq_matrix$Freq)
#   
#   return(json_lda)
# }
# 
# 
# # Find required quantities
# phi <- posterior(title_tags_lda)$terms %>% as.matrix
# theta <- posterior(title_tags_lda)$topics %>% as.matrix
# 
# alpha <- 0.01
# eta <- 0.02
# 
# theta <- t(apply(theta + alpha, 2, function(x) x/sum(x)))
# phi <- t(apply(t(phi) + eta, 2, function(x) x/sum(x)))
# 
# dim(phi)
# dim(theta)
# 
# 
# doc_length <- c(9,7,11,8,12,10,11,14,12,11,10,15,14,14,16)
# 
# vocab <- colnames(phi)
# 
# document <- as.matrix(intrain)
# freq_matrix <- data.frame(ST = colnames(document),
#                           Freq = colSums(document))
# 
# 
# library(tsne)
# svd_tsne <- function(x) tsne(svd(x)$u)
# # Convert to json
# json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
#                                vocab = vocab,
#                                mds.method = svd_tsne,
#                                doc.length = doc_length,
#                                term.frequency = freq_matrix$Freq,R = 5)
# 
# library(LDAvis)
# serVis(json_lda, out.dir = 'vis1', open.browser = FALSE)
