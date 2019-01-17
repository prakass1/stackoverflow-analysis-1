# util.R
# contains utility functions used by other files


#15 selected tags
selected_tags <- c("android",
                   "c",
                   "c#",
                   "c++",
                   "html",
                   "css",
                   "ios",
                   "java",
                   "javascript",
                   "jquery",
                   "mysql",
                   "php",
                   "python",
                   "r",
                   "sql")



# input: @filepath: the path with extension of the xml to read, @query: xpath query to select nodes
# returns a dataframw with the parsed xml contents
ReadXMLToDf <- function(filepath, query){
    #load the xml doc
    doc <- xmlParse(filepath)
    #return the top node of the xml file
    rootnode <- xmlRoot(doc)
    #return the set of xml nodes
    nodeset <- getNodeSet(rootnode, query)
    return(XML:::xmlAttrsToDataFrame(nodeset))
}


# writes the dataframe to a .csv file
# input: @df: a dataframe, @loc: filepath to store the csv
WriteToCsv <- function(df, loc){
    write_csv(df, loc)
}





# input: @x: character vector of tags
# returns the tags if at least one tag is in the top 15 selected tags, else returns an empty string
filterTags <- function(x){
    for(tag in selected_tags){
        testExpr <- paste0("<",tag,">")
        if(grepl(testExpr, x, fixed = TRUE)){
            return(x)
        }
    }
    return("")
}

# intput: @x: character vector of tags
# checks if the tags are one of the top 15 selected tags
# returns the tag when a top 15 tag is found
filterOneTag <- function(x){
    for(tag in selected_tags){
        testExpr <- paste0("<",tag,">")
        if(grepl(testExpr, x, fixed = TRUE)){
            return(tag)
        }
        
    }
    return("")
}




















# input: @text: a string
# performs contraction relacement, lowercasing, numbers and punctuation removal, 
# stopwords removal, stemming, lemmatization, and white space removal
CleanText <- function(text){
    text <- text %>% ReplaceContractions() %>% 
        ToLowerCase() %>% 
        RemoveNumbersAndPunctuation() %>% 
        RemoveStopwords() %>% 
        #Stem() %>% 
        #Lemmatize() %>% 
        RemoveWhiteSpace()
    return (text)
}



# input: @x: a string
# uses the textclean package to replace contractions
ReplaceContractions <- function(text){
    text <- text %>% replace_contraction(contraction.key = lexicon::key_contractions, 
                                         ignore.case = TRUE)
    return (text)
}


# input: @x: a string
# returns the text with converted to lowercase
ToLowerCase <- function(text){
    text <- tokens(text, what = "word") %>% 
        tokens_tolower(language = quanteda_options("language_stemmer"))
    return (paste(text, collapse = " "))
}

# input: @x: a string
# returns the text with non ascii characters removed
RemoveUnicode <- function(text){
    return(iconv(text, "latin1", "ASCII", sub=""))
}

# input: @x: a string
# returns the text with all punctuation and numbers removed
RemoveNumbersAndPunctuation <- function(text){
    text <- (iconv(text, "latin1", "ASCII", sub=""))
    #text <- gsub("<.+>", "", text)
    return (gsub("([0-9]+|[[:punct:]]+)", " ", text))
}

# input: @x: a string
# returns the text with stopwords removed
RemoveStopwords <- function(text){
    # tokenize the text and use the quanted stopwords list to remove stopwords
    text <- tokens(text, what = "word") %>% 
        tokens_select(stopwords(), selection = "remove")
    return (paste(text, collapse = " "))
}

# input: @x: a string
# lemmatizes the text using the textstem package
Lemmatize <- function(text){
    return (lemmatize_strings(text))
}

# input: @x: a string
# returns the text with all punctuations removed
RemoveWhiteSpace <- function(text){
    return (gsub("\\s+", " ", text))
}

# input: @x: a string
# tokenizes the text and performs stemming on the tokens
Stem <- function(text){
    # tokenize the text and use the quanteda stemmer
    text <- tokens(text, what = "word") %>% 
        tokens_wordstem(language = "english")
    return (paste(text, collapse = " "))
}






# input: @text: a string 
# pulls the nouns and proper nouns from text and returns them as a string
ExtractPOS <- function(text){
    text <- udpipe_annotate(ud_model, text) %>% 
        as_tibble() %>% 
        filter(upos == "NOUN" | upos == "PROPN")
    paste(text$token, collapse=" ")
}






# input: @df: a dataframe
# returns a Document Term Matrix (to return a Term Document Matrix, change the function name to TermDocumentMatrix)
DocumentTermMatrix <- function(df){
    #corpus <- Corpus (VectorSource(df$TidyBody)) 
    corpus <- Corpus (VectorSource(df)) 
    tdmcorpus <- DocumentTermMatrix(corpus)
    return(tdmcorpus)
}









# input: @x: a character vector.
# extracts all links using rvest functions
# returns the links as one character vector separated by commas
GetLinks <- function(x){
    # if a link is found
    if (grepl("http://", x, fixed = TRUE) == TRUE | grepl("https://", x, fixed = TRUE) == TRUE){
        #extract contents of href
        link <- tryCatch({
                read_html(x) %>% 
                html_nodes("a") %>% 
                html_attr("href")
            }, error = function(e){
                ""
            }, finally = {
                ""
            }
        )
    }
    else{
        link = ""
    }
    # return the links as one character vector
    return (paste(link, collapse=","))
}

# input: @x: a character vector.
# extracts all code texts using rvest functions
# returns the code texts as one character vector separated by commas
GetCode <- function(x){
    # if a code tag is found
    if (grepl("<code>", x, fixed = TRUE) == TRUE){
        # extract contents of the code tag
        code <- read_html(x) %>% 
            html_nodes("code") %>% 
            html_text()
    }
    else{
        code = ""
    }
    # return the code texts as one character vector
    return (paste(code, collapse=","))
}


# input: @x: a character vector.
# extracts all contents inside <p> tags using rvest functions
# returns the contents as one character vector separated by periods
GetParagraphContents <- function (x) {
    if (grepl ("<p>", x, fixed = TRUE) == TRUE) {
        x <- read_html (x) %>% 
            html_nodes ("p") %>% 
            html_text ( ) 
        return (paste(x, collapse="."))
    }
    else {
        return (x)
    }
}



# input: a date in character format or date format
# if the date is before 2018, returns false, else returns true
JoinedRecently <- function(date){
    if_else(date >= as.Date("2018-01-01"), 1, 0)
}




# input: @text: a string
# returns the sentiment of the text
GetSentiment <- function(text){
    # divides the text into separate words
    tokens <- data_frame(text = text) %>% 
        unnest_tokens(word, text)
    
    # gets sentiment score of words and sums the scores to get overall sentiment
    sentiment <- tokens %>%
        inner_join(get_sentiments("afinn")) %>% 
        summarise(sentiment = sum(score)) %>% 
        pull(sentiment)
    
    return (sentiment)
}






# input: @comments dataframe, @post_id: id of a post 
# returns the average sentiment of the comments of the post
SentimentOfComments <- function(comments, post_id){
    all_comments <- filter(comments, PostId == post_id)
    avg_sentiment <- all_comments$Sentiment %>% 
        mean()
    print(paste('Avg sentiment of post ', post_id, ': ', avg_sentiment))
    return (avg_sentiment)
}






# returns the location of a user, given a user Id
GetUserLocation <- function(user_id){
    location <- users_with_locations %>% 
        filter(Id == user_id) %>% 
        pull(Location)
    return (location)
}


########## Install packages if not present else load them ##########
# ipak function: install and load multiple R packages.
# source: https://gist.github.com/stevenworthington/3178163
# check to see if packages are installed. Install them if they are not, then load them into the R session.

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)){
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}
