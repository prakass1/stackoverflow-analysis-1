# 03_preprocessing.R
# more data cleaning and preprocessing


############ Preprocess Questions ############

# 1. clean question title and body
# 2. extract nouns from title and body and put them in searate columns
# 3. create training data by removing all columns except Id, Text (nouns), and Tags (label)



# perform preprocessing (lowercase, stopwords, punctuation, etc) on the title and body
questions <- questions %>% 
    rowwise() %>% 
    mutate(Title = CleanText(Title)) %>% 
    mutate(Body = CleanText(Body)) %>% 
    ungroup() 

# extract nouns from the Title and Body
ud_model <- udpipe_download_model(language = "english")
ud_model <- udpipe_load_model(ud_model$file_model)
questions <- questions %>% 
    rowwise() %>% 
    mutate(NounsTitle = ExtractPOS(Title), 
           NounsBody = ExtractPOS(Body)) %>% 
    ungroup()

# convert to lower case
questions <- questions %>% 
    rowwise() %>% 
    mutate(NounsTitle = ToLowerCase(NounsTitle), 
           NounsBody = ToLowerCase(NounsBody)) %>% 
    ungroup()

# remove numbers and punctuation
questions <- questions %>% 
    rowwise() %>% 
    mutate(NounsTitle = RemoveNumbersAndPunctuation(NounsTitle), 
           NounsBody = RemoveNumbersAndPunctuation(NounsBody)) %>% 
    ungroup()


# remove extra whitespace
questions <- questions %>% 
    rowwise() %>% 
    mutate(NounsTitle = RemoveWhiteSpace(NounsTitle), 
           NounsBody = RemoveWhiteSpace(NounsBody)) %>% 
    ungroup()



# create the training data by removing all unnecessary columns and giving more weight 
# to the Title by appending it thrice
train_questions <- questions %>% 
    rowwise() %>% 
    mutate(Text = paste(c(NounsTitle, NounsTitle, NounsTitle, NounsBody), collapse=" ")) %>% 
    ungroup() %>% 
    dplyr::select(Id, Text, Tags)
train_questions$Tags <- as.factor(train_questions$Tags)


# # create a document frequency matrix using Tags as documents
# dfm <- tokens(train_questions$Text, what = "word") %>% 
#     dfm(groups = train_questions$Tags)
# # view the most occuring features
# topfeatures(dfm, 50, decreasing = TRUE)
# remove = c('use', 'can', 'get', 'code', 'work', 'file', 'error', 'datum', 'value', 'like', 'want', 'function')
# 
# # remove custom stopwords from the training data
# train_questions <- train_questions %>% 
#     rowwise() %>% 
#     mutate(Text = {gsub(paste(remove, collapse = '|'), '', Text)}) %>% 
#     ungroup()



