# 02_preprocessing.R
# data cleaning and preprocessing

library(dplyr)

############ Preprocess Posts ############

# 1. separate question posts and answer posts into different dataframes
# 2. keep only the relevant columns in questions and answers
# 3. remove the time part of the datetime fields in questions and answers
# 4. convert the Nas in questions to zeros
# 5. select the questions having the top 15 selected tags
# 6. extract links and code from posts and move them to separate columns
# 7. add new columns HasCode and HasLinks


# posts contain questions which are PostTypeId 1 and answers are PostTypeId 2,
# so they can be separated into different dataframes
questions <- posts %>% 
    filter(PostTypeId == 1)
answers <- posts %>% 
    filter(PostTypeId == 2)

# remove the posts dataframe to save memory
rm(posts)


# some columns aren't relevant, so only the useful columns need to be kept 
# and the others can be removed
questions_cols <- c("Id", 
                    "PostTypeId", 
                    "CreationDate", 
                    "Score",
                    "ViewCount",
                    "Body",
                    "OwnerUserId",
                    "LastActivityDate",
                    "Title",
                    "Tags",
                    "AnswerCount",
                    "CommentCount",
                    "FavoriteCount")

answers_cols <- c("Id",
                  "CreationDate",
                  "Score",
                  "Body",
                  "OwnerUserId",
                  "CommentCount",
                  "ParentId")

# keep only selective columns
questions <- questions %>% 
    dplyr::select(questions_cols)
answers <- answers %>% 
    dplyr::select(answers_cols)

# remove the time part of the datetime columns as time isn't meaningful 
questions <- questions %>%
    transform(CreationDate = as.Date(CreationDate, tryFormats = c("%Y%m%d"))) %>%
    transform(LastActivityDate = as.Date(LastActivityDate, tryFormats = c("%Y%m%d")))
# same as above
answers <- answers %>%
    transform(CreationDate = as.Date(CreationDate, tryFormats = c("%Y%m%d")))


# make all NA cols to 0 for not having any NULLS
questions[is.na(questions)] <- 0



# apply the filterOneTag function on the values in the Tags column. 
# This will replace the Tags column only tags that are in the top 15 selected tags
# and the rest will be replaced with empty strings
questions["Tags"] <- apply(questions["Tags"], 1, filterOneTag)
# remove questions that have empty strings in the Tags columns
questions <- questions %>% 
    filter(Tags != "")


# extract links and code from questions and answers body and place them in new columns 
# then extract the <p> tags from the Body column and replace the column with the 
# contents inside the <p> tags. This removes the links and code, and keeps only the text
questions <- questions %>% 
    rowwise() %>% 
    mutate(Links = GetLinks(Body), Code = GetCode(Body)) %>% 
    mutate(Body = GetParagraphContents(Body)) %>% 
    mutate(HasLinks = if_else(Links != "", 1, 0), HasCode = if_else(Code != "", 1, 0)) %>% 
    ungroup()
# same as above
answers <- answers %>% 
    rowwise() %>% 
    mutate(Links = GetLinks(Body), Code = GetCode(Body)) %>% 
    mutate(Body = GetParagraphContents(Body)) %>% 
    mutate(HasLinks = if_else(Links != "", 1, 0), HasCode = if_else(Code != "", 1, 0)) %>% 
    ungroup()


#############################################











############ Preprocess Comments ############

# 1. filter out the comments that are not present in the questions and the answers dataframe
# 2. keep only the relevant columns in comments
# 3. remove the time part of the CreationDate column

# select all the post ids and filter the comments dataframe to contain 
# entries that are also in the questions and answers dataframes
questions_and_answers <- c(pull(questions, Id), pull(answers, Id))
comments <- comments %>% 
    filter(PostId %in% questions_and_answers)

comments_cols <- c("Id",
                   "PostId",
                   "Score",
                   "Text",
                   "CreationDate",
                   "UserId")

# keep only selected columns
comments <- comments %>% 
    dplyr::select(comments_cols)


# remove the time part of the CreationDate column as time is not meaningful
comments <- comments %>% 
    mutate(CreationDate = as.Date(CreationDate, tryformats=c("Y%m%d")))




#############################################







############ Preprocess Users ############

# 1. remove the users who haven't recently posted a question, answer, or comment
# 2. add a new column JoinedRecently, and also a Score column (upvotes - downvotes)
# 3. get links and code from users AboutMe and put them in separate columns


users_cols <- c("Id",
                "Reputation",
                "CreationDate",
                "DisplayName",
                "LastAccessDate",
                "WebsiteUrl",
                "Location",
                "AboutMe",
                "Views",
                "UpVotes",
                "DownVotes",
                "AccountId")
                    

# keep only selective columns
users <- users %>% 
    dplyr::select(users_cols)


# filter the users that are in posts and comments (recently active)
user_ids <- append(questions$OwnerUserId, answers$OwnerUserId) %>% 
    append(comments$UserId) %>% 
    unique()

# keep only the recently active users
users <- users %>% 
    filter(AccountId %in% user_ids)


# keep only the date part in the LastAccessDate and CreationDate columns
# then create a new column called Score which is just upvotes minus downvotes
# the last select removes the UpVotes and DownVotes columns
users <- users %>% 
    mutate(LastAccessDate = as.Date(LastAccessDate, tryformats=c("Y%m%d"))) %>% 
    mutate(CreationDate = as.Date(CreationDate, tryformats=c("Y%m%d"))) %>% 
    mutate(Score = UpVotes - DownVotes) %>% 
    mutate(JoinedRecently = JoinedRecently(CreationDate)) %>% 
    dplyr::select(-c(UpVotes, DownVotes))
    


# extract Links and Code from the AboutMe column (after converting to lower case) 
# and put them in separate columns
# remove the <p> tags and Links and Code from AboutMe
# create a new column called JoinedRecently, which stores 1 if the user joined in 2018,
# 0 if they joined earlier
users <- users %>% 
    rowwise() %>% 
    mutate(AboutMe = tolower(AboutMe)) %>% 
    mutate(Links = GetLinks(AboutMe), Code = GetCode(AboutMe)) %>% 
    mutate(AboutMe = GetParagraphContents(AboutMe)) %>% 
    mutate(HasLinks = if_else(Links != "", 1, 0), HasCode = if_else(Code != "", 1, 0)) %>% 
    ungroup()

# make all NA cols to '' so there are no NULLS
users[is.na(users)] <- ""

# clean users AboutMe
users <- users %>%
    rowwise() %>% 
    mutate(TidyAboutMe = CleanText(AboutMe)) %>% 
    ungroup

#############################################
