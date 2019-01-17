# 04_prediction.R
# build models for tag recommendation



# get most representative words
# group all the words by each tag and get the top relevant words per tag (tf-idf)
top_words <- train_questions %>%
    group_by(Tags) %>%
    unnest_tokens(output = word, input = Text) %>% 
    count(Tags, word) %>%
    bind_tf_idf(term = word, document = Tags, n = n) %>% 
    # sort by descending tf-idf
    arrange(desc(tf_idf)) %>% 
    group_by(Tags) %>%
    slice(1:60)



######### create document term matrix ###########

# remove words from tidy_questions that are not in top_words and 
# create a document term matrix
dtm <- train_questions %>% 
    unnest_tokens(word, Text) %>% 
    filter(word %in% top_words$word) %>% 
    # get count of each token in each document
    count(Id, word) %>% 
    # create a document-term matrix with all features and tf weighting
    cast_dtm(document = Id, term = word, value = n,
             weighting = tm::weightTfIdf)


# get the doc ids and extract their classes to create the class vector y
rows <- dtm$dimnames$Docs
y <- train_questions %>% 
    filter(Id %in% rows) %>% 
    pull(Tags) %>%
    factor()
# cast as a dataframe to feed to classifiers
dtm_tbl <- as_tibble(as.matrix(dtm))



############# classification ###############

# train test split ----->
# get random indices to split train and test sets
set.seed(100)
indices <- createDataPartition(y=y, p=0.9, list=FALSE)  

X_train <- dtm_tbl[indices,]
y_train <- y[indices]
X_test <- dtm_tbl[-indices,]
y_test <- y[-indices]


# SVM ---->
svm <- e1071::svm(X_train, y_train, kernel ="linear", probability = TRUE)
predictions_svm <- predict(svm, X_test, decision.values = TRUE, probability = TRUE)
paste0('accuracy: ', mean(y_test == predictions_svm))

svm_class_table <- table(y_test, predictions_svm)
confusion_matrix_svm <- confusionMatrix(svm_class_table, mode = "everything")
# save the svm model
saveRDS(svm, 'models/svm.rds')
# save the svm predictions confusion matrix
saveRDS(confusion_matrix_svm, 'models/confusion_matrix_svm.rds')








# naive bayes --->

# train test split
set.seed(100)
indices <- sample(1:nrow(dtm_tbl), floor(0.9*nrow(dtm_tbl)))
test_dfm <- as.dfm(dtm_tbl[indices,])
test_y_nb <- y[indices]
dfm <- as.dfm(dtm_tbl)

# train naive bayes model
nb_classifier <- textmodel_nb(dfm, y = y, distribution = "multinomial")
# create test dfm
test_dfm <- dfm_select(test_dfm, pattern = dfm, 
                       selection = "keep")
# make predictions
predictions_nb <- predict(nb_classifier, newdata = test_dfm)

paste0('accuracy: ', mean(test_y_nb == predictions_nb))

nb_class_table <- table(test_y_nb, predictions_nb)
confusion_matrix_nb <- confusionMatrix(nb_class_table, mode = "everything")
# save the naive bayes model
saveRDS(nb_classifier, 'models/nb_classifier.rds')
# save the naive bayes predictions confusion matrix
saveRDS(confusion_matrix_nb, 'models/confusion_matrix_nb.rds')

          

# nearest centroid ---->

nm_classifier <- klaR::nm(x=X_train, grouping=y_train)
# predict
predictions_nm <- predict(nm_classifier, newdata = X_test)$class
# accuracy
paste0('accuracy: ', mean(y_test == predictions_nm))

nm_class_table <- table(y_test, predictions_nm)
confusion_matrix_nm <- confusionMatrix(nm_class_table, mode = "everything")




####### Model performance visualizations -------->

# svm accuracy and kappa
confusion_matrix_svm$overall[1:4] %>% round(2)

# svm all performance metrics (for Rmd)
knitr::kable(confusion_matrix_svm$byClass %>% round(2))

# svm confusion matrix visual
qplot(y_test, predictions_svm, colour=y_test, size=I(0.01), geom = c("boxplot", "jitter"),
    main = "SVM Predicted Classes vs Observed", xlab = "Observed", ylab = "Predicted") + 
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.text.x = element_text(face = "bold", size=10, angle = 30),
          axis.text.y = element_text(face = "bold", size=10, angle=30),
          plot.title = element_text(size=15), legend.position="none")

# svm chord diagram
cols = c("red", "blue", "purple", "pink", "orange", "green", "steelblue1",
         "green4", "cyan", "royalblue", "magenta", "yellow", "steelblue4", "blue4", "olivedrab")
chordDiagram(svm_class_table, grid.col = cols)




# naive bayes chord diagram
cols = c("red", "blue", "purple", "pink", "orange", "green", "steelblue1",
         "green4", "cyan", "royalblue", "magenta", "yellow", "steelblue4", "blue4", "olivedrab")
chordDiagram(nb_class_table, grid.col = cols)


# naive bayes accuracy and kappa
confusion_matrix_nb$overall[1:4] %>% round(2)

# naive bayes all performance metrics (for Rmd)
knitr::kable(confusion_matrix_nb$byClass %>% round(2))

# naive bayes confusion matrix visual
qplot(test_y_nb, predictions_nb, colour=test_y,  size=I(0.01), geom = c("boxplot", "jitter"), 
    main = "Naive Bayes Predicted Classes vs Observed", 
    xlab = "Observed", ylab = "Predicted") + 
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.text.x = element_text(face = "bold", size=10, angle = 30),
          axis.text.y = element_text(face = "bold", size=10, angle=30),
          plot.title = element_text(size=15), legend.position="none")


## accuracies and kappas

nb_scores <- tibble(Accuracy = confusion_matrix_nb$overall[1],
                    Kappa = confusion_matrix_nb$overall[2],
                    Model = "Naive Bayes")
nm_scores <- tibble(Accuracy = confusion_matrix_nm$overall[1],
                    Kappa = confusion_matrix_nm$overall[2],
                    Model = "Nearest Mean")
svm_scores <- tibble(Accuracy = confusion_matrix_svm$overall[1],
                    Kappa = confusion_matrix_svm$overall[2],
                    Model = "SVM")
all_scores <- bind_rows(nb_scores, nm_scores, svm_scores)

ggplot(melt(all_scores), aes(fill=Model, y=value, x=variable)) + 
    geom_bar(position="dodge", stat="identity", width = 0.4) +
    scale_y_continuous(breaks = seq(0, 1, len = 30) %>% round(2)) +
    labs(x = "", y = "", title = "Accuray and Kappa Scores for 3 Classifiers") +
    theme_bw() +
    theme(axis.text=element_text(size=12),
          axis.text.x = element_text(face = "bold", size=15),
          axis.text.y = element_text(face = "bold", size=10),
          plot.title = element_text(size=15))

