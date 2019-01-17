server <- function(input, output, session) {
  wordcloud_rep <- repeatable(wordcloud)
  d <- reactive({
    term <- as.integer(input$max1)
  })
  output$plot <- renderPlot({
    if(input$selection == TRUE){
      tag_words %>% 
        with(wordcloud_rep(word, n, random.order = FALSE, max.words = input$max,
                           colors=brewer.pal(8, "Dark2")))
    }
    else if(input$selection1 == TRUE){
      
      tag_words %>%
        arrange(desc(tf_idf)) %>%
        mutate(word = factor(word, levels = rev(unique(word)))) %>% 
        group_by(Tags) %>% 
        top_n(d()) %>% 
        ungroup %>%
        ggplot(aes(word, tf_idf, fill = Tags)) +
        geom_col(show.legend = FALSE) +
        labs(x = NULL, y = paste0("Top tf-idf ",d())) +
        facet_wrap(~Tags, ncol = 5, scales = "free") +
        coord_flip()
      
    }
    else{
      
    }
  })
  
  output$pred <- renderText({
    
    if(input$selection11 == TRUE){
      if(input$button == 0){
        return()
      }
      
      isolate({
        print(input$text11)
        if((input$text11 != "") & (wordcount(input$text11,sep=" ") > 2)){
          pData = data.frame("Title"=c(input$text11),"Tags"=c("doc1"),stringsAsFactors = FALSE)
          predee = perfComputationForLDA(pData)
          test.topics <- posterior(LDA_Tags,predee)
          test.topics <- apply(test.topics$topics, 1, which.max)
          
          pred<- source_topic_relationship %>% 
            filter(paste0("Topic ",test.topics) %in% topic) %>%
            group_by(topic) %>%
            slice(which.max(avg))
          
          
          
       
          print(paste0("The prediction from LDA is ",   pred[[1]]))
        }
        else{
          return("Please provide a valid StackOverFlow Title")
        }
          
        })
    }
    else  if(input$selection12 == TRUE){
      if(input$button1 == 0){
        return()
      }
      
      isolate({
        print(input$text12)
        if((input$text12 != "") & (wordcount(input$text12,sep=" ") > 2)){
          #Load the classifier and predict for the text provided
          # create a new dfm from the training dfm
          new_dfm <- dfm_select(dfm(quanteda::tokens(input$text12)), pattern = training_dfm, 
                                selection = "keep")
          # predict
          mnb_prediction <- predict(mnb_classifier, newdata = new_dfm)
          
          
          print(paste0("The prediction from naive bayes is ", mnb_prediction))
        }
        else{
          return("Please provide a valid StackOverFlow Title")
        }
        
      })
    }
    
  
  })
  
}
