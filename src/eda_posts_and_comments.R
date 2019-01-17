# eda_posts_and_comments


score_questions <- questions %>%
    arrange(desc(Score)) %>%
    select(Score, HasCode, HasLinks, AnswerCount, CommentCount, FavoriteCount)

scoresCol <- cor(score_questions)

corrplot(scoresCol, method="color",  
         type="lower", order="hclust", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col="black", tl.srt=0, #Text label color and rotation
         # hide correlation coefficient on the principal diagonal
         diag=FALSE )




