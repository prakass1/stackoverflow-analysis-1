# 01_import.R
# import files into the workspace

posts_names <- c("Id",
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
                 "ParentId",
                 "AcceptedAnswerId",
                 "LastEditorUserId",
                 "LastEditDate",
                 "ClosedDate",
                 "FavoriteCount",
                 "OwnerDisplayName",
                 "LastEditorDisplayName",
                 "CommunityOwnedDate"
                 )

comments_names <- c("Id",
                    "PostId",
                    "Score",
                    "Text",
                    "CreationDate",
                    "UserId",
                    "UserDisplayName"
                    )

tags_names <- c("Id",
                "TagName",
                "Count",
                "ExcerptPostId",
                "WikiPostId"
                )

users_names <- c("Id",
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
                 "ProfileImageUrl",
                 "AccountId"
)


start_time <- Sys.time()


# import data from csvs
posts <- read_csv(paste0(csv_loc, "posts.csv"),
                  col_names = TRUE
                  )


comments <- read_csv(paste0(csv_loc, "comments.csv"),
                     col_names = TRUE
                    )


tags <- read_csv(paste0(csv_loc, "tags.csv"),
                 col_names = TRUE
                 )


users <- read_csv(paste0(csv_loc, "users.csv"),
                  col_names = TRUE
                  )


end_time <- Sys.time()
time_taken <- difftime(end_time, start_time, units='mins')
print(paste0("Time Taken for reading dataframes: ", time_taken))
