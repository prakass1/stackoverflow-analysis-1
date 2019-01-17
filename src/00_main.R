# 00_main.R
# A wrapper to run all R scripts

library("tryCatchLog")

# source configurations first
source("util.R")
source("SO_config.R")

xmls <- c()
ne_csvs <- c()

# check if csvs exist
count <-0


for(val in files){
    print(paste0(csv_loc, val, ext))
    if(file.exists(paste0(csv_loc, val, ext))){
        count=count+1
        # xmls <- append(xmls,val)
    }
    else{
        ne_csvs <- append(ne_csvs, val)
    }
}

######## csv stuff ##################
if(count!=4){
    # call to csvs
    start_time <- Sys.time()
    for(val in ne_csvs){
        # Assign each parsed vals to dataframe
        print("begin")
        # Attempt to use logging functionality: 
        # https://cran.r-project.org/web/packages/tryCatchLog/vignettes/tryCatchLog-intro.html ##
        tryLog(assign(paste0(val, "_df"), 
                      ReadXMLToDf(paste0(xml_loc, val, ".xml"), 
                                  paste0("//", val, "/row"))))
        print("end")
    }
}

end_time <- Sys.time()
time_taken <- difftime(end_time, start_time, units='mins')
print(paste0("Time Taken for creating dataframe is ", time_taken))


# export as csv
print(paste0("Begin writing to csv ....."))
for(val in ne_csvs){
    if(exists(paste0(val,"_df"))){
        dfs <- eval(parse(text=paste0(val, "_df")))
        WriteToCsv(dfs, paste0(csv_loc, val, ext))
    }
}
print(paste0("Writing to csv completed ....."))
# About 5min and 25 seconds


# save some memory
if(exists('dfs')){
    remove(dfs)
}

if(exists('posts_df')){
    remove(posts_df)
}

if(exists('comments_df')){
    remove(comments_df)
}

if(exists('tags_df')){
    remove(tags_df)
}

if(exists('users_df')){
    remove(users_df)
}


# import csv to dataframe
source('01_import.R')

# perform preprocessing to prepare data
source('02_preprocessing.R')

# Predictions
source("03_predictions.R")



